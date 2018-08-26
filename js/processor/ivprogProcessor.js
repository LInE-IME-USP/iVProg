import { Store } from './store/store';
import { StoreObject } from './store/storeObject';
import { StoreObjectArray } from './store/storeObjectArray';
import { Modes } from './mode';
import { Context } from './context';
import { Types } from './../ast/types';
import * as Commands from './../ast/commands/';
import * as Expressions from './../ast/expressions/';

export class IVProgProcessor {

  constructor(ast) {
    this.ast = ast;
    this.globalStore = new Store();
    this.stores = [this.globalStore];
    this.context = [Context.BASE];
    this.input = null;
    this.output = null;
  }

  registerInput (input) {
    this.input = input;
  }

  registerOutput (output) {
    this.output = output;
  }

  checkContext(context) {
    return this.context[this.context.length] === context;
  }

  ignoreSwitchCases (store) {
    if (store.mode === Modes.RETURN) {
      return true;
    } else if (store.mode === Modes.BREAK) {
      return true;
    } else {
      return false;
    }
  }

  interpretAST () {
    this.initGlobal();
    const mainFunc = this.findMainFunction();
    if(mainFunc === null) {
      // TODO: Better error message
      throw new Error("Missing main funciton.");
    }
    return this.runFunction(mainFunc, [], this.globalStore);
  }

  initGlobal () {
    if(!this.checkContext(Context.BASE)) {
      throw new Error("!!!CRITICAL: Invalid call to initGlobal outside BASE context!!!");
    }
    this.ast.global.forEach(decl => {
      this.executeCommand(this.globalStore, decl).then(v => this.globalStore = v);
    });
  }

  findMainFunction () {
    return this.ast.functions.find(v => v.isMain);
  }

  findFunction (name) {
    const val = this.ast.functions.find( v => v.name === name);
    if (!!!val) {
      // TODO: better error message;
      throw new Error(`Function ${name} is not defined.`);
    }
    return val;
  }

  runFunction (func, actualParameters, store) {
    let funcStore = new Store();
    funcStore.extendStore(this.globalStore);
    const returnStoreObject = new StoreObject(func.returnType, null);
    const funcName = func.isMain ? 'main' : func.name;
    const funcNameStoreObject = new StoreObject(Types.STRING, funcName, true);
    funcStore.insertStore('$', returnStoreObject);
    funcStore.insertStore('$name', funcNameStoreObject);
    funcStore = this.associateParameters(func.formalParameters, actualParameters, store, funcStore);
    this.context.push(Context.FUNCTION);
    this.stores.push(funcStore);
    const result = this.executeCommands(funcStore, func.commands);
    this.stores.pop();
    this.context.pop();
    return result;
  }

  associateParameters (formalList, actualList, callerStore, calleeStore) {
    if (formalList.length != actualList.length) {
      // TODO: Better error message
      throw new Error("Numbers of parameters doesn't match");
    }
    formalList.forEach((v, i) => {
      const val = this.evaluateExpression(callerStore, actualList[i]);
      switch (v.dimensions) {
        case 1: {
          if (val.lines > 0 && val.columns === null) {
            calleeStore.insertStore(v.id, val);
          } else {
            // TODO: Better error message
            throw new Error(`Parameter ${v.id} is not compatible with the value given.`);
          }
          break;
        }
        case 2: {
          if (val.lines > 0 && val.columns > 0) {
            calleeStore.insertStore(v.id, val);
          } else {
            // TODO: Better error message
            throw new Error(`Parameter ${v.id} is not compatible with the value given.`);
          }
          break;
        }
        case 0: {
          if (val.type !== v.type) {
            // TODO: Better error message
            throw new Error(`Parameter ${v.id} is not compatible with ${val.type}.`);
          } else {
            calleeStore.insertStore(v.id, val);
          }
        }
      }
    });
    return calleeStore;
  }

  executeCommands (store, cmds) {
    return cmds.reduce((promise, cmd) => promise.then( sto => {
      while (sto.mode === Modes.PAUSE) {
        continue;
      }
      if(sto.mode === Modes.RETURN) {
        return Promise.resolve(sto);
      } else if (this.checkContext(Context.BREAKABLE && 
        sto.mode === Modes.BREAK)) {
          return Promise.resolve(sto);
      }
      return this.executeCommand(sto, cmd);
    }), Promise.resolve(store));
  }

  executeCommand (store, cmd) {

    while (store.mode === Modes.PAUSE) {
      continue;
    }

    if(store.mode === Modes.RETURN) {
      return Promise.resolve(store);
    } else if(this.checkContext(Context.BREAKABLE) && store.mode === Modes.BREAK) {
      return Promise.resolve(store);
    }

    if (cmd instanceof Commands.Declaration) {
      return this.executeDeclaration(store, cmd);
    } else if (cmd instanceof Commands.Assign) {
      return this.executeAssign(store, cmd);
    } else if (cmd instanceof Commands.Break) {
      return this.executeBreak(store, cmd);
    } else if (cmd instanceof Commands.Return) {
      return this.executeReturn(store, cmd);
    } else if (cmd instanceof Commands.IfThenElse) {
      return this.executeIfThenElse(store, cmd);
    } else if (cmd instanceof Commands.While) {
      return this.executeWhile(store, cmd);
    } else if (cmd instanceof Commands.DoWhile) {
      return this.executeDoWhile(store, cmd);
    } else if (cmd instanceof Commands.For) {
      return this.executeFor(store, cmd);
    } else if (cmd instanceof Commands.Switch) {
      return this.executeSwitch(store, cmd);
    } else if (cmd instanceof Commands.FunctionCall) {
      return this.executeFunctionCall(store, cmd);
    } else {
      throw new Error("!!!CRITICAL A unknown command was found!!!\n" + cmd);
    }
  }

  executeFunctionCall (store, cmd) {
    const func = this.findFunction(cmd.id);
    this.runFunction(func, cmd.actualParameters, store);
    return Promise.resolve(store);
  }

  executeSwitch (store, cmd) {
    const auxCaseFun = (promise, switchExp, aCase) => {
      return promise.then( result => {
        const sto = result.sto;
        if (this.ignoreSwitchCases(sto)) {
          return Promise.resolve(result);
        } else if (result.wasTrue || aCase.isDefault) {
          const newSto = this.executeCommand(result.sto,aCase.commands);
          return Promise.resolve({wasTrue: true, sto: newSto});
        } else {
          const value = this.evaluateExpression(sto,
            new Expressions.InfixApp('==', switchExp, aCase.expression));
          if (value.value) {
            const newSto = this.executeCommand(result.sto,aCase.commands);
            return Promise.resolve({wasTrue: true, sto: newSto});
          } else {
            return Promise.resolve({wasTrue: false, sto: newSto});
          }
        }
      });
    }

    try {
      this.context.push(Context.BREAKABLE);
      let breakLoop = false;
      const case0 = cmd.cases[0];
      let result = auxCaseFun(Promise.resolve({wasTrue: false, sto: store}),
          cmd.expression,
          case0);
      for (let index = 1; index < cmd.cases.length && !breakLoop; index++) {
        const aCase = cmd.cases[index];
        result = auxCaseFun(result, cmd.expression, aCase);
        result.then( r => breakLoop = this.ignoreSwitchCases(r.sto));
      }
      this.context.pop();
      return result.then(r => r.sto);
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeFor (store, cmd) {
    try {
      //BEGIN for -> while rewrite
      const initCmd = cmd.assignment;
      const condition = cmd.condition;
      const increment = cmd.increment;
      const whileBlock = new Commands.CommandBlock([],
        cmd.commands.concat(increment));
      const forAsWhile = new Commands.While(condition, whileBlock);
      //END for -> while rewrite
      const newCmdList = [initCmd,forAsWhile];
      return Promise.resolve(this.executeCommands(store, newCmdList));
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeDoWhile (store, cmd) {
    try {
      this.context.push(Context.BREAKABLE);
      const newStore = this.executeCommands(store, cmd.commands);
      const value = this.evaluateExpression(newStore, cmd.expression);
      if (value.type !== Types.BOOLEAN) {
        // TODO: Better error message -- Inform line and column from token!!!!
        // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
        return Promise.reject(new Error(`DoWhile expression must be of type boolean`));
      }
      if (value.value) {
        return Promise.resolve(this.executeCommand(newStore, cmd));
      } else {
        return Promise.resolve(newStore);
      }
    } catch (error) {
      return Promise.reject(error)
    }
  }

  executeWhile (store, cmd) {
    try {
      this.context.push(Context.BREAKABLE);
      const value = this.evaluateExpression(store, cmd.expression);
      if(value.type === Types.BOOLEAN) {
        if(value.value) {
          const newStore = this.executeCommands(store, cmd.commands);
          this.context.pop();
          return Promise.resolve(this.executeCommand(newStore, cmd));
        } else {
          this.context.pop();
          return Promise.resolve(store);
        }
      } else {
        // TODO: Better error message -- Inform line and column from token!!!!
        // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
        return Promise.reject(new Error(`Loop condition must be of type boolean`));
      }
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeIfThenElse (store, cmd) {
    try {
      const value = this.evaluateExpression(cmd.condition);
      if(value.type === Types.BOOLEAN) {
        if(value.value) {
          return Promise.resolve(this.executeCommand(store, cmd.ifTrue));
        } else {
          return Promise.resolve(this.executeCommand(store, cmd.ifFalse));
        }
      } else {
        // TODO: Better error message -- Inform line and column from token!!!!
        // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
        return Promise.reject(new Error(`If expression must be of type boolean`));
      }
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeReturn (store, cmd) {
    try {
      const funcType = store.applyStore('$');
      const value = this.evaluateExpression(store, cmd.expression);
      const funcName = store.applyStore('$name');
      if (funcType.type !== value.type) {
        // TODO: Better error message -- Inform line and column from token!!!!
        // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
        return Promise.reject(new Error(`Function ${funcName.value} must return ${funcType.type} instead of ${value.type}.`));
      } else {
        store.updateStore('$', value);
        store.mode = Modes.RETURN;
        return Promise.resolve(store);
      }
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeBreak (store, cmd) {
    if(this.checkContext(Context.BREAKABLE)) {
      store.mode = Modes.BREAK;
      return Promise.resolve(store);
    } else {
      return Promise.reject(new Error("!!!CRITIAL: Break command outside Loop/Switch scope!!!"));
    }
  }

  executeAssign (store, cmd) {
    try {
      const value = this.evaluateExpression(store, cmd.expression);
      store.updateStore(cmd.id, value);
      return Promise.resolve(store);
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeDeclaration (store, cmd) {
    try {
      const value = this.evaluateExpression(store, cmd.initial);
      if(cmd instanceof Commands.ArrayDeclaration) {
        const temp = new StoreObjectArray(decl.subtype, decl.lines, decl.columns, null, decl.isConst);
        store.insertStore(decl.id, temp);
        store.updateStore(decl.id, value);
      } else {
        const temp = new StoreObject(decl.type, null, decl.isConst);
        store.insertStore(decl.id, temp);
        store.updateStore(decl.id, value);   
      }
      return Promise.resolve(store);
    } catch (e) {
      return Promise.reject(e);
    }
  }

}
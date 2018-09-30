import { Store } from './store/store';
import { StoreObject } from './store/storeObject';
import { StoreObjectArray } from './store/storeObjectArray';
import { StoreObjectRef } from './store/storeObjectRef';
import { Modes } from './modes';
import { Context } from './context';
import { Types } from './../ast/types';
import { Operators } from './../ast/operators';
import { LanguageDefinedFunction } from './definedFunctions';
import { resultTypeAfterInfixOp, resultTypeAfterUnaryOp } from './compatibilityTable';
import * as Commands from './../ast/commands/';
import * as Expressions from './../ast/expressions/';
import { StoreObjectArrayAddress } from './store/storeObjectArrayAddress';
import { StoreObjectArrayAddressRef } from './store/storeObjectArrayAddressRef';

export class IVProgProcessor {

  constructor (ast) {
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
    return this.context[this.context.length - 1] === context;
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
      this.executeCommand(this.globalStore, decl).then(sto => this.globalStore = sto);
    });
  }

  findMainFunction () {
    return this.ast.functions.find(v => v.isMain);
  }

  findFunction (name) {
    if(name.match(/^\$.+$/)) {
      const fun = LanguageDefinedFunction.getFunction(name);
      if(!!!fun) {
        throw new Error("!!!Internal Error. Language defined function not implemented -> " + name + "!!!");
      }
      return fun;
    } else {
      const val = this.ast.functions.find( v => v.name === name);
      if (!!!val) {
        // TODO: better error message;
        throw new Error(`Function ${name} is not defined.`);
      }
      return val;
    }
  }

  runFunction (func, actualParameters, store) {
    let funcStore = new Store();
    funcStore.extendStore(this.globalStore);
    const returnStoreObject = new StoreObject(func.returnType, null);
    const funcName = func.isMain ? 'main' : func.name;
    const funcNameStoreObject = new StoreObject(Types.STRING, funcName, true);
    funcStore.insertStore('$', returnStoreObject);
    funcStore.insertStore('$name', funcNameStoreObject);
    const newFuncStore$ = this.associateParameters(func.formalParameters, actualParameters, store, funcStore);
    return newFuncStore$.then(sto => {
      this.context.push(Context.FUNCTION);
      this.stores.push(sto);
      return this.executeCommands(sto, func.variablesDeclarations)
        .then(stoWithVars => this.executeCommands(stoWithVars, func.commands)).then(finalSto => {
          this.stores.pop();
          this.context.pop();
          return finalSto;
        });
    });
  }

  associateParameters (formalList, actualList, callerStore, calleeStore) {
    if (formalList.length != actualList.length) {
      // TODO: Better error message
      throw new Error("Numbers of parameters doesn't match");
    }
    const promises$ = actualList.map(actualParameter => this.evaluateExpression(callerStore, actualParameter));
    return Promise.all(promises$).then(values => {
      for (let i = 0; i < values.length; i++) {
        const stoObj = values[i];
        const formalParameter = formalList[i];
        switch (formalParameter.dimensions) {
          case 1: {
            if (stoObj.lines > 0 && stoObj.columns === null
              && stoObj.subtype === formalParameter.type) {

              if(formalParameter.byRef && !stoObj.inStore) {
                throw new Error('You must inform a variable as parameter');
              }

              if(formalParameter.byRef) {
                const ref = new StoreObjectArrayAddressRef(stoObj);
                calleeStore.insertStore(formalParameter.id, ref);
              } else {
                calleeStore.insertStore(formalParameter.id, stoObj);
              }

            } else {
              // TODO: Better error message
              throw new Error(`Parameter ${formalParameter.id} is not compatible with the value given.`);
            }
            break;
          }
          case 2: {
            if (stoObj.lines > 0 && stoObj.columns > 0
              && stoObj.subtype === formalParameter.type) {

              if(formalParameter.byRef && !stoObj.inStore) {
                throw new Error('You must inform a variable as parameter');
              }

              if(formalParameter.byRef) {
                const ref = new StoreObjectArrayAddressRef(stoObj);
                calleeStore.insertStore(formalParameter.id, ref);
              } else {
                calleeStore.insertStore(formalParameter.id, stoObj);
              }

            } else {
              // TODO: Better error message
              throw new Error(`Parameter ${formalParameter.id} is not compatible with the value given.`);
            }
            break;
          }
          case 0: {
            if(formalParameter.byRef && !stoObj.inStore) {

              throw new Error('You must inform a variable as parameter');
            } else if (formalParameter.type !== Types.ALL && stoObj.type !== formalParameter.type) {

              // TODO: Better error message
              throw new Error(`Parameter ${formalParameter.id} is not compatible with ${stoObj.type}.`);
            } else {

              if(formalParameter.byRef) {
                let ref = null;
                if (stoObj instanceof StoreObjectArrayAddress) {
                  ref = new StoreObjectArrayAddressRef(stoObj);
                } else {
                  ref = new StoreObjectRef(stoObj.id, callerStore);
                }
                calleeStore.insertStore(formalParameter.id, ref);
              } else {
                calleeStore.insertStore(formalParameter.id, stoObj);
              }

            }
          }
        }
      }
      return calleeStore;
    });
  }

  executeCommands (store, cmds) {
    // helper to partially apply a function, in this case executeCommand
    const partial = (fun, cmd) => (sto) => fun(sto, cmd);
    return cmds.reduce((lastCommand, next) => {
      const nextCommand = partial(this.executeCommand.bind(this), next);
      return lastCommand.then(nextCommand);
    }, Promise.resolve(store));
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
    } else if (cmd instanceof Commands.ArrayIndexAssign) {
      return this.executeArrayIndexAssign(store, cmd);
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
    } else if (cmd instanceof Commands.SysCall) {
      return this.executeSysCall(store, cmd);
    } else {
      throw new Error("!!!CRITICAL A unknown command was found!!!\n" + cmd);
    }
  }

  executeSysCall (store, cmd) {
    const func = cmd.langFunc.bind(this);
    return func(store, cmd);
  }

  executeFunctionCall (store, cmd) {
    return new Promise((resolve, reject) => {
      const func = this.findFunction(cmd.id);
      this.runFunction(func, cmd.actualParameters, store)
        .then(_ => resolve(store))
        .catch(err => reject(err));
    }); 
  }

  executeSwitch (store, cmd) {
    this.context.push(Context.BREAKABLE);
    const auxCaseFun = (promise, switchExp, aCase) => {
      return promise.then( result => {
        const sto = result.sto;
        if (this.ignoreSwitchCases(sto)) {
          return Promise.resolve(result);
        } else if (result.wasTrue || aCase.isDefault) {
          const $newSto = this.executeCommands(result.sto,aCase.commands);
          return $newSto.then(nSto => {
            return Promise.resolve({wasTrue: true, sto: nSto});
          });
        } else {
          const $value = this.evaluateExpression(sto,
            new Expressions.InfixApp(Operators.EQ, switchExp, aCase.expression));
          return $value.then(vl => {
            if (vl.value) {
              const $newSto = this.executeCommands(result.sto,aCase.commands);
              return $newSto.then(nSto => {
                return Promise.resolve({wasTrue: true, sto: nSto});
              });
            } else {
              return Promise.resolve({wasTrue: false, sto: sto});
            }
          });
        }
      });
    }

    try {
      let breakLoop = false;
      let $result = Promise.resolve({wasTrue: false, sto: store});
      for (let index = 0; index < cmd.cases.length && !breakLoop; index++) {
        const aCase = cmd.cases[index];
        $result = auxCaseFun($result, cmd.expression, aCase);
        $result.then( r => breakLoop = this.ignoreSwitchCases(r.sto));
      }
      return $result.then(r => {
        this.context.pop();
        if(r.sto.mode === Modes.BREAK) {
          r.sto.mode = Modes.RUN;
        }
        return r.sto;
      });
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
      return this.executeCommands(store, newCmdList);
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeDoWhile (store, cmd) {
    try {
      this.context.push(Context.BREAKABLE);
      const $newStore = this.executeCommands(store, cmd.commands);
      return $newStore.then(sto => {
        if(sto.mode === Modes.BREAK) {
          this.context.pop();
          sto.mode = Modes.RUN;
          return Promise.resolve(sto);
        }
        const $value = this.evaluateExpression(sto, cmd.expression);
        return $value.then(vl => {
          if (vl.type !== Types.BOOLEAN) {
            // TODO: Better error message -- Inform line and column from token!!!!
            // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
            return Promise.reject(new Error(`DoWhile expression must be of type boolean`));
          }
          if (vl.value) {
            this.context.pop();
            return this.executeCommand(sto, cmd);
          } else {
            this.context.pop();
            return Promise.resolve(sto);
          }
        });
      });
    } catch (error) {
      return Promise.reject(error)
    }
  }

  executeWhile (store, cmd) {
    try {
      this.context.push(Context.BREAKABLE);
      const $value = this.evaluateExpression(store, cmd.expression);
      return $value.then(vl => {
        if(vl.type === Types.BOOLEAN) {
          if(vl.value) {
            const $newStore = this.executeCommands(store, cmd.commands);
            return $newStore.then(sto => {
              this.context.pop();
              if (sto.mode === Modes.BREAK) {
                sto.mode = Modes.RUN;
                return Promise.resolve(sto);
              }
              return this.executeCommand(sto, cmd);
            });
          } else {
            this.context.pop();
            return Promise.resolve(store);
          }
        } else {
          // TODO: Better error message -- Inform line and column from token!!!!
          // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
          return Promise.reject(new Error(`Loop condition must be of type boolean`));
        }
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeIfThenElse (store, cmd) {
    try {
      const $value = this.evaluateExpression(store, cmd.condition);
      return $value.then(vl => {
        if(vl.type === Types.BOOLEAN) {
          if(vl.value) {
            return this.executeCommands(store, cmd.ifTrue.commands);
          } else if( cmd.ifFalse !== null){
            if(cmd.ifFalse instanceof Commands.IfThenElse) {
              return this.executeCommand(store, cmd.ifFalse);
            } else {
              return this.executeCommands(store, cmd.ifFalse.commands);
            }
          } else {
            return Promise.resolve(store);
          }
        } else {
          // TODO: Better error message -- Inform line and column from token!!!!
          // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
          return Promise.reject(new Error(`If expression must be of type boolean`));
        }
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeReturn (store, cmd) {
    try {
      const funcType = store.applyStore('$');
      const $value = this.evaluateExpression(store, cmd.expression);
      const funcName = store.applyStore('$name');
      return $value.then(vl => {

        if(vl === null && funcType === Types.VOID) {
          return Promise.resolve(store);
        }

        if (vl === null || funcType.type !== vl.type) {
          // TODO: Better error message -- Inform line and column from token!!!!
          // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
          return Promise.reject(new Error(`Function ${funcName.value} must return ${funcType.type} instead of ${vl.type}.`));
        } else {
          store.updateStore('$', vl);
          store.mode = Modes.RETURN;
          return Promise.resolve(store);
        }
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeBreak (store, _) {
    if(this.checkContext(Context.BREAKABLE)) {
      store.mode = Modes.BREAK;
      return Promise.resolve(store);
    } else {
      return Promise.reject(new Error("!!!CRITIAL: Break command outside Loop/Switch scope!!!"));
    }
  }

  executeAssign (store, cmd) {
    try {
      const $value = this.evaluateExpression(store, cmd.expression);
      return $value.then( vl => {
        store.updateStore(cmd.id, vl) 
        return store;
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeArrayIndexAssign (store, cmd) {
    return new Promise((resolve, reject) => {
      const mustBeArray = store.applyStore(cmd.id);
      if(mustBeArray.type !== Types.ARRAY) {
        reject(new Error(cmd.id + " is not a vector/matrix"));
        return;
      }
      const line$ = this.evaluateExpression(store, cmd.line);
      const column$ = this.evaluateExpression(store, cmd.column);
      const value$ =  this.evaluateExpression(store, cmd.expression);
      Promise.all([line$, column$, value$]).then(results => {
        const lineSO = results[0];
        if(lineSO.type !== Types.INTEGER) {
          // TODO: better error message
          //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
          reject(new Error("Array dimension must be of type int"));
          return;
        }
        const line = lineSO.number;
        const columnSO = results[1];
        let column = null
        if (columnSO !== null) {
          if(columnSO.type !== Types.INTEGER) {
            // TODO: better error message
            //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
            reject(new Error("Array dimension must be of type int"));
            return;
          }
          column = columnSO.number;
        }
        const value = results[2];
        if (line >= mustBeArray.lines) {
          // TODO: better error message
          return Promise.reject(new Error(`${exp.id}: index out of bounds: ${lines}`));
        }
        if (column !== null && mustBeArray.columns === null ){
          // TODO: better error message
          return Promise.reject(new Error(`${exp.id}: index out of bounds: ${column}`));
        }
        if(column !== null && column >= mustBeArray.columns) {
          // TODO: better error message
          return Promise.reject(new Error(`${exp.id}: index out of bounds: ${column}`));
        }

        const newArray = Object.assign(new StoreObjectArray(null,null,null), mustBeArray);
        if (column !== null) {
         if (value.type === Types.ARRAY) {
           reject(new Error("Invalid operation. This must be a value: line "+cmd.sourceInfo.line));
           return;
         }
         newArray.value[line].value[column] = value;
         store.updateStore(cmd.id, newArray);
        } else {
         if(mustBeArray.columns !== null && value.type !== Types.ARRAY) {
          reject(new Error("Invalid operation. This must be a vector: line "+cmd.sourceInfo.line));
          return;
         }
         newArray.value[line] = value;
         store.updateStore(cmd.id, newArray);
        }
        resolve(store);
      }).catch(err => reject(err));
    });
  }

  executeDeclaration (store, cmd) {
    try {
      const $value = this.evaluateExpression(store, cmd.initial);
      if(cmd instanceof Commands.ArrayDeclaration) {
        const $lines = this.evaluateExpression(store, cmd.lines);
        const $columns = cmd.columns === null ? null: this.evaluateExpression(store, cmd.columns);
        return Promise.all([$lines, $columns, $value]).then(values => {
          const lineSO = values[0];
          if(lineSO.type !== Types.INTEGER) {
            // TODO: better error message
            //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
            return Promise.reject(new Error("Array dimension must be of type int"));
          }
          const line = lineSO.number;
          const columnSO = values[1];
          let column = null
          if (columnSO !== null) {
            if(columnSO.type !== Types.INTEGER) {
              // TODO: better error message
              //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
              return Promise.reject(new Error("Array dimension must be of type int"));
            }
            column = columnSO.number;
          }
          const value = values[2];
          const temp = new StoreObjectArray(cmd.subtype, line, column, null, cmd.isConst);
          store.insertStore(cmd.id, temp);
          if(value !== null) {
            store.updateStore(cmd.id, value);
          }
          return store;
        });
        
      } else {
        const temp = new StoreObject(cmd.type, null, cmd.isConst);
        store.insertStore(cmd.id, temp);
        return $value.then(vl => {
          if (vl !== null)
            store.updateStore(cmd.id, vl)
          return store;
        });
      }
    } catch (e) {
      return Promise.reject(e);
    }
  }

   evaluateExpression (store, exp) {

    if (exp instanceof Expressions.UnaryApp) {
      return this.evaluateUnaryApp(store, exp);
    } else if (exp instanceof Expressions.InfixApp) {
      return this.evaluateInfixApp(store, exp);
    } else if (exp instanceof Expressions.ArrayAccess) {
      return this.evaluateArrayAccess(store, exp);
    } else if (exp instanceof Expressions.VariableLiteral) {
      return this.evaluateVariableLiteral(store, exp);
    } else if (exp instanceof Expressions.IntLiteral) {
      return this.evaluateLiteral(store, exp);
    } else if (exp instanceof Expressions.RealLiteral) {
      return this.evaluateLiteral(store, exp);
    } else if (exp instanceof Expressions.BoolLiteral) {
      return this.evaluateLiteral(store, exp);
    } else if (exp instanceof Expressions.StringLiteral) {
      return this.evaluateLiteral(store, exp);
    } else if (exp instanceof Expressions.ArrayLiteral) {
      return this.evaluateArrayLiteral(store, exp);
    } else if (exp instanceof Expressions.FunctionCall) {
      return this.evaluateFunctionCall(store, exp);
    }
    console.log('null exp');
    return Promise.resolve(null);
  }

  evaluateFunctionCall (store, exp) {
    const func = this.findFunction(exp.id);
    if(func.returnType === Types.VOID) {
      // TODO: better error message
      return Promise.reject(new Error(`Function ${exp.id} cannot be used inside an expression`));
    }
    const $newStore = this.runFunction(func, exp.actualParameters, store);
    return $newStore.then( sto => {
      const val = sto.applyStore('$');
      if (val.type === Types.ARRAY) {
        return Promise.resolve(Object.assign(new StoreObjectArray(null,null,null,null,null), val));
      } else {
        return Promise.resolve(Object.assign(new StoreObject(null,null), val));
      }
    });
  }

  evaluateArrayLiteral (store, exp) {
    if(!exp.isVector) {
      const $matrix = this.evaluateMatrix(store, exp.value);
      return $matrix.then(list => {
        const arr = new StoreObjectArray(list[0].subtype, list.length, list[0].lines, list);
        if(arr.isValid)
          return Promise.resolve(arr);
        else
          return Promise.reject(new Error(`Invalid array`))
      });
    } else {
      return this.evaluateVector(store, exp.value).then(list => {
        const stoArray = new StoreObjectArray(list[0].type, list.length, null, list);
        if(stoArray.isValid)
          return Promise.resolve(stoArray);
        else
          return Promise.reject(new Error(`Invalid array`))
      });
    }
  }

  evaluateVector (store, exps) {
    return Promise.all(exps.map( exp => this.evaluateExpression(store, exp)));
  }

  evaluateMatrix (store, exps) {
    return Promise.all(exps.map( vector => {
      const $vector = this.evaluateVector(store, vector.value)
      return $vector.then(list => new StoreObjectArray(list[0].type, list.length, null, list))
    } ));
  }

  evaluateLiteral (_, exp) {
    return Promise.resolve(new StoreObject(exp.type, exp.value));
  }

  evaluateVariableLiteral (store, exp) {
    try {
      const val = store.applyStore(exp.id);
      if (val.type === Types.ARRAY) {
        return Promise.resolve(Object.assign(new StoreObjectArray(null,null,null,null), val));
      } else {
        return Promise.resolve(Object.assign(new StoreObject(null,null), val));
      }
    } catch (error) {
      return Promise.reject(error);
    }
  }

  evaluateArrayAccess (store, exp) {
    const mustBeArray = store.applyStore(exp.id);
    if (mustBeArray.type !== Types.ARRAY) {
      // TODO: better error message
      return Promise.reject(new Error(`${exp.id} is not of type array`));
    }
    const $line = this.evaluateExpression(store, exp.line);
    const $column = this.evaluateExpression(store, exp.column);
    return Promise.all([$line, $column]).then(values => {
      const lineSO = values[0];
      const columnSO = values[1];
      if(lineSO.type !== Types.INTEGER) {
        // TODO: better error message
        //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
        return Promise.reject(new Error("Array dimension must be of type int"));
      }
      const line = lineSO.number;
      let column = null;
      if(columnSO !== null) {
        if(columnSO.type !== Types.INTEGER) {
          // TODO: better error message
          //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
          return Promise.reject(new Error("Array dimension must be of type int"));
        }
        column = columnSO.number;
      }

      if (line >= mustBeArray.lines) {
        // TODO: better error message
        return Promise.reject(new Error(`${exp.id}: index out of bounds: ${lines}`));
      }
      if (column !== null && mustBeArray.columns === null ){
        // TODO: better error message
        return Promise.reject(new Error(`${exp.id}: index out of bounds: ${column}`));
      }
      if(column !== null && column >= mustBeArray.columns) {
        // TODO: better error message
        return Promise.reject(new Error(`${exp.id}: index out of bounds: ${column}`));
      }
      return Promise.resolve(new StoreObjectArrayAddress(mustBeArray.id, line, column, store));
    });
  }

  evaluateUnaryApp (store, unaryApp) {
    const $left = this.evaluateExpression(store, unaryApp.left);
    return $left.then( left => {
      const resultType = resultTypeAfterUnaryOp(unaryApp.op, left.type);
      if (resultType === Types.UNDEFINED) {
        // TODO: better urgent error message
        return Promise.reject(new Error(`Cannot use this op to ${left.type}`));
      }
      switch (unaryApp.op.ord) {
        case Operators.ADD.ord:
          return new StoreObject(resultType, left.value);
        case Operators.SUB.ord:
          return new StoreObject(resultType, left.value.negated());
        case Operators.NOT.ord:
          return new StoreObject(resultType, !left.value);
        default:
        return Promise.reject(new Error('!!!Critical Invalid UnaryApp '+ unaryApp.op));
      }
    });
  }

  evaluateInfixApp (store, infixApp) {
    const $left = this.evaluateExpression(store, infixApp.left);
    const $right = this.evaluateExpression(store, infixApp.right);
    return Promise.all([$left, $right]).then(values => {
      const left = values[0];
      const right = values[1];
      const resultType = resultTypeAfterInfixOp(infixApp.op, left.type, right.type);
      if (resultType === Types.UNDEFINED) {
        // TODO: better urgent error message
        return Promise.reject(new Error(`Cannot use this ${infixApp.op.value} to ${left.type.value} and ${right.type.value}`));
      }
      let result = null;
      switch (infixApp.op.ord) {
        case Operators.ADD.ord:
          return new StoreObject(resultType, left.value.plus(right.value));
        case Operators.SUB.ord:
          return new StoreObject(resultType, left.value.minus(right.value));
        case Operators.MULT.ord:
          return new StoreObject(resultType, left.value.times(right.value));
        case Operators.DIV.ord: {
          result = left.value / right.value;
          if (resultType === Types.INTEGER)
            result = left.value.idiv(right.value);
          else
            result = left.value.div(right.value);
          return new StoreObject(resultType, result);
        }
        case Operators.MOD.ord:
          return new StoreObject(resultType, left.value.modulo(right.value));
        case Operators.GT.ord: {
          if (left.type === Types.STRING) {
            result = left.value.length > right.value.length;
          } else {
            result = left.value.gt(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.GE.ord: {
          if (left.type === Types.STRING) {
            result = left.value.length >= right.value.length;
          } else {
            result = left.value.gte(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.LT.ord: {
          if (left.type === Types.STRING) {
            result = left.value.length < right.value.length;
          } else {
            result = left.value.lt(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.LE.ord: {
          if (left.type === Types.STRING) {
            result = left.value.length <= right.value.length;
          } else {
            result = left.value.lte(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.EQ.ord: {
          if (left.type === Types.INTEGER || left.type === Types.REAL) {
            result = left.value.eq(right.value);
          } else {
            result = left.value === right.value;
          }
          return new StoreObject(resultType, result);
        }
        case Operators.NEQ.ord: {
          if (left.type === Types.INTEGER || left.type === Types.REAL) {
            result = !left.value.eq(right.value);
          } else {
            result = left.value !== right.value;
          }
          return new StoreObject(resultType, result);
        }
        case Operators.AND.ord:
          return new StoreObject(resultType, left.value && right.value);
        case Operators.OR.ord:
          return new StoreObject(resultType, left.value || right.value);
        default:
          return Promise.reject(new Error('!!!Critical Invalid InfixApp '+ infixApp.op));
      }
    });
  }

}
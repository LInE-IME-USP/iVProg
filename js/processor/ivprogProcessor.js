import { Store } from './store/store';
import { StoreObject } from './store/storeObject';
import { StoreObjectArray } from './store/storeObjectArray';
import { StoreObjectRef } from './store/storeObjectRef';
import { Modes } from './modes';
import { Context } from './context';
import { Types } from './../typeSystem/types';
import { Operators } from './../ast/operators';
import { LanguageDefinedFunction } from './definedFunctions';
import { resultTypeAfterInfixOp, resultTypeAfterUnaryOp } from './compatibilityTable';
import * as Commands from './../ast/commands/';
import * as Expressions from './../ast/expressions/';
import { StoreObjectArrayAddress } from './store/storeObjectArrayAddress';
import { StoreObjectArrayAddressRef } from './store/storeObjectArrayAddressRef';
import { CompoundType } from './../typeSystem/compoundType';
import { convertToString } from '../typeSystem/parsers';

let loopTimeoutMs = 10000

export class IVProgProcessor {

  static get LOOP_TIMEOUT () {
    return loopTimeoutMs;
  }

  static set LOOP_TIMEOUT (ms) {
    loopTimeoutMs = ms;
  }

  constructor (ast) {
    this.ast = ast;
    this.globalStore = new Store();
    this.stores = [this.globalStore];
    this.context = [Context.BASE];
    this.input = null;
    this.forceKill = false;
    this.loopTimers = [];
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
    let returnStoreObject = null;
    if(func.returnType instanceof CompoundType) {
      if(func.returnType.dimensions > 1) {
        returnStoreObject = new StoreObjectArray(func.returnType,-1,-1,[[]]);
      } else {
        returnStoreObject = new StoreObjectArray(func.returnType,-1,null,[]);
      }
    } else {
      returnStoreObject = new StoreObject(func.returnType, null);
    }
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
        if(formalParameter.type.isCompatible(stoObj.type)) {
          if(formalParameter.byRef && !stoObj.inStore) {
            throw new Error('You must inform a variable as parameter');
          }

          if(formalParameter.byRef) {
            let ref = null;
            if (stoObj instanceof StoreObjectArrayAddress) {
              ref = new StoreObjectArrayAddressRef(stoObj);
            } else {
              ref = new StoreObjectRef(stoObj.id, callerStore);
            }
            calleeStore.insertStore(formalParameter.id, ref);
          } else {
            let realValue = this.parseStoreObjectValue(stoObj);
            calleeStore.insertStore(formalParameter.id, realValue);
          }
        } else {
          throw new Error(`Parameter ${formalParameter.id} is not compatible with the value given.`);
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

    if(this.forceKill) {
      return Promise.reject("Interrupção forçada do programa!");
    } else if (store.mode === Modes.PAUSE) {
      return Promise.resolve(this.executeCommand(store, cmd));
    } else if(store.mode === Modes.RETURN) {
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
        .then(sto => {
          if(!Types.VOID.isCompatible(func.returnType) && sto.mode !== Modes.RETURN) {
            // TODO: better error message
            reject(new Error(`Function ${func.name} must have a return command`));
          } else {
            resolve(store);
          }
        })
        .catch(err => reject(err));
      return null;
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
    const outerRef = this;
    return new Promise((resolve, reject) => {
      try {
        outerRef.loopTimers.push(Date.now());
        outerRef.context.push(Context.BREAKABLE);
        const $newStore = outerRef.executeCommands(store, cmd.commands);
        $newStore.then(sto => {
          if(sto.mode === Modes.BREAK) {
            outerRef.context.pop();
            sto.mode = Modes.RUN;
            outerRef.loopTimers.pop();
            resolve(sto);
          }
          const $value = outerRef.evaluateExpression(sto, cmd.expression);
          $value.then(vl => {
            if (!vl.type.isCompatible(Types.BOOLEAN)) {
              // TODO: Better error message -- Inform line and column from token!!!!
              // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
              reject(new Error(`DoWhile expression must be of type boolean`));
            }
            if (vl.value) {
              outerRef.context.pop();
              outerRef.loopTimers.forEach(t => {
                if(Date.now() - t >= IVProgProcessor.LOOP_TIMEOUT) {
                  console.log("Timeout...");
                  outerRef.forceKill = true;
                  reject(new Error("Potential endless loop detected."));
                }  
              })
              resolve(outerRef.executeCommand(sto, cmd));
            } else {
              outerRef.context.pop();
              outerRef.loopTimers.pop();
              console.log("Clear Timeout...");
              resolve(sto);
            }
          }).catch(err => reject(err));
        }).catch(err => reject(err));
      } catch (error) {
        reject(error)
      }
      return null;
    });
  }

  executeWhile (store, cmd) {
    const outerRef = this;
    return new Promise((resolve, reject) => {
      try {
        outerRef.loopTimers.push(Date.now());
        outerRef.context.push(Context.BREAKABLE);
        const $value = outerRef.evaluateExpression(store, cmd.expression);
        $value.then(vl => {
          if(vl.type.isCompatible(Types.BOOLEAN)) {
            if(vl.value) {
              const $newStore = outerRef.executeCommands(store, cmd.commands);
              $newStore.then(sto => {
                outerRef.context.pop();
                if (sto.mode === Modes.BREAK) {
                  outerRef.loopTimers.pop();
                  sto.mode = Modes.RUN;
                  resolve(sto);
                } else
                  outerRef.loopTimers.forEach(t => {
                    if(Date.now() - t >= IVProgProcessor.LOOP_TIMEOUT) {
                      console.log("Timeout...");
                      outerRef.forceKill = true;
                      reject(new Error("Potential endless loop detected."));
                    }  
                  })
                  resolve(outerRef.executeCommand(sto, cmd));
              }).catch(err => reject(err));
            } else {
              outerRef.context.pop();
              outerRef.loopTimers.pop();
              console.log("Clear Timeout...");
              resolve(store);
            }
          } else {
            // TODO: Better error message -- Inform line and column from token!!!!
            // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
            reject(new Error(`Loop condition must be of type boolean`));
          }
        }).catch(err => reject(err));
        
      } catch (error) {
        reject(error);
      }
      return null;
    });
  }

  executeIfThenElse (store, cmd) {
    try {
      const $value = this.evaluateExpression(store, cmd.condition);
      return $value.then(vl => {
        if(vl.type.isCompatible(Types.BOOLEAN)) {
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

        if(vl === null && funcType.isCompatible(Types.VOID)) {
          return Promise.resolve(store);
        }

        if (vl === null || !funcType.type.isCompatible(vl.type)) {
          // TODO: Better error message -- Inform line and column from token!!!!
          // THIS IF SHOULD BE IN A SEMANTIC ANALYSER
          return Promise.reject(new Error(`Function ${funcName.value} must return ${funcType.type} instead of ${vl.type}.`));
        } else {
          let realValue = this.parseStoreObjectValue(vl);
          store.updateStore('$', realValue);
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
        let realValue = this.parseStoreObjectValue(vl);
        store.updateStore(cmd.id, realValue) 
        return store;
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeArrayIndexAssign (store, cmd) {
    return new Promise((resolve, reject) => {
      const mustBeArray = store.applyStore(cmd.id);
      if(!(mustBeArray.type instanceof CompoundType)) {
        reject(new Error(cmd.id + " is not a vector/matrix"));
        return;
      }
      const line$ = this.evaluateExpression(store, cmd.line);
      const column$ = this.evaluateExpression(store, cmd.column);
      const value$ =  this.evaluateExpression(store, cmd.expression);
      Promise.all([line$, column$, value$]).then(results => {
        const lineSO = results[0];
        if(!Types.INTEGER.isCompatible(lineSO.type)) {
          // TODO: better error message
          //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
          reject(new Error("Array dimension must be of type int"));
          return;
        }
        const line = lineSO.number;
        const columnSO = results[1];
        let column = null
        if (columnSO !== null) {
          if(!Types.INTEGER.isCompatible(columnSO.type)) {
            // TODO: better error message
            //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
            reject(new Error("Array dimension must be of type int"));
            return;
          }
          column = columnSO.number;
        }
        const value = this.parseStoreObjectValue(results[2]);
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
         if (value.type instanceof CompoundType) {
           reject(new Error("Invalid operation. This must be a value: line "+cmd.sourceInfo.line));
           return;
         }
         newArray.value[line].value[column] = value;
         store.updateStore(cmd.id, newArray);
        } else {
         if(mustBeArray.columns !== null && value.type instanceof CompoundType) {
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
          if(!Types.INTEGER.isCompatible(lineSO.type)) {
            // TODO: better error message
            //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
            return Promise.reject(new Error("Array dimension must be of type int"));
          }
          const line = lineSO.number;
          const columnSO = values[1];
          let column = null
          if (columnSO !== null) {
            if(!Types.INTEGER.isCompatible(columnSO.type)) {
              // TODO: better error message
              //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
              return Promise.reject(new Error("Array dimension must be of type int"));
            }
            column = columnSO.number;
          }
          const value = values[2];
          const temp = new StoreObjectArray(cmd.type, line, column, null);
          store.insertStore(cmd.id, temp);
          let realValue = value;
          if (value !== null) {
            if(value instanceof StoreObjectArrayAddress) {
              if(value.type instanceof CompoundType) {
                realValue = Object.assign(new StoreObjectArray(null,null,null), value.refValue);
              } else {
                realValue = Object.assign(new StoreObject(null,null), value.refValue);
              }
            }
          } else {
            realValue = new StoreObjectArray(cmd.type, line, column, [])
            if(column !== null) {
              for (let i = 0; i < line; i++) {
                realValue.value.push(new StoreObjectArray(new CompoundType(cmd.type.innerType, 1), column, null, []));
              }
            }
          }
          realValue.readOnly = cmd.isConst;
          store.updateStore(cmd.id, realValue);
          return store;
        });
        
      } else {
        const temp = new StoreObject(cmd.type, null);
        store.insertStore(cmd.id, temp);
        return $value.then(vl => {
          let realValue = vl;
          if (vl !== null) {
            if(vl instanceof StoreObjectArrayAddress) {
              if(vl.type instanceof CompoundType) {
                realValue = Object.assign(new StoreObjectArray(null,null,null), vl.refValue);
              } else {
                realValue = Object.assign(new StoreObject(null,null), vl.refValue);
              }
            }
          } else {
            realValue = new StoreObject(cmd.type,0);
          }
          realValue.readOnly = cmd.isConst;
          store.updateStore(cmd.id, realValue);
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
    return Promise.resolve(null);
  }

  evaluateFunctionCall (store, exp) {
    const func = this.findFunction(exp.id);
    if(Types.VOID.isCompatible(func.returnType)) {
      // TODO: better error message
      return Promise.reject(new Error(`Function ${exp.id} cannot be used inside an expression`));
    }
    const $newStore = this.runFunction(func, exp.actualParameters, store);
    return $newStore.then( sto => {
      if(sto.mode !== Modes.RETURN) {
        return Promise.reject(new Error("The function that was called did not had a return command: "+exp.id));
      }
      const val = sto.applyStore('$');
      if (val instanceof StoreObjectArray) {
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
        const type = new CompoundType(list[0].type.innerType, 2);
        const arr = new StoreObjectArray(type, list.length, list[0].lines, list);
        if(arr.isValid)
          return Promise.resolve(arr);
        else
          return Promise.reject(new Error(`Invalid array`))
      });
    } else {
      return this.evaluateVector(store, exp.value).then(list => {
        const type = new CompoundType(list[0].type, 1);
        const stoArray = new StoreObjectArray(type, list.length, null, list);
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
      return $vector.then(list => {
        const type = new CompoundType(list[0].type, 1);
        return new StoreObjectArray(type, list.length, null, list)
      });
    } ));
  }

  evaluateLiteral (_, exp) {
    return Promise.resolve(new StoreObject(exp.type, exp.value));
  }

  evaluateVariableLiteral (store, exp) {
    try {
      const val = store.applyStore(exp.id);
      if (val instanceof StoreObjectArray) {
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
    if (!(mustBeArray.type instanceof CompoundType)) {
      // TODO: better error message
      console.log(mustBeArray.type);
      return Promise.reject(new Error(`${exp.id} is not of type array`));
    }
    const $line = this.evaluateExpression(store, exp.line);
    const $column = this.evaluateExpression(store, exp.column);
    return Promise.all([$line, $column]).then(values => {
      const lineSO = values[0];
      const columnSO = values[1];
      if(!Types.INTEGER.isCompatible(lineSO.type)) {
        // TODO: better error message
        //SHOULD NOT BE HERE. IT MUST HAVE A SEMANTIC ANALYSIS
        return Promise.reject(new Error("Array dimension must be of type int"));
      }
      const line = lineSO.number;
      let column = null;
      if(columnSO !== null) {
        if(!Types.INTEGER.isCompatible(columnSO.type)) {
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
      if (Types.UNDEFINED.isCompatible(resultType)) {
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
      if (Types.UNDEFINED.isCompatible(resultType)) {
        // TODO: better urgent error message
        return Promise.reject(new Error(`Cannot use this ${infixApp.op} to ${left.type} and ${right.type}`));
      }
      let result = null;
      switch (infixApp.op.ord) {
        case Operators.ADD.ord: {
          if(Types.STRING.isCompatible(left.type)) {
            const rightStr = convertToString(right.value, right.type);
            return new StoreObject(resultType, left.value + rightStr);
          } else if (Types.STRING.isCompatible(right.type)) {
            const leftStr = convertToString(left.value, left.type);
            return new StoreObject(resultType, leftStr + right.value);
          } else {
            return new StoreObject(resultType, left.value.plus(right.value));
          }
        }
        case Operators.SUB.ord:
          return new StoreObject(resultType, left.value.minus(right.value));
        case Operators.MULT.ord:
          return new StoreObject(resultType, left.value.times(right.value));
        case Operators.DIV.ord: {
          result = left.value / right.value;
          if (Types.INTEGER.isCompatible(resultType))
            result = left.value.divToInt(right.value);
          else
            result = left.value.div(right.value);
          return new StoreObject(resultType, result);
        }
        case Operators.MOD.ord:
          return new StoreObject(resultType, left.value.modulo(right.value));
        case Operators.GT.ord: {
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length > right.value.length;
          } else {
            result = left.value.gt(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.GE.ord: {
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length >= right.value.length;
          } else {
            result = left.value.gte(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.LT.ord: {
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length < right.value.length;
          } else {
            result = left.value.lt(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.LE.ord: {
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length <= right.value.length;
          } else {
            result = left.value.lte(right.value);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.EQ.ord: {
          if (Types.INTEGER.isCompatible(left.type) || Types.REAL.isCompatible(left.type)) {
            result = left.value.eq(right.value);
          } else {
            result = left.value === right.value;
          }
          return new StoreObject(resultType, result);
        }
        case Operators.NEQ.ord: {
          if (Types.INTEGER.isCompatible(left.type) || Types.REAL.isCompatible(left.type)) {
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

  parseStoreObjectValue (vl) {
    let realValue = vl;
    if(vl instanceof StoreObjectArrayAddress) {      
      if(vl.type instanceof CompoundType) {
        switch(vl.type.dimensions) {
          case 1: {
            realValue = new StoreObjectArray(vl.type, vl.value);
            break;
          }
          default: {
            throw new Error("Three dimensional array address...");
          }
        }
      } else {
        realValue = new StoreObject(vl.type, vl.value);
      }
    }
    return realValue;
  }

}
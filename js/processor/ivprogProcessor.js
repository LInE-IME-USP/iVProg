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
import { Config } from '../util/config';
import Decimal from 'decimal.js';
import { ProcessorErrorFactory } from './error/processorErrorFactory';
import { RuntimeError } from './error/runtimeError';

export class IVProgProcessor {

  static get LOOP_TIMEOUT () {
    return Config.loopTimeout;
  }

  static set LOOP_TIMEOUT (ms) {
    Config.setConfig({loopTimeout: ms});
  }

  static get MAIN_INTERNAL_ID () {
    return "$main";
  }

  constructor (ast) {
    this.ast = ast;
    this.globalStore = new Store("$global");
    this.stores = [this.globalStore];
    this.context = [Context.BASE];
    this.input = null;
    this.forceKill = false;
    this.loopTimers = [];
    this.output = null;
  }

  registerInput (input) {
    if(this.input !== null)
      this.input = null;
    this.input = input;
  }

  registerOutput (output) {
    if(this.output !== null)
      this.output = null;
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

  prepareState () {
    if(this.stores !== null) {
      for (let i = 0; i < this.stores.length; i++) {
        delete this.stores[i];
      }
      this.stores = null;
    }
    if(this.globalStore !== null)
      this.globalStore = null;
    this.globalStore = new Store("$global");
    this.stores = [this.globalStore];
    this.context = [Context.BASE];
  }

  interpretAST () {
    this.prepareState();
    return this.initGlobal().then( _ => {
      const mainFunc = this.findMainFunction();
      if(mainFunc === null) {
        throw ProcessorErrorFactory.main_missing();
      }
      return this.runFunction(mainFunc, [], this.globalStore);
    });
  }

  initGlobal () {
    if(!this.checkContext(Context.BASE)) {
      throw ProcessorErrorFactory.invalid_global_var();
    }
    return this.executeCommands(this.globalStore, this.ast.global);
  }

  findMainFunction () {
    return this.ast.functions.find(v => v.isMain);
  }

  findFunction (name) {
    if(name.match(/^\$.+$/)) {
      const fun = LanguageDefinedFunction.getFunction(name);
      if(!!!fun) {
        throw ProcessorErrorFactory.not_implemented(name);
      }
      return fun;
    } else {
      const val = this.ast.functions.find( v => v.name === name);
      if (!!!val) {
        throw ProcessorErrorFactory.function_missing(name);
      }
      return val;
    }
  }

  runFunction (func, actualParameters, store) {
    const funcName = func.isMain ? IVProgProcessor.MAIN_INTERNAL_ID : func.name;
    let funcStore = new Store(funcName);
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
    funcStore.insertStore('$', returnStoreObject);
    const newFuncStore$ = this.associateParameters(func.formalParameters, actualParameters, store, funcStore);
    const outerRef = this;
    return newFuncStore$.then(sto => {
      this.context.push(Context.FUNCTION);
      this.stores.push(sto);
      return this.executeCommands(sto, func.variablesDeclarations)
        .then(stoWithVars => outerRef.executeCommands(stoWithVars, func.commands)).then(finalSto => {
          outerRef.stores.pop();
          outerRef.context.pop();
          return finalSto;
        });
    });
  }

  associateParameters (formalList, actualList, callerStore, calleeStore) {
    const funcName = calleeStore.name === IVProgProcessor.MAIN_INTERNAL_ID ? 
      LanguageDefinedFunction.getMainFunctionName() : calleeStore.name;

    if (formalList.length != actualList.length) {
      throw ProcessorErrorFactory.invalid_parameters_size(funcName, formalList.length, actualList.length);
    }
    const promises$ = actualList.map(actualParameter => this.evaluateExpression(callerStore, actualParameter));
    return Promise.all(promises$).then(values => {
      for (let i = 0; i < values.length; i++) {
        const stoObj = values[i];
        const exp = actualList[i];
        let shouldTypeCast = false;
        const formalParameter = formalList[i];
        if(!formalParameter.type.isCompatible(stoObj.type)) {
          if (Config.enable_type_casting && !formalParameter.byRef
            && Store.canImplicitTypeCast(formalParameter.type, stoObj.type)) {
              shouldTypeCast =  true;
          } else {
            throw ProcessorErrorFactory.invalid_parameter_type(funcName, exp.toString());
          }
        }

        if(formalParameter.byRef && !stoObj.inStore) {
          throw ProcessorErrorFactory.invalid_ref(funcName, exp.toString());
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
          if (shouldTypeCast) {
            realValue = Store.doImplicitCasting(formalParameter.type, realValue);
          }
          calleeStore.insertStore(formalParameter.id, realValue);
        }
      }
      return calleeStore;
    });
  }

  executeCommands (store, cmds) {
    // helper to partially apply a function, in this case executeCommand
    const outerRef = this;
    const partial = (fun, cmd) => (sto) => fun(sto, cmd);
    return cmds.reduce((lastCommand, next) => {
      const nextCommand = partial(outerRef.executeCommand.bind(outerRef), next);
      return lastCommand.then(nextCommand);
    }, Promise.resolve(store));
  }

  executeCommand (store, cmd) {
    if(this.forceKill) {
      return Promise.reject("FORCED_KILL!");
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
    } else if (cmd instanceof Commands.DoWhile) {
      return this.executeDoWhile(store, cmd);
    } else if (cmd instanceof Commands.While) {
      return this.executeWhile(store, cmd);
    } else if (cmd instanceof Commands.For) {
      return this.executeFor(store, cmd);
    } else if (cmd instanceof Commands.Switch) {
      return this.executeSwitch(store, cmd);
    } else if (cmd instanceof Expressions.FunctionCall) {
      
      return this.executeFunctionCall(store, cmd);
    } else if (cmd instanceof Commands.SysCall) {
      return this.executeSysCall(store, cmd);
    } else {
      throw ProcessorErrorFactory.unknown_command(cmd.sourceInfo);
    }
  }

  executeSysCall (store, cmd) {
    const func = cmd.langFunc.bind(this);
    return func(store, cmd);
  }

  executeFunctionCall (store, cmd) {
    let func = null;
    if(cmd.isMainCall) {
      func = this.findMainFunction();
    } else {
      func = this.findFunction(cmd.id);
    }
    return this.runFunction(func, cmd.actualParameters, store)
      .then(sto => {
        if(!Types.VOID.isCompatible(func.returnType) && sto.mode !== Modes.RETURN) {
          const funcName = func.name === IVProgProcessor.MAIN_INTERNAL_ID ? 
            LanguageDefinedFunction.getMainFunctionName() : func.name;
          return Promise.reject(ProcessorErrorFactory.function_no_return(funcName));
        } else {
          return store;
        }
      })
  }

  executeSwitch (store, cmd) {
    this.context.push(Context.BREAKABLE);
    const outerRef = this;
    const caseSequence = cmd.cases.reduce( (prev,next) => {
      return prev.then( tuple => {
        if(outerRef.ignoreSwitchCases(tuple[1])) {
          return Promise.resolve(tuple);
        } else if(tuple[0] || next.isDefault) {
          return outerRef.executeCommands(tuple[1], next.commands)
            .then(nSto => Promise.resolve([true, nSto]));
        } else {
          return outerRef.evaluateExpression(tuple[1],
            new Expressions.InfixApp(Operators.EQ, cmd.expression, next.expression)
            ).then(equalityResult => {
              if (equalityResult.value) {
                return this.executeCommands(tuple[1], next.commands)
                  .then(nSto => Promise.resolve([true, nSto]));
              } else {
                return Promise.resolve(tuple);
              }
          });
        }
      });
    }, Promise.resolve([false, store]));
    return caseSequence.then(tuple => tuple[1]);
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
      forAsWhile.sourceInfo = cmd.sourceInfo;
      //END for -> while rewrite
      const newCmdList = [initCmd,forAsWhile];
      return this.executeCommands(store, newCmdList);
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeDoWhile (store, cmd) {
    const outerRef = this;
    try {
      outerRef.loopTimers.push(Date.now());
      outerRef.context.push(Context.BREAKABLE);
      const $newStore = outerRef.executeCommands(store, cmd.commands);
      return $newStore.then(sto => {
        if(sto.mode === Modes.BREAK) {
          outerRef.context.pop();
          sto.mode = Modes.RUN;
          outerRef.loopTimers.pop();
          return sto;
        }
        const $value = outerRef.evaluateExpression(sto, cmd.expression);
        return $value.then(vl => {
          if (!vl.type.isCompatible(Types.BOOLEAN)) {
            return Promise.reject(ProcessorErrorFactory.loop_condition_type_full(cmd.sourceInfo));
          }
          if (vl.value) {
            outerRef.context.pop();
            for (let i = 0; i < outerRef.loopTimers.length; i++) {
              const time = outerRef.loopTimers[i];
              if(Date.now() - time >= IVProgProcessor.LOOP_TIMEOUT) {
                outerRef.forceKill = true;
                return Promise.reject(ProcessorErrorFactory.endless_loop_full(cmd.sourceInfo));
              }
            }
            return outerRef.executeCommand(sto, cmd);
          } else {
            outerRef.context.pop();
            outerRef.loopTimers.pop();
            return sto;
          }
        })
      })
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeWhile (store, cmd) {
    const outerRef = this;
    try {
      outerRef.loopTimers.push(Date.now());
      outerRef.context.push(Context.BREAKABLE);
      const $value = outerRef.evaluateExpression(store, cmd.expression);
      return $value.then(vl => {
        if(vl.type.isCompatible(Types.BOOLEAN)) {
          if(vl.value) {
            const $newStore = outerRef.executeCommands(store, cmd.commands);
            return $newStore.then(sto => {
              outerRef.context.pop();
              if (sto.mode === Modes.BREAK) {
                outerRef.loopTimers.pop();
                sto.mode = Modes.RUN;
                return sto;
              }
              for (let i = 0; i < outerRef.loopTimers.length; i++) {
                const time = outerRef.loopTimers[i];
                if(Date.now() - time >= IVProgProcessor.LOOP_TIMEOUT) {
                  outerRef.forceKill = true;
                  return Promise.reject(ProcessorErrorFactory.endless_loop_full(cmd.sourceInfo));
                }
              }
              return outerRef.executeCommand(sto, cmd);
            });
          } else {
            outerRef.context.pop();
            outerRef.loopTimers.pop();
            return store;
          }
        } else {
          return Promise.reject(ProcessorErrorFactory.loop_condition_type_full(cmd.expression.toString(), cmd.sourceInfo));
        }
      })
      
    } catch (error) {
      return Promise.reject(error);
    }
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
          return Promise.reject(ProcessorErrorFactory.if_condition_type_full(cmd.condition.toString(), cmd.sourceInfo));
        }
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeReturn (store, cmd) {
    try {
      const funcType = store.applyStore('$').type;
      const $value = this.evaluateExpression(store, cmd.expression);
      const funcName = store.name === IVProgProcessor.MAIN_INTERNAL_ID ? 
        LanguageDefinedFunction.getMainFunctionName() : store.name;
      return $value.then(vl => {

        if(vl === null && funcType.isCompatible(Types.VOID)) {
          store.mode = Modes.RETURN;
          return Promise.resolve(store);
        }

        if (vl === null || !funcType.isCompatible(vl.type)) {
          const stringInfo = funcType.stringInfo();
          const info = stringInfo[0];
          return Promise.reject(ProcessorErrorFactory.invalid_return_type_full(funcName, info.type, info.dim, cmd.sourceInfo));
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

  executeBreak (store, cmd) {
    if(this.checkContext(Context.BREAKABLE)) {
      store.mode = Modes.BREAK;
      return Promise.resolve(store);
    } else {
      return Promise.reject(ProcessorErrorFactory.unexpected_break_command_full(cmd.sourceInfo));
    }
  }

  executeAssign (store, cmd) {
    try {
      const inStore = store.applyStore(cmd.id);
      const $value = this.evaluateExpression(store, cmd.expression);
      return $value.then( vl => {
        let realValue = this.parseStoreObjectValue(vl);
        if(!inStore.type.isCompatible(realValue.type)) {
          if(Config.enable_type_casting && Store.canImplicitTypeCast(inStore.type, vl.type)) {
            realValue = Store.doImplicitCasting(inStore.type, realValue);
          } else {
            const stringInfo = inStore.type.stringInfo()
            const info = stringInfo[0]
            return Promise.reject(ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, cmd.sourceInfo));
          }
        }
        
        store.updateStore(cmd.id, realValue) 
        return store;
      });
    } catch (error) {
      return Promise.reject(error);
    }
  }

  executeArrayIndexAssign (store, cmd) {
    const mustBeArray = store.applyStore(cmd.id);
    if(!(mustBeArray.type instanceof CompoundType)) {
      return Promise.reject(ProcessorErrorFactory.invalid_array_access_full(cmd.id, cmd.sourceInfo));
    }
    const line$ = this.evaluateExpression(store, cmd.line);
    const column$ = this.evaluateExpression(store, cmd.column);
    const value$ =  this.evaluateExpression(store, cmd.expression);
    return Promise.all([line$, column$, value$]).then(results => {
      const lineSO = results[0];
      if(!Types.INTEGER.isCompatible(lineSO.type)) {
        return Promise.reject(ProcessorErrorFactory.array_dimension_not_int_full(cmd.sourceInfo));
      }
      const line = lineSO.number;
      const columnSO = results[1];
      let column = null
      if (columnSO !== null) {
        if(!Types.INTEGER.isCompatible(columnSO.type)) {
          return Promise.reject(ProcessorErrorFactory.array_dimension_not_int_full(cmd.sourceInfo));
        }
        column = columnSO.number;
      }
      const value = this.parseStoreObjectValue(results[2]);
      if (line >= mustBeArray.lines) {
        if(mustBeArray.isVector) {
          return Promise.reject(ProcessorErrorFactory.vector_line_outbounds_full(cmd.id, line, mustBeArray.lines, cmd.sourceInfo));
        } else {
          return Promise.reject(ProcessorErrorFactory.matrix_line_outbounds_full(cmd.id, line, mustBeArray.lines, cmd.sourceInfo));
        }
      } else if (line < 0) {
        throw ProcessorErrorFactory.array_dimension_not_positive_full(cmd.sourceInfo);
      }
      if (column !== null && mustBeArray.columns === null ){
        return Promise.reject(ProcessorErrorFactory.vector_not_matrix_full(cmd.id, cmd.sourceInfo));
      }
      if(column !== null ) {
        if (column >= mustBeArray.columns) {
          return Promise.reject(ProcessorErrorFactory.matrix_column_outbounds_full(cmd.id, column,mustBeArray.columns, cmd.sourceInfo));
        } else if (column < 0) {
          throw ProcessorErrorFactory.array_dimension_not_positive_full(cmd.sourceInfo);
        }
      }

      const newArray = Object.assign(new StoreObjectArray(null,null,null), mustBeArray);
      if (column !== null) {
        if (value.type instanceof CompoundType || !newArray.type.canAccept(value.type)) {
          const type = mustBeArray.type.innerType;
          const stringInfo = type.stringInfo()
          const info = stringInfo[0]
          return Promise.reject(ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, cmd.sourceInfo));
        }
        newArray.value[line].value[column] = value;
        store.updateStore(cmd.id, newArray);
      } else {
        if((mustBeArray.columns !== null && value.type instanceof CompoundType) || !newArray.type.canAccept(value.type)) {
          const type = mustBeArray.type;
          const stringInfo = type.stringInfo()
          const info = stringInfo[0]
          const exp = cmd.expression.toString()
          return Promise.reject(ProcessorErrorFactory.incompatible_types_array_full(exp,info.type, info.dim-1, cmd.sourceInfo));
        }
        newArray.value[line] = value;
        store.updateStore(cmd.id, newArray);
      }
      return store;
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
            return Promise.reject(ProcessorErrorFactory.array_dimension_not_int_full(cmd.sourceInfo));
          }
          const line = lineSO.number;
          if(line < 0) {
            throw ProcessorErrorFactory.array_dimension_not_positive_full(cmd.sourceInfo);
          }
          const columnSO = values[1];
          let column = null
          if (columnSO !== null) {
            if(!Types.INTEGER.isCompatible(columnSO.type)) {
              return Promise.reject(ProcessorErrorFactory.array_dimension_not_int_full(cmd.sourceInfo));
            }
            column = columnSO.number;
            if(column < 0) {
              throw ProcessorErrorFactory.array_dimension_not_positive_full(cmd.sourceInfo);
            }
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
            if(!vl.type.isCompatible(cmd.type)) {
              if(Config.enable_type_casting && Store.canImplicitTypeCast(cmd.type, vl.type)) {
                realValue = Store.doImplicitCasting(cmd.type, realValue);
              } else {
                const stringInfo = typeInfo.type.stringInfo();
                const info = stringInfo[0];
                return Promise.reject(ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, cmd.sourceInfo));
              }
            }
            if(vl instanceof StoreObjectArrayAddress) {
              if(vl.type instanceof CompoundType) {
                return Promise.reject(new Error("!!!Critical Error: Compatibility check failed, a Type accepts a CompoundType"))
              } else {
                realValue = Object.assign(new StoreObject(null,null), vl.refValue);
              }
            }
          } else {
            realValue = new StoreObject(cmd.type, 0);
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
    if(exp.isMainCall) {
      return Promise.reject(ProcessorErrorFactory.void_in_expression_full(LanguageDefinedFunction.getMainFunctionName(), exp.sourceInfo));
    }
    const func = this.findFunction(exp.id);
    if(Types.VOID.isCompatible(func.returnType)) {
      return Promise.reject(ProcessorErrorFactory.void_in_expression_full(exp.id, exp.sourceInfo));
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
    const errorHelperFunction = (validationResult, exp) => {
      const errorCode = validationResult[0];
      switch(errorCode) {
        case StoreObjectArray.WRONG_COLUMN_NUMBER: {
          const columnValue = validationResult[1];
          return Promise.reject(ProcessorErrorFactory.invalid_array_literal_column_full(arr.columns, columnValue, exp.sourceInfo));
        }
        case StoreObjectArray.WRONG_LINE_NUMBER: {
          const lineValue = validationResult[1];
          return Promise.reject(ProcessorErrorFactory.invalid_array_literal_line_full(arr.lines, lineValue, exp.sourceInfo));
        }
        case StoreObjectArray.WRONG_TYPE: {
          let line = null;
          let strExp = null;
          if (validationResult.length > 2) {
            line = validationResult[1];
            const column = validationResult[2];
            strExp = exp.value[line].value[column].toString()
          } else {
            line = validationResult[1];
            strExp = exp.value[line].toString()
          }
          return Promise.reject(ProcessorErrorFactory.invalid_array_literal_type_full(strExp, exp.sourceInfo));            }
      }
    };
    if(!exp.isVector) {
      const $matrix = this.evaluateMatrix(store, exp.value);
      return $matrix.then(list => {
        const type = new CompoundType(list[0].type.innerType, 2);
        const arr = new StoreObjectArray(type, list.length, list[0].lines, list);
        const checkResult = arr.isValid;
        if(checkResult.length == 0)
          return Promise.resolve(arr);
        else {
          return errorHelperFunction(checkResult, exp);
        }
      });
    } else {
      return this.evaluateVector(store, exp.value).then(list => {
        const type = new CompoundType(list[0].type, 1);
        const stoArray = new StoreObjectArray(type, list.length, null, list);
        const checkResult = stoArray.isValid;
        if(checkResult.length == 0)
          return Promise.resolve(stoArray);
        else {
          return errorHelperFunction(checkResult, exp);
        }
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
      return Promise.reject(ProcessorErrorFactory.invalid_array_access_full(exp.id, exp.sourceInfo));
    }
    const $line = this.evaluateExpression(store, exp.line);
    const $column = this.evaluateExpression(store, exp.column);
    return Promise.all([$line, $column]).then(values => {
      const lineSO = values[0];
      const columnSO = values[1];
      if(!Types.INTEGER.isCompatible(lineSO.type)) {
        return Promise.reject(ProcessorErrorFactory.array_dimension_not_int_full(exp.sourceInfo));
      }
      const line = lineSO.number;
      let column = null;
      if(columnSO !== null) {
        if(!Types.INTEGER.isCompatible(columnSO.type)) {
          return Promise.reject(ProcessorErrorFactory.array_dimension_not_int_full(exp.sourceInfo));
        }
        column = columnSO.number;
      }

      if (line >= mustBeArray.lines) {
        if(mustBeArray.isVector) {
          return Promise.reject(ProcessorErrorFactory.vector_line_outbounds_full(exp.id, line, mustBeArray.lines, exp.sourceInfo));
        } else {
          return Promise.reject(ProcessorErrorFactory.matrix_line_outbounds_full(exp.id, line, mustBeArray.lines, exp.sourceInfo));
        }
      } else if (line < 0) {
        throw ProcessorErrorFactory.array_dimension_not_positive_full(exp.sourceInfo);
      }
      if (column !== null && mustBeArray.columns === null ){
        return Promise.reject(ProcessorErrorFactory.vector_not_matrix_full(exp.id, exp.sourceInfo));
      }
      if(column !== null ) {
        if (column >= mustBeArray.columns) {
          return Promise.reject(ProcessorErrorFactory.matrix_column_outbounds_full(exp.id, column,mustBeArray.columns, exp.sourceInfo));
        } else if (column < 0) {
          throw ProcessorErrorFactory.array_dimension_not_positive_full(exp.sourceInfo);
        }
        
      }
      return Promise.resolve(new StoreObjectArrayAddress(mustBeArray.id, line, column, store));
    });
  }

  evaluateUnaryApp (store, unaryApp) {
    const $left = this.evaluateExpression(store, unaryApp.left);
    return $left.then( left => {
      const resultType = resultTypeAfterUnaryOp(unaryApp.op, left.type);
      if (Types.UNDEFINED.isCompatible(resultType)) {
        const stringInfo = left.type.stringInfo();
        const info = stringInfo[0];
        return Promise.reject(ProcessorErrorFactory.invalid_unary_op_full(unaryApp.op, info.type, info.dim, unaryApp.sourceInfo));
      }
      switch (unaryApp.op.ord) {
        case Operators.ADD.ord:
          return new StoreObject(resultType, left.value);
        case Operators.SUB.ord:
          return new StoreObject(resultType, left.value.negated());
        case Operators.NOT.ord:
          return new StoreObject(resultType, !left.value);
        default:
          return Promise.reject(new RuntimeError('!!!Critical Invalid UnaryApp '+ unaryApp.op));
      }
    });
  }

  evaluateInfixApp (store, infixApp) {
    const $left = this.evaluateExpression(store, infixApp.left);
    const $right = this.evaluateExpression(store, infixApp.right);
    return Promise.all([$left, $right]).then(values => {
      let shouldImplicitCast = false;
      const left = values[0];
      const right = values[1];
      let resultType = resultTypeAfterInfixOp(infixApp.op, left.type, right.type);
      if (Types.UNDEFINED.isCompatible(resultType)) {
        if (Config.enable_type_casting && Store.canImplicitTypeCast(left.type, right.type)) {
          shouldImplicitCast = true;
        } else {
          const stringInfoLeft = left.type.stringInfo();
          const infoLeft = stringInfoLeft[0];
          const stringInfoRight = right.type.stringInfo();
          const infoRight = stringInfoRight[0];
          return Promise.reject(ProcessorErrorFactory.invalid_infix_op_full(infixApp.op, infoLeft.type, infoLeft.dim,
            infoRight.type,infoRight.dim,infixApp.sourceInfo));
        }
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
        case Operators.MULT.ord: {
          result = left.value.times(right.value);
          if(result.dp() > Config.decimalPlaces) {
            result = new Decimal(result.toFixed(Config.decimalPlaces));
          }
          return new StoreObject(resultType, result);
        }
        case Operators.DIV.ord: {
          if (Types.INTEGER.isCompatible(resultType))
            result = left.value.divToInt(right.value);
          else
            result = left.value.div(right.value);
          if(result.dp() > Config.decimalPlaces) {
            result = new Decimal(result.toFixed(Config.decimalPlaces));
          }
          return new StoreObject(resultType, result);
        }
        case Operators.MOD.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if(shouldImplicitCast) {
            resultType = Types.INTEGER;
            leftValue = leftValue.trunc();
            rightValue = rightValue.trunc();
          }
          result = leftValue.modulo(rightValue);
          if(result.dp() > Config.decimalPlaces) {
            result = new Decimal(result.toFixed(Config.decimalPlaces));
          }
          return new StoreObject(resultType, result);
        }          
        case Operators.GT.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length > right.value.length;
          } else {
            if (shouldImplicitCast) {
              resultType = Types.BOOLEAN;
              leftValue = leftValue.trunc();
              rightValue = rightValue.trunc();
            }
            result = leftValue.gt(rightValue);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.GE.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length >= right.value.length;
          } else {
            if (shouldImplicitCast) {
              resultType = Types.BOOLEAN;
              leftValue = leftValue.trunc();
              rightValue = rightValue.trunc();
            }
            result = leftValue.gte(rightValue);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.LT.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length < right.value.length;
          } else {
            if (shouldImplicitCast) {
              resultType = Types.BOOLEAN;
              leftValue = leftValue.trunc();
              rightValue = rightValue.trunc();
            }
            result = leftValue.lt(rightValue);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.LE.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if (Types.STRING.isCompatible(left.type)) {
            result = left.value.length <= right.value.length;
          } else {
            if (shouldImplicitCast) {
              resultType = Types.BOOLEAN;
              leftValue = leftValue.trunc();
              rightValue = rightValue.trunc();
            }
            result = leftValue.lte(rightValue);
          }
          return new StoreObject(resultType, result);
        }
        case Operators.EQ.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if (Types.INTEGER.isCompatible(left.type) || Types.REAL.isCompatible(left.type)) {
            if (shouldImplicitCast) {
              resultType = Types.BOOLEAN;
              leftValue = leftValue.trunc();
              rightValue = rightValue.trunc();
            }
            result = leftValue.eq(rightValue);
          } else {
            result = left.value === right.value;
          }
          return new StoreObject(resultType, result);
        }
        case Operators.NEQ.ord: {
          let leftValue = left.value;
          let rightValue = right.value;
          if (Types.INTEGER.isCompatible(left.type) || Types.REAL.isCompatible(left.type)) {
            if (shouldImplicitCast) {
              resultType = Types.BOOLEAN;
              leftValue = leftValue.trunc();
              rightValue = rightValue.trunc();
            }
            result = !leftValue.eq(rightValue);
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
          return Promise.reject(new RuntimeError('!!!Critical Invalid InfixApp '+ infixApp.op));
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
            throw new RuntimeError("Three dimensional array address...");
          }
        }
      } else {
        realValue = new StoreObject(vl.type, vl.value);
      }
    }
    return realValue;
  }

}
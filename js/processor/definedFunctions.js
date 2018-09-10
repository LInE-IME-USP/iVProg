import * as Commands from './../ast/commands';
import {Types, toInt, toString, toBool} from './../ast/types';
import { LanguageService } from '../services/languageService';
import { StoreObject } from './store/storeObject';

function createOutputFun () {
  const writeFunction = function (store, _) {
    const val = store.applyStore('p1');
    this.output.sendOutput(''+val.value);
    return Promise.resolve(store);
  }
  const block = new Commands.CommandBlock([], [new Commands.SysCall(writeFunction)]);
  const func = new Commands.Function('$write', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, false)],
    block);
  return func;
}

function createInputFun () {
  const readFunction = function (store, _) {
    const request = new Promise((resolve, _) => {
      this.input.requestInput(resolve);
    });
    return request.then(text => {
      const typeToConvert = store.applyStore('p1').type;
      let stoObj = null;
      if (typeToConvert === Types.INTEGER) {
        const val = toInt(text);
        stoObj = new StoreObject(Types.INTEGER, val);
      } else if (typeToConvert === Types.REAL) {
        stoObj = new StoreObject(Types.REAL, parseFloat(text));
      } else if (typeToConvert === Types.BOOLEAN) {
        stoObj = new StoreObject(Types.BOOLEAN, toBool(text));
      } else if (typeToConvert === Types.STRING) {
        stoObj = new StoreObject(Types.STRING, toString(text));
      }
      store.updateStore('p1', stoObj);
      return Promise.resolve(store);
    });
  }
  const block = new Commands.CommandBlock([],  [new Commands.SysCall(readFunction)]);
  const func = new Commands.Function('$read', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, true)],
    block);
  return func;
}

function valueToKey (value, object) {
  for (const key in object) {
    if(object.hasOwnProperty(key)){
      if (object[key] === value) {
        return key;
      }
    }
  }
  return null;
}

const funcsObject = {
  $read: createInputFun(),
  $write: createOutputFun()
}

export const LanguageDefinedFunction = Object.freeze({
  getMainFunctionName: () => LanguageService.getCurrentLangFuncs().main_function,
  getInternalName: (localName) => valueToKey(localName, LanguageService.getCurrentLangFuncs()),
  getFunction: (internalName) => funcsObject[internalName],
});
import * as Commands from './../ast/commands';
import {Types} from './../ast/types';
import { LanguageService } from '../services/languageService';

function createOutputFun () {
  const block = new Commands.CommandBlock([], [new Commands.SysCall('$write')]);
  const func = new Commands.Function('$write', Types.VOID,
    [new Commands.FormalParameter(Types.ALL, 'p1', 0, false)],
    block);
  return func;
}

function createInputFun () {
  const block = new Commands.CommandBlock([],  [new Commands.SysCall('$read')]);
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
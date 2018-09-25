import { LanguageService } from '../services/languageService';
import {createInputFun, createOutputFun} from './lib/io';

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
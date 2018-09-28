import { LanguageService } from '../services/languageService';
import {createInputFun, createOutputFun} from './lib/io';
import {createLengthFun, createLowercaseFun,
  createrCharAtFun, createSubstringFun,
  createUppercaseFun} from './lib/strings';
import {createMatrixColumnsFun, createMatrixLinesFun,
  createNumElementsFun} from './lib/arrays';
import {createCastBoolFun, createCastIntFun,
  createCastRealFun, createCastStringFun,
  createIsBoolFun, createIsIntFun,
  createIsRealFun} from './lib/lang';
import {createAbsFun, createCosFun,
  createInvertFun, createLogFun,
  createMaxFun, createMinFun,
  createNegateFun, createPowFun,
  createSinFun, createSqrtFun,
  createTanFun} from './lib/math';

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

function concatObjects (...objs) {
  let result = {};
  for (let i = 0; i < objs.length; i++) {
    const obj = objs[i];
    for(const key in obj) {
      if(obj.hasOwnProperty(key)) {
        result[key] = obj[key];
      }
    }
  }
  return result;
}

const libsObject = {
  $mathLib: {
    $sin: createSinFun(),
    $cos: createCosFun(),
    $tan: createTanFun(),
    $sqrt: createSqrtFun(),
    $pow: createPowFun(),
    $log: createLogFun(),
    $abs: createAbsFun(),
    $negate: createNegateFun(),
    $invert: createInvertFun(),
    $max: createMaxFun(),
    $min: createMinFun()
  },
  $ioLib: {
    $read: createInputFun(),
    $write: createOutputFun()
  },
  $strLib: {
    $substring: createSubstringFun(),
    $length: createLengthFun(),
    $uppercase: createUppercaseFun(),
    $lowercase: createLowercaseFun(),
    $charAt: createrCharAtFun(),
  },
  $arrayLib: {
    $numElements: createNumElementsFun(),
    $matrixLines: createMatrixLinesFun(),
    $matrixColumns: createMatrixColumnsFun()
  },
  $langLib: {
    $isReal: createIsRealFun(),
    $isInt: createIsIntFun(),
    $isBool: createIsBoolFun(),
    $castReal: createCastRealFun(),
    $castInt: createCastIntFun(),
    $castBool: createCastBoolFun(),
    $castString: createCastStringFun()
  }
}

const funcsObject = concatObjects(libsObject.$ioLib, libsObject.$langLib,
  libsObject.$strLib, libsObject.$arrayLib);

export const LanguageDefinedFunction = Object.freeze({
  getMainFunctionName: () => LanguageService.getCurrentLangFuncs().main_function,
  getInternalName: (localName) => {
    if (localName.indexOf(".") !== -1) {
      const names = localName.split(".");
      const lib = valueToKey(names[0], LanguageService.getCurrentLangLibs());
      const internalName = valueToKey(names[1], LanguageService.getCurrentLangFuncs());
      if (lib === null || internalName === null) {
        return null;
      }
      return lib + "." + internalName;
    }
    return valueToKey(localName, LanguageService.getCurrentLangFuncs());
  },
  getFunction: (internalName) => {
    if (internalName.indexOf(".") !== -1) {
      const names = internalName.split(".");
      const libName = names[0];
      const funName = names[1];
      return libsObject[libName][funName];
    }
    return funcsObject[internalName];
  },
});
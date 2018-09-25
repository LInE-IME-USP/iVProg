import { LanguageService } from "../services/languageService";
import { BigNumber } from 'bignumber.js'

export const Types = Object.freeze({
  INTEGER: {value: "int", ord: 0},
  REAL: {value: "real", ord: 1},
  STRING: {value: "string", ord: 2},
  BOOLEAN: {value: "bool", ord: 3},
  VOID: {value: "void", ord: 4},
  ARRAY: {value: 'array', ord: 5},
  UNDEFINED: {value: 'undefined', ord: 6},
  ALL: {value: 'all', ord: 7}
});

export function toInt (str) {
  return new BigNumber(str);
}

export function toString (str) {
  let value = str.replace(/^"/, '');
  value = value.replace(/"$/, '');
  value = value.replace(/\\b/g, "\b");
  value = value.replace(/\\t/g, "\t");
  value = value.replace(/\\n/g, "\n");
  value = value.replace(/\\r/g, "\r");
  value = value.replace(/\\\"/g, "\"");
  value = value.replace(/\\\'/g, "\'");
  value = value.replace(/\\\\/g, "\\");
  return value;
}

export function toReal (value) {
  return new BigNumber(value);
}

export function toBool (str) {
  const val = "'" + str + "'";
  const lexer = LanguageService.getCurrentLexer();
  const instance = new lexer(null);
  if (instance.literalNames[lexer.RK_TRUE] === val) {
    return true;
  } else if (instance.literalNames[lexer.RK_FALSE] === val) {
    return false;
  } else {
    // TODO: better error message
    throw new Error(str + "not a valid boolean");
  }
}
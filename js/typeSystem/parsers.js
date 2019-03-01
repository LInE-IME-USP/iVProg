import { LanguageService } from "../services/languageService";
import { Types } from "./types";
import Decimal from "decimal.js";

export function toInt (str) {
  return new Decimal(str);
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
  return new Decimal(value);
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

export function convertBoolToString (bool) {
  const lexer = LanguageService.getCurrentLexer();
  const instance = new lexer(null);
  if (bool) {
    return instance.literalNames[lexer.RK_TRUE];
  } else {
    return instance.literalNames[lexer.RK_FALSE];
  }
}

export function convertToString(value, type) {
  switch (type.ord) {
    case Types.INTEGER.ord:
      return value.toString();
    case Types.REAL.ord: {
      if (value.dp() <= 0) {
        return value.toFixed(1);  
      } else {
        return value.toNumber();
      }
    }
    case Types.BOOLEAN.ord:
      return convertBoolToString(value);
    default:
      return value;
  }
}
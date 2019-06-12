import line_i18n from 'line-i18n';
import { LanguageService } from "./languageService";
import { LanguageDefinedFunction } from "./../processor/definedFunctions";
import Langs from './../../i18n';
import { Operators } from "./../ast/operators";

class IVProgLocalizedStrings extends line_i18n.LocalizedStrings {

  constructor(langService, langsJsons, shouldListenToChange =  false) {
    super(langService, langsJsons, shouldListenToChange);
  }

  translateType (type, dim) {
    switch (dim) {
      case 0:
        return this.getUI(`type_${type}`);
      default:
        const transType = this.getUI(`type_${type}`);
        if(dim === 1)
          return this.getUI("matrix_info_string", [transType])
        else
          return this.getUI("vector_info_string", [transType])
    }
  }
  
  translateOp (op) {
    switch(op.ord) {
      case Operators.AND.ord:
      case Operators.OR.ord:
      case Operators.NOT.ord:
        return this.getUI(`logic_operator_${op.value}`);
      default:
        return op.value;
    }
  }

  translateInternalFunction (name, category = null) {
    if (category == null) {
      return LanguageDefinedFunction.getLocalName(name);
    } else {
      return LanguageDefinedFunction.getLocalName(`${category}.${name}`);
    }
  }
}

export const LocalizedStrings = Object.freeze(new IVProgLocalizedStrings(LanguageService, Langs, true));

let _instance = null;

export function getInstance () {
  if(_instance == null) {
    _instance = new IVProgLocalizedStrings(LanguageService, Langs);
  }
  return _instance;
}

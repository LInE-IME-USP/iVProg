import { LanguageService } from "./languageService";
import line_i18n from 'line-i18n';
import Langs from './../../i18n';

class IVProgLocalizedStrings extends line_i18n.LocalizedStrings {

  constructor(langService, langsJsons, shouldListenToChange =  false) {
    super(langService, langsJsons, shouldListenToChange);
  }

  translateType (type, dim) {
    switch (dim) {
      case 0:
        return this.getUI(type);
      default:
        const transType = this.getUI(type);
        if(dim === 1)
          return this.getUI("vector_string", [transType])
        else
          return this.getUI("matrix_string", [transType])
    }
  }
  
  translateOp (op) {
    switch(op.ord) {
      case Operators.AND.ord:
      case Operators.OR.ord:
      case Operators.NOT.ord:
        return this.getUI(op.value);
      default:
        return op.value;
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

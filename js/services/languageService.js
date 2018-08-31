import Lexers from './../../grammar/';
import { isNullOrUndefined } from 'util';

const DEFAULT_LANG = 'pt';

export const LanguageService  = ({

  getLang: () => {
    const lang = localStorage.getItem('ivprog.lang');
    if (lang === null) {
      throw new Error("Internal Error. User language information has not been set");
    }
    return lang;
  },

  getDefaultLang: () => {
    return DEFAULT_LANG;
  },

  getCurrentLexer: () => {
    const lexer = Lexers[LanguageService.getLang()];
    if(lexer === null || lexer === undefined) {
      return Lexers[DEFAULT_LANG];
    } else {
      return lexer;
    }
  }

});
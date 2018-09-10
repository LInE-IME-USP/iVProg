import Lexers from './../../grammar/';

const DEFAULT_LANG = "pt";

export const LanguageService  = Object.freeze({

  getLang: () => {
    const lang = localStorage.getItem('ivprog.lang');
    if (lang === null || lang === undefined) {
      console.warn("Internal Error. User language information has not been set. Returning default...");
      return LanguageService.getDefaultLang();
    }
    return lang;
  },

  getDefaultLang: () => {
    return DEFAULT_LANG;
  },

  getCurrentLexer: () => {
    const langInfo = Lexers[LanguageService.getLang()];
    if(langInfo === null || langInfo === undefined) {
      return Lexers[DEFAULT_LANG].lexer;
    } else {
      return langInfo.lexer;
    }
  },

  getCurrentLangFuncs: () => {
    const langInfo = Lexers[LanguageService.getLang()];
    if(langInfo === null || langInfo === undefined) {
      return Lexers[DEFAULT_LANG].langFuncs;
    } else {
      return langInfo.langFuncs;
    }
  }

});
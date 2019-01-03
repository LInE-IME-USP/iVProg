import Lexers from './../../grammar/';
import line_i18n from 'line-i18n';

// This is for LanguageService with localStorage
// const DEFAULT_LANG = "pt";

class LanguageServiceExtended extends line_i18n.LanguageServiceNoLS {

  constructor () {
    super(iLMparameters.lang);
  }

  getCurrentLexer () {
    const langInfo = Lexers[this.getLang()];
    if(langInfo === null || langInfo === undefined) {
      return Lexers[this.getDefaultLang()].lexer;
    } else {
      return langInfo.lexer;
    }
  }

  getCurrentLangFuncs () {
    const langInfo = Lexers[this.getLang()];
    if(langInfo === null || langInfo === undefined) {
      return Lexers[this.getDefaultLang()].langFuncs;
    } else {
      return langInfo.langFuncs;
    }
  }

  getCurrentLangLibs () {
    const langInfo = Lexers[this.getLang()];
    if(langInfo === null || langInfo === undefined) {
      return Lexers[this.getDefaultLang()].langLibs;
    }
    return langInfo.langLibs;
  }
}

export const LanguageService  = new LanguageServiceExtended();

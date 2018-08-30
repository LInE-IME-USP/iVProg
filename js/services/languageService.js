import Lexers from './../../grammar/';
export class LanguageService {

  constructor () {
    throw new Error('LanguageService class must not be instantiated!');
  }

  static getLang () {
    const lang = localStorage.getItem('ivprog.lang');
    if (lang === null) {
      throw new Error("Internal Error. User language information has not been set");
    }
    return lang;
  }

  static getCurrentLexer () {
    return Lexers[LanguageService.getLang()];
  }
}
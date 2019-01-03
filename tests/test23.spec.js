import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('Variable initialization with a function call', () => {

    let input = `programa {

      funcao inicio() {
        inteiro a = func(5)
      }

      funcao inteiro fun(inteiro a) {
        retorne a * 2
      }
    }`;
    const lexer = LanguageService.getCurrentLexer();

    it(`should not throw an Error`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree.bind(as);
        expect(fun).not.toThrow();
    });
});

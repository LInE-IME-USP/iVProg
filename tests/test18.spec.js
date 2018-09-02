import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('Call to a function that receives no parameter', () => {

    let input = `programa {

      funcao inicio() {
        inteiro a
        fun()
      }

      funcao fun() {
        retorne
      }
    }`;
    const lexer = LanguageService.getCurrentLexer();

    it(`should not throw an Error`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree.bind(as);
        expect(fun).not.toThrow();
    });
});

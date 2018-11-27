import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('SwitchCase command', () => {

    let input = `funcao inteiro test(real i) {
      escolha (i) {
        caso 1:
          retorne 0
        caso contrario:
          retorne 4
      }
    }`;
    const lexer = LanguageService.getCurrentLexer();

    it(`should not result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        as.pushVariableStack();
        const fun = as.parseFunction.bind(as);
        expect(fun).not.toThrow();
        as.popVariableStack();
    });
});

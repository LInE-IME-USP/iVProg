import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('Break command inside a loop', () => {
    let input = `funcao inteiro test(real i) {
      inteiro a = 5 + i
      a = 5 + G[i][6]
      enquanto (a > 5) {
        a = a + 1
        se (a == 3) {
          pare
        }
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
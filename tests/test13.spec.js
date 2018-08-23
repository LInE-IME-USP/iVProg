import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
    SyntaxError
} from './../js/ast/SyntaxError';

describe('A complete program code', () => {
    let input = `programa {
      
      const real PI = 5.5
      inteiro V = -10*2

      funcao inteiro test(real i) {
        escolha (i) {
          caso 1:
            retorne 0
          casocontrario:
            retorne 4
        }
      }
    }`;
    const lexer = Lexers['pt_br'];

    it(`should not result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseFunction.bind(as);
        expect(fun).not.toThrow(SyntaxError);
    });
});

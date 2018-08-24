import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';

describe('A start function with no return type', () => {
    let input = `programa {
      
      const real PI = 5.5
      inteiro V = -10*2

      funcao inicio() {
        test(PI)
        retorne
      }

      funcao inteiro test(real i) {
        escolha (i) {
          caso 1:
            retorne 0
          caso contrario:
            retorne 4
        }
      }
    }`;
    const lexer = Lexers['pt_br'];

    it(`should not result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree.bind(as);
        expect(fun).not.toThrow();
    });
});

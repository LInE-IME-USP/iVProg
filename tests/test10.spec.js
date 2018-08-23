import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
    SyntaxError
} from './../js/ast/SyntaxError';

describe('IfThenElseIfThenElse command chain', () => {
    let input = `funcao inteiro test(real i) {
      inteiro a = 5 + i
      a = 5 + G[i][6]
      se (a > 5) {
        a = 5
      } senao se (a<5) {
        a = 0
      } senao {
        a = -1
      }
    }`;
    const lexer = Lexers['pt_br'];

    it(`should not result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseFunction.bind(as);
        expect(fun).not.toThrow();
    });
});
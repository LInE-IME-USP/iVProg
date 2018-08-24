import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
    SyntaxError
} from './../js/ast/SyntaxError';

describe('For with no \';\'', () => {
    let input = `funcao inteiro test(real i) {
      inteiro a = 5 + i
      a = 5 + G[i][6]
      para (a = 0
        a < 5
        a = a + 1) {
         a = a
      }
    }`;
    const lexer = Lexers['pt_br'];

    it(`should result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseFunction.bind(as);
        expect(fun).toThrow();
    });
});
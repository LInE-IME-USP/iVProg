import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
    SyntaxError
} from './../js/ast/SyntaxError';

describe('Literal arrays that have more than 2 dimensions', () => {
    let input = `programa {
        const inteiro a[1][1] = {
            {
                {1,2},
                {3,4}
            }
        }

        funcao inicio() {
            retorna
        }
    }`;
    const lexer = Lexers['pt_br'];

    it(`should result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree.bind(as);
        expect(fun).toThrow();
    });
});
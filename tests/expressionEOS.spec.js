import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
    SyntaxError
} from './../js/ast/SyntaxError';

describe('Expressions which ends with ID terminals:', () => {
    let input = 'test = i\nb = i + 1\n';
    const lexer = Lexers['pt_br'];

    it(`${input} should not result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        expect(as.parseIDCommand()).not.toThrow(SyntaxError);
    });
});
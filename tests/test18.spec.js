import Lexers from './../grammar/';
import * as Expressions from './../js/ast/expressions/';
import * as Commands from './../js/ast/commands/';
import {Types} from './../js/ast/types';
import {
    IVProgParser
} from './../js/ast/ivprogParser';

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
    const lexer = Lexers['pt_br'];

    it(`should not throw an Error`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree.bind(as);
        expect(fun).not.toThrow();
    });
});

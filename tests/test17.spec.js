import Lexers from './../grammar/';
import * as Expressions from './../js/ast/expressions/';
import * as Commands from './../js/ast/commands/';
import {Types} from './../js/ast/types';
import {
    IVProgParser
} from './../js/ast/ivprogParser';

describe('Variable declaration inside a function', () => {
    let input = `programa {

      funcao inicio() {
        inteiro a
      }
    }`;
    const lexer = Lexers['pt_br'];

    const ast = {
      global: [ ],
      functions: [
        new Commands.Function(null,Types.VOID,[],
        new Commands.CommandBlock([
          new Commands.Declaration('a',Types.INTEGER,null,false)],[]))
      ]
    }

    it(`must be inside the variables list`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree();
        expect(fun).toEqual(ast);
    });
});

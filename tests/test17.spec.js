import * as Expressions from './../js/ast/expressions/';
import * as Commands from './../js/ast/commands/';
import { Operators } from './../js/ast/operators';
import {Types} from './../js/ast/types';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('Variable declaration inside a function', () => {

    let input = `programa {

      funcao inicio() {
        inteiro a
        a = a + 1
      }
    }`;
    const lexer = LanguageService.getCurrentLexer();

    const ast = {
      global: [ ],
      functions: [
        new Commands.Function(null,Types.VOID,[],
        new Commands.CommandBlock([
          new Commands.Declaration('a',Types.INTEGER,null,false)],[
            new Commands.Assign('a',
              new Expressions.InfixApp(Operators.ADD, new Expressions.VariableLiteral('a'), new Expressions.IntLiteral(1)))]))
      ]
    }

    it(`must be inside the variables list`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseTree();
        expect(fun).toEqual(ast);
    });
});

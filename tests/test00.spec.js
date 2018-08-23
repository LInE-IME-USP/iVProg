import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import * as Expressions from './../js/ast/expressions/';
import * as Commands from './../js/ast/commands/';
import { Types } from './../js/ast/types';

describe("Testing Syntax Analysis for default", () => {
  var lang = 'pt_br';
  var input;

  var asa;
  var lexer;

  it("it should produce a valid AST", () => {
    lexer  = Lexers[lang];
    input = `programa {
    const real PI = 1
    const inteiro a[5][5]
    }`;

    asa = {
        global: [new Commands.Declaration('PI', Types.REAL, new Expressions.IntLiteral(1), true),
        new Commands.ArrayDeclaration('a', Types.INTEGER, new Expressions.IntLiteral(5), new Expressions.IntLiteral(5), null, true)],
        functions: []
      };
      const as = new IVProgParser(input, lexer);
      expect(as.parseTree()).toEqual(asa);
  });
});
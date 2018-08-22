import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import * as Expressions from './../js/ast/expressions/';
import * as Commands from './../js/ast/commands/';

describe("Testing Syntax Analysis for default", () => {
  var lang = 'pt_br';
  var input;

  var asa;
  var lexer;

  it("it should produce a valid AST", () => {
    lexer  = Lexers[lang];
    input = `programa {
    const real PI
    const inteiro a[5][5]
    }`;

    asa = {
        global: [new Commands.Declaration('PI', 'real', null, true),
        new Commands.ArrayDeclaration('a', 'int', new Expressions.IntLiteral(5), new Expressions.IntLiteral(5), null, true)],
        functions: []
      };
      const as = new IVProgParser(input, lexer);
      expect(as.parseTree()).toEqual(asa);
  });
});
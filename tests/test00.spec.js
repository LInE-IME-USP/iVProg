import {
    IVProgParser
} from './../js/ast/ivprogParser';
import * as Expressions from './../js/ast/expressions/';
import * as Commands from './../js/ast/commands/';
import { LanguageService } from './../js/services/languageService';
import { Types } from './../js/ast/types';

describe("Testing Syntax Analysis for default", () => {

  var input;

  const asa = {
    global: [new Commands.Declaration('PI', Types.REAL, new Expressions.IntLiteral(1), true),
    new Commands.ArrayDeclaration('a', Types.INTEGER, new Expressions.IntLiteral(5), new Expressions.IntLiteral(5), null, false)],
    functions: []
  };
  const lexer  = LanguageService.getCurrentLexer();

  it("it should produce a valid AST", () => {
    input = `programa {
    const real PI = 1
    inteiro a[5][5]
    }`;
      const as = new IVProgParser(input, lexer);
      expect(as.parseTree()).not.toEqual(asa);
  });
});
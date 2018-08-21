import Lexers from './../grammar/';
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import {
    SyntaxError}from './../js/ast/SyntaxError';

describe('Illegal newline syntax', () => {

  const input = `programa
  {
    const
      inteiro a = 1, b
    real
      PI = 5.6, c;
  }
  `;
  const lexer = Lexers['pt_br'];

  it(`should result in SyntaxError`, () => {
      const as = new IVProgParser(input, lexer);
      const fun = as.parseTree.bind(as);
      expect(fun).toThrow();
  });

});
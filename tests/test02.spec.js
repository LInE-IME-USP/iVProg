import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from './../js/services/languageService';

describe('Legal newline syntax', () => {

  const input = `programa
  {
    const inteiro val = 5

    funcao p (
      inteiro a,
      inteiro b
    )
    {
      teste = a + b
    }
  }
  `;
  const lexer = LanguageService.getCurrentLexer();

  it(`should not result in SyntaxError`, () => {
      const as = new IVProgParser(input, lexer);
      const fun = as.parseTree.bind(as);
      expect(fun).not.toThrow();
  });

});
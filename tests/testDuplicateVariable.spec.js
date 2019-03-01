import { IVProgParser } from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('A code with a duplicate variable declarations', function () {

  let input = `programa {

    funcao inicio() {
      real a = 90.0, a = 0.0
      escreva(Matematica.tan(a))
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should throw a syntax exception`, function () {
    const parser = new IVProgParser(input, lexer);
    const func = parser.parseTree;
    func.bind(parser);
    expect(func).toThrow();
  });
});

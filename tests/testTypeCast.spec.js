import { IVProgParser } from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('When implicit type casting is enabled', function () {

  let input = `programa {

    funcao inicio() {
      real v = 5
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should coerce integer into real`, function () {
    const parser = new IVProgParser(input, lexer);
    const fun = parser.parseTree.bind(parser);
    expect(fun).not.toThrow();
  });
});

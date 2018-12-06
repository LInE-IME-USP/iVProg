import { IVProgParser } from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';

describe('A code with a duplicate function', function () {

  let input = `programa {

    funcao inicio() {
      escreva(Matematica.tan(90))
    }

    funcao vazio inicio() {

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

import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from "./../js/processor/semantic/semanticAnalyser";
import { LanguageService } from '../js/services/languageService';

describe('A valid matrix declaration', function () {

  let input = `programa {

    funcao inicio() {
      real v[4][2] = {{1,1},{1,1},{1,1},{1,1}}
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should not throw a semantic exception`, function () {
    const parser = new IVProgParser(input, lexer);
    const semantic = new SemanticAnalyser(parser.parseTree());
    const fun = semantic.analyseTree.bind(semantic);
    expect(fun).not.toThrow();
  });
});

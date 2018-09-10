import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';

describe('The semantic analyser', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a = 5
      real i = 8.5
      inteiro b[a][i];
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();

  it(`should guarantee that array variable as dimensions are integers`, function () {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const fun = sem.analyseTree.bind(sem);
    expect(fun).toThrow();
  });
});

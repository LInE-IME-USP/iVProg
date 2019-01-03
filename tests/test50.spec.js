import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';

describe('The semantic analyser', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a[5] = {1, 2, 3, 4, 0xff}
      escreva(a[6])
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();

  it(`should not check when accessing a vector/matrix for size, only types`, function () {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const fun = sem.analyseTree.bind(sem);
    expect(fun).not.toThrow();
  });
});

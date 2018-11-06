import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';

describe('The semantic analyser', function () {

  const code = `programa {

    const inteiro V = 1

    funcao inicio() {
      inteiro a = 5
      a = aNumber()
    }

    funcao inteiro aNumber () {
      inteiro a = 5
      se (V == 1) {
        retorne a + 3
      } senao {
        a = a * 2
      }
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();

  it(`should guarantee that a function has an accessible return`, function () {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const fun = sem.analyseTree.bind(sem);
    expect(fun).toThrow();
  });
});

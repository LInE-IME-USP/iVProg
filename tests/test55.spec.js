import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';

describe('Math lib with no Lib mention ', function () {

  const code = `programa {

    funcao inicio() {
      real a = 1.0
      escreva(trocar_sinal(a))
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();

  it(`should throw an exception`, function () {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const fun = sem.analyseTree.bind(sem);
    expect(fun).toThrow();
  });
});

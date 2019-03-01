import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';

describe('A invalid relational operation inside an if', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a = 5
      se ( a * 2.3 > 8) {
        a = 8
      } senao {
        a = -1
      }
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

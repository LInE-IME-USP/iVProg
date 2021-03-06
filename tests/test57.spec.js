import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';
import { OutputTest } from '../js/util/outputTest';
import { IVProgProcessor } from '../js/processor/ivprogProcessor';

describe('Is_Real function ', function () {

  const code = `programa {

    funcao inicio() {
      cadeia a = "1.2"
      escreva(e_real(a))
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();

  it(`should return true if the string is valid real representation`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(sem.analyseTree());
    exec.registerOutput(out);
    exec.interpretAST().then(_ => {
      expect(out.list).toEqual(["verdadeiro"]);
      done();
    }).catch(err => done(err));
  });
});

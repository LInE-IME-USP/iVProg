import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';
import { OutputTest } from '../js/util/outputTest';
import { IVProgProcessor } from '../js/processor/ivprogProcessor';

describe('Math lib modulo function ', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a = -1
      inteiro b = Matematica.modulo(a)
      escreva(b)
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();

  it(`should be executed with no exception`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(sem.analyseTree());
    exec.registerOutput(out);
    exec.interpretAST().then(_ => {
      expect(out.list).toEqual(['1']);
      done();
    }).catch(err => done(err));
  });
});

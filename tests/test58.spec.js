import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';
import { OutputTest } from '../js/util/outputTest';
import { IVProgProcessor } from '../js/processor/ivprogProcessor';

describe('Assign to an array index', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a[2] = {0,0}
      a[0] = 1
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();

  it(`should not throw an exception`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(sem.analyseTree());
    exec.registerOutput(out);
    exec.interpretAST().then(_ => {
      done();
    }).catch(err => done(err));
  });
});

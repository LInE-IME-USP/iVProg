import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from './../js/services/languageService';
import { OutputTest } from './../js/util/outputTest';
import { InputTest } from './../js/util/inputTest';
import { IVProgProcessor } from './../js/processor/ivprogProcessor';

describe('Reading a single value into a vector/matrix position', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a[2] = {0,0}
      leia(a[0])
      escreva(a[0])
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();
  const inPut = new InputTest(['1'])

  it(`should produce a valid state`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(sem.analyseTree());
    exec.registerOutput(out);
    exec.registerInput(inPut);
    exec.interpretAST().then(sto => {
      expect(out.list).toEqual(['1']);
      done();
    }).catch(err => done(err));
  });
});

import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { SemanticAnalyser } from "./../js/processor/semantic/semanticAnalyser";
import { LanguageService } from '../js/services/languageService';

describe('Command Do...While after the set timeout', function () {

  IVProgProcessor.LOOP_TIMEOUT = 500;
  let input = `programa {

    funcao inicio() {
      inteiro a = 0
      faca {
        a = a + 1
      } enquanto(1 < 4)
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should be forcedly killed `, function (done) {
    const parser = new IVProgParser(input, lexer);
    const semantic = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(semantic.analyseTree());
    exec.interpretAST().then(_ => {
      done("No error thrown");
    }).catch( _ => {
      expect(1).toEqual(1);
      done();
    });
  });
});

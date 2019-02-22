import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { SemanticAnalyser } from "./../js/processor/semantic/semanticAnalyser";
import { LanguageService } from '../js/services/languageService';

describe('Tangent of 90° angles', function () {

  let input = `programa {

    funcao inicio() {
      escreva(Matematica.tangente(90))
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should throw an exception`, function (done) {
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

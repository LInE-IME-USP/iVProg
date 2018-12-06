import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { SemanticAnalyser } from "./../js/processor/semantic/semanticAnalyser";
import { LanguageService } from '../js/services/languageService';

describe('A call to the main function', function () {

  let input = `programa {
      inteiro a = 0
      funcao inicio () {
        se (a > 5) {
          retorne
        } senao {
          a = a + 1
          inicio()
        }
      }
    }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should produce a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const semantic = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(semantic.analyseTree());
    exec.interpretAST().then(sto => {
      expect(sto).toBeTruthy();
      done();
    }).catch(err => {
      done(err);
    });
  });
});

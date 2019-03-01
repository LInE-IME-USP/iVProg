import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('IfThenElse command ', function () {
  let input = `programa {

    funcao inicio() {
      inteiro a = 0
      se (a > 2) {
        a = 5
      } senao se (a < 2) {
        a = 10
      } senao {
        a = -1
      }
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should produce a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(10);
      done();
    }).catch( err => done(err));
  });
});

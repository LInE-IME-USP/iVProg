import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('Command Do...While', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = 0
      faca {
        a = a + 1
      } enquanto(a < 5)
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should result in a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(5);
      done();
    }).catch( err => done(err));
  });
});

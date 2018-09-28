import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('A finite while loop', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = 0
      inteiro b = 0
      enquanto (a < 5) {
        a = a + 1
      }
      b = 8
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should terminate`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(5);
      done();
    }).catch( err => done(err));
  });
});

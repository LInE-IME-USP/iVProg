import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('A break command inside a for loop', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = 0
      para(a = 0;a < 5; a = a + 1) {
        pare
      }
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should terminate it`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(0);
      done();
    }).catch( err => done(err));
  });
});

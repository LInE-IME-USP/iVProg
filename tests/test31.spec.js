import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('A case without return/break', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = 1
      escolha (a) {
        caso 0:
          a = a + 1
          pare
        caso 1:
          a = a + 2
        caso 2:
          a = a * 2
          pare
        caso contrario:
          a = 5 + 8
      }
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should fall through`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(6);
      done();
    }).catch( err => done(err));
  });
});

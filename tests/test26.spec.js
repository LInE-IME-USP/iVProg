import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('Assigning a line from a matrix to another vector', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a[2][2] = {{5,2},{8,6}}
      inteiro b[2] = a[0]
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should result in a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('b')).toBeTruthy();
      done();
    }).catch( err => done(err));
  });
});

import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { InputTest } from './../js/util/inputTest';
import { LanguageService } from '../js/services/languageService';

describe('The sum of a real and a integer', function () {

  const code = `programa {

    funcao inicio() {
      real a
      leia(a)
      a = a + 0xff
    }
  }`;

  localStorage.setItem('ivprog.lang', 'pt');
  const input = new InputTest(['5.8']);

  const lexer = LanguageService.getCurrentLexer();

  it(`should result in a real`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.registerInput(input);
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(5.8 + 0xff);
      localStorage.removeItem('ivprog.lang');
      done();
    }).catch( err => done(err));
  });
});

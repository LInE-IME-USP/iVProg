import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { InputTest } from './../js/util/inputTest';
import { LanguageService } from '../js/services/languageService';

describe('The read function', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a;
      leia(a);
    }
  }`;

  const input = new InputTest(['0xff']);

  const lexer = LanguageService.getCurrentLexer();

  it(`should read data from the input and convert it to the appropriate type`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.registerInput(input);
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').number).toEqual(255);
      done();
    }).catch( err => done(err));
  });
});

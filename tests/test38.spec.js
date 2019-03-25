import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { OutputTest } from './../js/util/outputTest';
import { LanguageService } from '../js/services/languageService';

describe('The write function', function () {

  const code = `programa {

    funcao inicio() {
      real a = 8.01
      escreva(a)
    }
  }`;

  const output = new OutputTest();

  const lexer = LanguageService.getCurrentLexer();

  it(`should print the value passed to it, no matter it's type`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.registerOutput(output);
    exec.interpretAST().then(sto => {
      expect(output.list).toEqual([8.01]);
      done();
    }).catch( err => done(err));
  });
});

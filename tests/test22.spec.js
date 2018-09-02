import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('An assignment to a variable', function () {


  const input = `programa {

    funcao inicio() {
      inteiro a
      a = 5.5
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should respect the variable type`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      done(new Error('Should not have resolved'));
    }).catch( _ => {expect(1).toEqual(1);
    done();});
  });
});

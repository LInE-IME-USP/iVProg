import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('An assignment to a variable', function (done) {

  const input = `programa {

    funcao inicio() {
      inteiro a
      a = 5.5
    }
  }`;

  const lexer = Lexers['pt_br'];
  const result = 2;

  it(`should respect the variable type`, function () {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      done(new Error('Should not have resolved'));
    }).catch( _ => done());
  });
});

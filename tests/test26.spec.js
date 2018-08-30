import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('Assigning a line from a matrix to another vector', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a[2][2] = {{5,2},{8,6}}
      inteiro b[2] = a[0]
    }
  }`;

  const lexer = Lexers['pt_br'];

  it(`should result in a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('b')).toBeTruthy();
      done();
    }).catch( err => done(err));
  });
});

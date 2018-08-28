import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('Assigning an ID to another variable', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = 5
      inteiro b = a
    }
  }`;

  const lexer = Lexers['pt_br'];

  it(`should result in a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('b').value).toEqual(5);
      done();
    }).catch( err => done(err));
  });
});

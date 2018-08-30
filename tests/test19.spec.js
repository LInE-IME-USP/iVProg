import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('Multi(*) operation', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a
      a = -2 + 2 * 4 + 2
    }
  }`;

  const lexer = Lexers['pt_br'];

  it(`should have higher priority than Sum(+)`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').value).toEqual(8);
      done();
    }).catch( err => done(err));
  });
});

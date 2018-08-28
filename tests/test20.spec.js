import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('An Array initialization with expressions', function () {

  const input = `programa {

    funcao inicio() {
      inteiro a[2] = {2+2,3*5}
    }
  }`;

  const lexer = Lexers['pt_br'];
  const result = [4,15];

  it(`should produce a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect([sto.applyStore('a').value[0].value,sto.applyStore('a').value[1].value]).toEqual(result);
      done();
    });
  });
});

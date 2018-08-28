import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('A call to a function that returns a valid type', function (done) {

  const input = `programa {

    funcao inicio() {
      inteiro a = soma(1, 1)
    }

    funcao inteiro soma(inteiro a, inteiro b)
    {
      retorne a + b
    }
  }`;

  const lexer = Lexers['pt_br'];
  const result = 2;

  it(`should produce a valid state`, function () {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').value).toEqual(result);
      done();
    });
  });
});

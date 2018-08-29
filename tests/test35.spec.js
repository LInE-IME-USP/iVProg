import Lexers from './../grammar/';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('A recursive call', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = fib(3)
    }

    funcao inteiro fib(inteiro n) {
      se (n <= 0 ) {
        retorne 0
      } senao se (n == 1) {
        retorne 1
      } senao {
        retorne fib(n-1) + fib(n-2)
      }
    }
  }`;

  const lexer = Lexers['pt_br'];

  it(`should produce a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').value).toEqual(2);
      done();
    }).catch( err => done(err));
  });
});

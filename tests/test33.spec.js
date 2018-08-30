import Lexers from './../grammar/';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'

describe('A non-const global variable', function () {

  let input = `programa {

    inteiro a = 5

    funcao inicio() {
      escolha (a) {
        caso 0:
          a = a + 1
          pare
        caso 1:
          a = a + 2
        caso 2:
          a = a * 2
          pare
        caso contrario:
          a = 5 + 8
      }
    }
  }`;

  const lexer = Lexers['pt_br'];

  it(`should be modifiable inside a function`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').value).toEqual(13);
      done();
    }).catch( err => done(err));
  });
});

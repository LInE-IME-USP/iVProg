import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('A break command inside a switch..case', function () {

  let input = `programa {

    funcao inicio() {
      inteiro a = 0
      escolha (a) {
        caso 2:
          a = a + 1
          pare
        caso 1:
          a = a + 2
          pare
        caso 0:
          a = -5
          pare
        caso contrario:
          a = 5 + 8
      }
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();

  it(`should terminate it`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').value).toEqual(-5);
      done();
    }).catch( err => done(err));
  });
});

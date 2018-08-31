import Lexers from './../grammar/';
import {Types} from './../js/ast/types';
import { IVProgParser } from './../js/ast/ivprogParser';
import { IVProgProcessor} from './../js/processor/ivprogProcessor'
import { LanguageService } from '../js/services/languageService';

describe('A call to a function that returns a valid type', function () {

  const input = `programa {

    funcao inicio() {
      inteiro a = soma(1, 1)
    }

    funcao inteiro soma(inteiro a, inteiro b)
    {
      retorne a + b
    }
  }`;

  const lexer = LanguageService.getCurrentLexer();
  const result = 2;

  it(`should produce a valid state`, function (done) {
    const parser = new IVProgParser(input, lexer);
    const exec = new IVProgProcessor(parser.parseTree());
    exec.interpretAST().then(sto => {
      expect(sto.applyStore('a').value).toEqual(result);
      done();
    }).catch(err => done(err));
  });
});

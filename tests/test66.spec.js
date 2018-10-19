

import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';
import { OutputTest } from '../js/util/outputTest';
import { IVProgProcessor } from '../js/processor/ivprogProcessor';

describe('Non initialized matrix', function () {

  localStorage.setItem('ivprog.lang', 'pt');

  const code = `programa {

    funcao inicio() {
      cadeia a = "mustache"
      cadeia b = "mostacheh"
      escreva(editDist(a,b,comprimento(a), comprimento(b)))
    }
  
    funcao inteiro editDist(cadeia str1 , cadeia str2 , inteiro m ,inteiro n) {
      inteiro l = m + 1
      inteiro c = n + 1
      inteiro i, j
      inteiro mat[l][c]
      para(i = 0; i <= m; i = i + 1 ) {
        para(j = 0; j <= n; j = j + 1) {
          se (i==0) {
            mat[i][j] = j
          } senao se (j == 0) {
            mat[i][j] = i
          } senao se (char_at(str1, i-1) == char_at(str2, j-1)) {
            mat[i][j] = mat[i-1][j-1]
          } senao {
            mat[i][j] = 1 + Matematica.minimo({mat[i][j-1], mat[i-1][j], mat[i-1][j-1]})
          }
        }
      }
      retorne mat[m][n]
  } 
  
  }`

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();

  it(`should not throw an exception`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(sem.analyseTree());
    exec.registerOutput(out);
    exec.interpretAST().then(_ => {
      expect(out.list.length).toEqual(1);
      done()
    }).catch( err => done(err));
  });
});

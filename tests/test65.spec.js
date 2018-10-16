import { IVProgParser } from './../js/ast/ivprogParser';
import { SemanticAnalyser } from './../js/processor/semantic/semanticAnalyser';
import { LanguageService } from '../js/services/languageService';
import { OutputTest } from '../js/util/outputTest';
import { IVProgProcessor } from '../js/processor/ivprogProcessor';

describe('Non initialized vector/matrix', function () {

  const code = `programa {

    funcao inicio() {
      inteiro a[16] = {1,8,3,5,7,4,2,1,8,3,5,7,-4,2,10,-1}
      inteiro tam = numero_elementos(a) - 1
      inteiro i
      a = mergeSort(a, 0, tam)
      para(i = 0; i< tam; i = i + 1) {
        escreva(a[i] + ", ")
      }
      escreva(a[i])
    }
  
    funcao inteiro[] mergeSort(inteiro a[], inteiro init, inteiro end) {
      inteiro meio = (init + end) / 2
      inteiro size1 = meio - init + 1, size2 = end - meio, sizeF = end - init + 1
      inteiro p1[size1]
      inteiro p2[size2]
      inteiro f[sizeF]
      se (init < end) {
        p1 = mergeSort(a, init, meio)
        p2 = mergeSort(a, meio+1, end)
        f = merge(p1, p2)
        retorne f
      } senao {
        retorne { a[init] }
      }
    }
  
    funcao inteiro[] merge(inteiro p1[], inteiro p2[]) {
      inteiro lenp1 = numero_elementos(p1)
      inteiro lenp2 = numero_elementos(p2)
      inteiro sizeF = lenp1 + lenp2
      inteiro f[sizeF]
      inteiro i = 0, a = 0, b =0
      enquanto(i < lenp1+lenp2) {
        se(a < lenp1) {
          se(b <lenp2) {
            se(p1[a] <= p2[b]) {
              f[i] = p1[a]
              a = a + 1
              i = i + 1
            } senao {
              f[i] = p2[b]
              b = b + 1
              i = i + 1
            }
          } senao {
            enquanto(a < lenp1) {
              f[i] = p1[a]
              a = a + 1
              i = i + 1
            }
          }
        } senao {
          enquanto(b < lenp2) {
            f[i] = p2[b]
            b = b + 1
            i = i + 1
          }
        }
      }
      retorne f
    }
  }
  `;

  localStorage.setItem('ivprog.lang', 'pt');

  const lexer = LanguageService.getCurrentLexer();
  const out = new OutputTest();

  it(`should not throw an exception`, function (done) {
    const parser = new IVProgParser(code, lexer);
    const sem = new SemanticAnalyser(parser.parseTree());
    const exec = new IVProgProcessor(sem.analyseTree());
    exec.registerOutput(out);
    exec.interpretAST().then(_ => {
      expect(out.list.length).toEqual(16);
      done()
    }).catch( err => done(err));
  });
});

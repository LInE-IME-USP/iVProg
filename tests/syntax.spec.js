import Lexers from './../grammar/';
import {
    AnalisadorSintatico
} from './../js/asa/analisadorSintatico';
describe("Testing Syntax Analysis for default", () => {

    var input;

    var asa;
    var lexer;

    it("it should produce a valid tree", () => {
      lexer  = Lexers['pt_br'];
      input = `programa {
      const real PI
      const inteiro a[5][5]
      }`;

      asa = {
          global: [{
              isConst: true,
              tipo: 'real',
              id: 'PI',
              lines: null,
              columns: null,
              initial: null
          }, {
              isConst: true,
              tipo: 'int',
              id: 'a',
              lines: {
                  type: 'int',
                  value: 5
              },
              columns: {
                  type: 'int',
                  value: 5
              },
              initial: null
          }],
          functions: []
        };
        const as = new AnalisadorSintatico(input, lexer);
        expect(as.parseTree()).toEqual(asa);
    });
});
import {
    IVProgParser
} from './../js/ast/ivprogParser';
import { LanguageService } from '../js/services/languageService';


describe('For with no \';\'', () => {

    let input = `funcao inteiro test(real i) {
      inteiro a = 5 + i
      a = 5 + G[i][6]
      para (a = 0
        a < 5
        a = a + 1) {
         a = a
      }
    }`;
    const lexer = LanguageService.getCurrentLexer();

    it(`should result in SyntaxError`, () => {
        const as = new IVProgParser(input, lexer);
        const fun = as.parseFunction.bind(as);
        expect(fun).toThrow();
    });
});
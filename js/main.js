import {
    InputStream,
    CommonTokenStream
} from 'antlr4/index';
import { IVProgParser } from './ast/ivprogParser';
import Lexers from '../grammar/';

const lang = 'pt_br';

const ivprogLexer = Lexers[lang];

const input = `programa {

  const real PI = 5.5
  inteiro V = -10*2

  funcao inteiro test(real i) {
    escolha (i) {
      caso 1:
        retorne 0
      caso contrario:
        retorne 4
    }
  }
}`;

// const lexer = new ivprogLexer(new InputStream(input));
// const stream = new CommonTokenStream(lexer);
// stream.fill();
// let i = 1;
// let token = null;
// while ((token = stream.LT(i)).type !== ivprogLexer.EOF && token.type !== ivprogLexer.WHITESPACE) {
//     console.log(`${token.type}-${token.text}`);
//     console.log('\n')
//     i++;
// }
const anaSin = new IVProgParser(input, ivprogLexer);
try {
  console.log(anaSin.parseTree());
} catch(a) {
  console.log(a);
}
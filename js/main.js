import {
    InputStream,
    CommonTokenStream
} from 'antlr4/index';
import { IVProgParser } from './ast/ivprogParser';
import Lexers from '../grammar/';

const lang = 'pt_br';

const ivprogLexer = Lexers[lang];

const input = `programa {
        const inteiro a[1][1] = {
            {
              1,2,3
            },
            {
              1,3
            }
        }

        funcao inicio() {
            retorna
        }
    }`;
// let input2 = 'test = i\nb = i + 1\n';
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
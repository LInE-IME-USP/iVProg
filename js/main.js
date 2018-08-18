import {
    InputStream,
    CommonTokenStream
} from 'antlr4/index';
import { AnalisadorSintatico } from './asa/analisadorSintatico';
import Lexers from '../grammar/';

const lang = 'pt_br';

const ivprogLexer = Lexers[lang];

const input = `programa {
  const real PI
  inteiro i
  const inteiro a[5][5], b, c[i]

  funcao
  inteiro teste()
  {
    inteiro i[5];
  }

}`;
const lexer = new ivprogLexer(new InputStream(input));
const stream = new CommonTokenStream(lexer);
stream.fill();
let i = 1;
let token = null;
while ((token = stream.LT(i)).type !== ivprogLexer.EOF && token.type !== ivprogLexer.ESPACO) {
    console.log(`${token.type}-${token.text}`);
    console.log('\n')
    i++;
}
const anaSin = new AnalisadorSintatico(input, ivprogLexer);
try {
  console.log(anaSin.parseTree().global);
} catch(a) {
  console.log(a);
}

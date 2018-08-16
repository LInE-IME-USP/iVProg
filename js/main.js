import { InputStream, CommonTokenStream } from 'antlr4/index';
import Parsers from '../grammar/';

const lang = 'pt_br';

const ivprogParser = Parsers[lang];

const input = `programa {
  const real PI = 0x25ff
  funcao inteiro casa(inteiro a, inteiro[][] b) {
    cadeia s = "teste"
    escreva(s)
    se (a <= 5) {
      a = 10;
    }
    senao se (a > 5 E a < 10) {
      a = 15
    } senao {
      a = 20
    }
  }
}`;
const lexer = new ivprogParser(new InputStream(input));
const parser = new CommonTokenStream(lexer);
parser.fill();
let i = 1;
let token = null;
while((token = parser.LT(i)).type !== ivprogParser.EOF && token.type !== ivprogParser.ESPACO) {
  console.log(`${token.type}-${token.text}`);
  console.log('\n')
  i++;
}




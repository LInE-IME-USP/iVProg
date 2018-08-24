import {
    InputStream,
    CommonTokenStream
} from 'antlr4/index';
import * as Commands from './ast/commands';
import { IVProgParser } from './ast/ivprogParser';
import Lexers from '../grammar/';

const lang = 'pt_br';

const ivprogLexer = Lexers[lang];

const input = `programa {

  const real C = 6.8-5.8+1
             
  funcao abc() {
     inteiro a = 8
     se (a * C > 80) {
        a = 0
     } senao se(verdadeiro) {
        a = -1
       fun()
     }
  }

  funcao real fun() {
    retorne 3
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
  const data = anaSin.parseTree();
  console.log(data);
  var editor = new JsonEditor('#json-renderer', data);
  $('#btn').click( () => {
    const input = $('#input').val();
    const analiser = new IVProgParser(input, ivprogLexer);
    try {
      const data = analiser.parseTree();
      console.log(data);
      editor.load(data);  
    } catch (error) {
      alert(error);
    }
    
  });
} catch(a) {
  console.log(a);
}
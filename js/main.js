import {
    InputStream,
    CommonTokenStream
} from 'antlr4/index';
import * as Commands from './ast/commands';
import { IVProgParser } from './ast/ivprogParser';
import Lexers from '../grammar/';
import { IVProgProcessor } from './processor/ivprogProcessor';

const lang = 'pt_br';

const ivprogLexer = Lexers[lang];

const input = `programa {
             
  funcao inicio() {
     inteiro a[2] = {1,2}
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
const proc = new IVProgProcessor(anaSin.parseTree());
proc.interpretAST().then( sto => {
  console.log(sto.applyStore('a'));
}).catch(e => console.log(e));
// try {
//   const data = anaSin.parseTree();
//   console.log(data);
//   var editor = new JsonEditor('#json-renderer', data);
//   $('#btn').click( () => {
//     const input = $('#input').val();
//     const analiser = new IVProgParser(input, ivprogLexer);
//     try {
//       const data = analiser.parseTree();
//       console.log(data);
//       editor.load(data);  
//     } catch (error) {
//       alert(error);
//     }
    
//   });
// } catch(a) {
//   console.log(a);
// }
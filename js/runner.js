import { IVProgParser } from './ast/ivprogParser';
import { IVProgProcessor } from './processor/ivprogProcessor';
import {DOMInput} from './io/domInput';
import {DOMOutput} from './io/domOutput';
import { LanguageService } from './services/languageService';
import { LocalizedStrings } from './services/localizedStringsService';

export function runner () {
  const ivprogLexer = LanguageService.getCurrentLexer();
console.log(LocalizedStrings.getUI('start'));

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
// const anaSin = new IVProgParser(input, ivprogLexer);
const editor = new JsonEditor('#json-renderer', {});
const domIn = new DOMInput('#dom-in');
const domOut = new DOMOutput('#dom-out');
// proc.interpretAST().then( sto => {
//   console.log(sto.applyStore('a'));
// }).catch(e => console.log(e));
try {
  $('#btn').click( () => {
    const input = $('#input').val();
    const analiser = new IVProgParser(input, ivprogLexer);
    try {
      const data = analiser.parseTree();
      const proc = new IVProgProcessor(data);
      proc.registerInput(domIn);
      domOut.clear();
      proc.registerOutput(domOut);
      proc.interpretAST().then(sto => editor.load(sto.store))
        .catch( e => alert(e));
    } catch (error) {
      alert(error);
    }
    
  });
} catch(a) {
  console.log(a);
}
}
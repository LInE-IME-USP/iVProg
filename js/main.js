import { IVProgParser } from './ast/ivprogParser';
import { IVProgProcessor } from './processor/ivprogProcessor';
import { SemanticAnalyser } from "./processor/semantic/semanticAnalyser";
import {DOMInput} from './io/domInput';
import {DOMOutput} from './io/domOutput';
import { LanguageService } from './services/languageService';
import { LocalizedStrings } from './services/localizedStringsService';

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
      const semAna = new SemanticAnalyser(data);
      const proc = new IVProgProcessor(semAna.analyseTree());
      proc.registerInput(domIn);
      domOut.clear();
      proc.registerOutput(domOut);
      proc.interpretAST().then(_ => editor.load({}))
        .catch( e => {alert(e);console.log(e);});
    } catch (error) {
      alert(error);
      console.log(error);
    }
    
  });
} catch(a) {
  console.log(a);
}
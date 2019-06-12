import { IVProgParser } from './ast/ivprogParser';
import { IVProgProcessor } from './processor/ivprogProcessor';
import {DOMConsole} from './io/domConsole';
import { LanguageService } from './services/languageService';
import { SemanticAnalyser } from './processor/semantic/semanticAnalyser';

export function runner () {
const ivprogLexer = LanguageService.getCurrentLexer();


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
const domConsole = new DOMConsole("#console", true);
domConsole.hide();
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
      proc.registerInput(domConsole);
      domConsole.clear();
      proc.registerOutput(domConsole);
      proc.interpretAST().then(sto => editor.load(sto.store))
        .catch( e => {alert(e); console.log(e)});
    } catch (error) {
      alert(error);
      console.log(error);
    }
    
  });
} catch(a) {
  console.log(a);
}
}

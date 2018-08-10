import { InputStream, CommonTokenStream } from 'antlr4/index';
import { ivprogParser, ivprogLexer, ivprogVisitor, ivprogListener } from '../grammar/';

class MyVisitor extends ivprogVisitor {
 
  visitParse(ctx) { console.log(ctx);}

  visitPrograma(ctx) { console.log(ctx);}

  visitListaDeclaracoes(ctx) { console.log(ctx);}

  visitDeclaracoesGlobais(ctx) { console.log(ctx);}

}

class MyListener extends ivprogListener {
  enterParse(ctx) {
    console.log(ctx);
  }

  exitParse(ctx) {
    console.log(ctx);
  }
}

const list = new MyListener();
const vist = new MyVisitor();
const input = ""; // Load string content
const lexer = new ivprogLexer(new InputStream(input));
const parser = new ivprogParser(new CommonTokenStream(lexer));
const result = parser.parse().enterRule(list);
//const result = vist.visit(parser.programa());


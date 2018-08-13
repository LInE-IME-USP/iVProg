import { InputStream, CommonTokenStream } from 'antlr4/index';
import { pt_br } from '../grammar/';


const input = "casa = 35;"; // Load string content
const lexer = new pt_br(new InputStream(input));
const parser = new CommonTokenStream(lexer);
console.log(parser.getTokens(0, 3, null));
//const result = vist.visit(parser.programa());


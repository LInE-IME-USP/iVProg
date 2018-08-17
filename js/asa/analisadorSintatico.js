import { CommonTokenStream, InputStream } from 'antlr4/index';
import { SintaxError } from './SintaxError';

export class AnalisadorSintatico {

  constructor (input, lexerClass) {
    this.lexerClass = lexerClass;
    this.lexer = new lexerClass(new InputStream(input));
    this.tokenStream = new CommonTokenStream(this.lexer);
    this.tokenStream.fill();
    this.pos = 1;
    this.variableTypes = [this.lexerClass.PR_INTEIRO, this.lexerClass.PR_REAL, this.lexerClass.PR_LOGICO, this.lexerClass.PR_CADEIA];

  }

  parseTree () {
    return this.parseProgram();
  }

  getToken (index = null) {
    if(index === null)
      index = this.pos;
    return this.tokenStream.LT(index);
  }

  isEOF () {
    this.getToken(this.pos);
    return this.tokenStream.fetchedEOF;
  }

  parseProgram () {
    let token = this.getToken();

    if(this.lexerClass.PR_PROGRAMA === token.type) {
      this.pos++;
      this.consumeNewLines();
      this.checkOpenCurly();
      this.pos++;
      this.consumeNewLines();
      const globalVars = this.parseGlobalVariables();
      this.consumeNewLines();
      const functions = []; // this.parseFunctions();
      this.consumeNewLines();
      this.checkCloseCurly();
      this.pos++;
      this.consumeNewLines();
      if(!this.isEOF()) {
        throw new Error("No extra characters are allowed after 'program {...}'");
      }
      return {global: globalVars, functions: functions};
    } else {
      throw SintaxError.createError(this.lexer.literalNames[this.lexerClass.PR_PROGRAMA], token);
    }
  }

  checkOpenCurly () {
    let token = null;
    if(this.lexerClass.ABRE_CHA !== (token = this.getToken()).type){
      throw SintaxError.createError('{', token);
    }
  }

  checkCloseCurly () {
    let token = null;
    if(this.lexerClass.FECHA_CHA !== (token = this.getToken()).type){
      throw SintaxError.createError('}', token);
    }
  }

  /* It checks if the current token at position pos is a ']'.
  * As a check function it doesn't increment pos.
  *
  * @params bool:attempt, indicates that the token is optional. Defaults: false
  *
  * @returns true if the attempt is true and current token is '[',
  *   false is attempt is true and current token is not '['
  **/
  checkOpenBrace (attempt = false) {
    const token = this.getToken();
    if(this.lexerClass.ABRE_COL !== token.type){
      if (!attempt) {
        throw SintaxError.createError('[', token);
      } else {
        return false;
      }
    }
    return true;
  }

  checkCloseBrace (attempt = false) {
    const token = this.getToken();
    if(this.lexerClass.FECHA_COL !== token.type){
      if (!attempt) {
        throw SintaxError.createError(']', token);
      } else {
        return false;
      }
    }
    return true;
  } 

  parseGlobalVariables () {
    let vars = [];
    while(true) {
      const decl = this.parseHasConst();
      const eosToken = this.getToken();
      if (decl !== null && eosToken.type !== this.lexerClass.EOS) {
        throw SintaxError.createError('new line or \';\'', eosToken);
      }
      
      if (decl === null){
        break;
      } else {
        vars.concat(decl);
        this.pos++;
      }
    }
    return vars;
  }

  /*
  * Checks if the next token is PR_CONST. It's only available
  * at global variables declaration level
  * @returns Declararion(const, type, id, initVal?)
  **/
  parseHasConst () {
    const constToken = this.getToken();
    if(constToken.type === this.lexerClass.PR_CONST) {
      this.pos++;
      const typeToken = this.getToken();
      if(!this.isVariableType(typeToken)) {
        throw SintaxError.createError(this.getCommaTypeString(), typeToken);
      }
      this.pos++;;
      return this.parseDeclararion(typeToken, true);
    } else if(this.isVariableType(constToken)) {
      this.pos++;
      return this.parseDeclararion(constToken);
    } else {
      return null;
    }

  }

  /*
  * Parses a declarion of the form: type --- id --- (= --- EAnd)?
  * @returns Declararion(const, type, id, initVal?)
  **/
  parseDeclararion (typeToken, isConst = false) {
    const idToken = this.getToken();
    let initial = null;
    let dim1 = null;
    let dim2 = null;
    if(idToken.type !== this.lexerClass.ID) {
      throw SintaxError.createError('ID', idToken);
    }
    this.pos++;
    // Check for array or vector
    // ID[int/IDi][int/IDj]
    if (this.checkOpenBrace(true)) {
      this.pos++;
      dim1 = this.getArrayDimension();
      this.checkCloseBrace();
      this.pos++;
      if(this.checkOpenBrace(true)) {
        this.pos++;
        dim2 = this.getArrayDimension();
        this.checkCloseBrace();
        this.pos++;
      }
    }

    const equalsToken = this.getToken();
    if(equalsToken.type === this.lexerClass.ATRIBUICAO) {
      //process Expression(EAnd) => initial != null
      console.log("= founds");
    }

    const commaToken = this.getToken();
    if(commaToken.type === this.lexerClass.VIRGULA) {
      console.log("comma found");
      this.pos++;
      return [{
        isConst: isConst,
        tipo: typeToken.text,
        id: idToken.text,
        lines: dim1,
        columns: dim2,
        initial: initial
      }]
      .concat(this.parseDeclararion(typeToken, isConst));
    } else {
       return [{
        isConst: isConst,
        tipo: typeToken.text,
        id: idToken.text,
        lines: dim1,
        columns: dim2,
        initial: initial
      }]
    }
  }

  consumeNewLines () {
    let token = this.getToken();
    while(token.type === this.lexerClass.EOS && token.text.match('[\r\n]+')) {
      this.pos++;
      token = this.getToken();
    }
  }

  isVariableType (token) {
    return this.variableTypes.find(v => v === token.type);
  }

  /*
  * Reads the next token of the stream to check if it is a Integer or an ID.
  * @returns Integer | ID
  **/
  getArrayDimension () {
    const dimToken = this.getToken();
    if(dimToken.type !== this.lexerClass.INTEIRO && dimToken.type !== this.lexerClass.ID) {
      throw SintaxError.createError('int or ID', dimToken);
    }
    this.pos++;
    return dimToken.text;
  }

  getCommaTypeString () {
    return this.variableTypes.map( x => this.lexer.literalNames[x])
      .reduce((o, n) => {
        if (o.length <= 0)
          return n;
        else
          return o + ", " + n;
      }, '');
  }
}
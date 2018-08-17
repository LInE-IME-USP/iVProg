import { CommonTokenStream } from 'antlr4/index';
import { SintaxError } from './SintaxError';

class AnalisadorSintatico {

  constructor (lexer) {
    this.lexer = lexer;
    this.tokenStream = new CommonTokenStream(lexer);
    this.tokenStream.fill();
    this.pos = 1;
    this.variableTypes = [this.lexer.PR_INTEIRO, this.lexer.PR_REAL, this.lexer.PR_LOGICO, this.lexer.PR_CADEIA];

  }

  parseTree () {
    return {};
  }

  getToken (index = null) {
    if(index === null)
      index = this.pos;
    return this.tokenStream.LT(index);
  }

  parseProgram () {
    let token = null;

    if(this.lexer.PR_PROGRAMA === (token = this.getToken()).type) {
      try {
        this.pos++;
        this.consumeNewLines();
        checkOpenCurly();
        this.pos++;
        this.consumeNewLines();
        const globalVars = parseGlobalVariables();
        this.consumeNewLines();
        const functions = parseFunctions();
        this.consumeNewLines();
        checkCloseCurly();
        this.pos++;
        this.consumeNewLines();
        if(this.lexer.EOF !== (token = this.getToken()).type) {
          console.log("No extra characters are allowed after program {...}");
        }
        return {global: globalVars, functions: functions};

      } catch(SintaxError err) {
        console.log(err.message);
      }
    } else {
      throw SintaxError.createError(this.lexer.literalNames(this.lexer.PR_PROGRAMA), token);
    }
    return null;
  }

  checkOpenCurly () {
    let token = null;
    if(this.lexer.ABRE_CHA !== (token = this.getToken()).type){
      throw SintaxError.createError(this.getErrorString('{', token));
    }
  }

  checkCloseCurly () {
    let token = null;
    if(this.lexer.FECHA_CHA !== (token = this.getToken()).type){
      throw SintaxError.createError(this.getErrorString('}', token));
    }
  }

  checkOpenBrace (attempt = false) {
    const token = this.getToken();
    if(this.lexer.ABRE_COL !== token.type){
      if (!attempt) {
        throw SintaxError.createError(this.getErrorString('[', token));
      } else {
        return false;
      }
    }
    return true;
  }

  checkCloseBrace (attempt = false) {
    const token = this.getToken();
    if(this.lexer.FECHA_COL !== token.type){
      if (!attempt) {
        throw SintaxError.createError(this.getErrorString(']', token));
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
      if (eosToken.type !== this.lexer.EOS) {
        throw SintaxError.createError('new line or \';\'', eosToken);
      }
      this.pos++;
      if (decl === null)
        break;
      else
        vars.concat(decl);
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
    if(constToken.type === this.lexer.PR_CONST) {
      this.pos++;
      const typeToken = this.getToken();
      if(!this.isVariableType(typeToken)) {
        throw SintaxError.createError(this.getCommaTypeString(), typeToken);
      }
      this.pos++;;
      return parseDeclararion(typeToken, true);
    } else if(isVariableType(constToken)) {
      this.pos++;
      return parseDeclararion(constToken);
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
    if(idToken.type !== this.lexer.ID) {
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
    if(equalsToken.type === this.lexer.ATRIBUICAO) {
      //process Expression(EAnd) => initial != null
    }

    const commaToken = this.getToken();
    if(commaToken.type === this.lexer.VIRGULA) {
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
      return [{isConst: isConst, tipo: typeToken.text, id: idToken.text. initial: initial}];
    }
  }

  consumeNewLines () {
    token = this.getToken();
    while(token.type === this.lexer.EOS && token.text.match('[\r\n]')) {
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
    if(dimToken.type !== this.lexer.PR_INTEIRO || dimToken.type !== this.lexer.ID) {
      throw SintaxError.createError('int or ID', dimToken);
    }
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
import { CommonTokenStream, InputStream } from 'antlr4/index';
import { SyntaxError } from './SyntaxError';

export class AnalisadorSintatico {

  constructor (input, lexerClass) {
    this.lexerClass = lexerClass;
    this.lexer = new lexerClass(new InputStream(input));
    this.tokenStream = new CommonTokenStream(this.lexer);
    this.tokenStream.fill();
    this.pos = 1;
    this.variableTypes = [this.lexerClass.PR_INTEIRO,
      this.lexerClass.PR_REAL,
      this.lexerClass.PR_LOGICO,
      this.lexerClass.PR_CADEIA
    ];
    this.functionTypes = this.variableTypes.push(this.lexerClass.PR_VAZIO);
  }

  parseTree () {
    return this.parseProgram();
  }

  getToken (index = this.pos) {
    // if(index === null)
    //   index = this.pos;
    return this.tokenStream.LT(index);
  }

  isEOF () {
    this.getToken(this.pos);
    return this.tokenStream.fetchedEOF;
  }

  parseProgram () {
    const token = this.getToken();
    let globalVars = [];
    let functions = [];

    if(this.lexerClass.PR_PROGRAMA === token.type) {
      this.pos++;
      this.consumeNewLines();
      this.checkOpenCurly();
      this.pos++;
      while(true) {
        this.consumeNewLines();
        const token = this.getToken();
        if (token.type === this.lexerClass.PR_CONST || token.type === this.lexerClass.ID) {
          globalVars = globalVars.concat(this.parseGlobalVariables());
        } else if (token.type === this.lexerClass.PR_FUNCAO) {
          functions = functions.concat([]);
        } else {
          break;
        }
      }
      this.consumeNewLines();
      this.checkCloseCurly();
      this.pos++;
      this.consumeNewLines();
      if(!this.isEOF()) {
        throw new Error("No extra characters are allowed after 'program {...}'");
      }
      return {global: globalVars, functions: functions};
    } else {
      throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.PR_PROGRAMA], token);
    }
  }

  checkOpenCurly () {
    const token = this.getToken();
    if(this.lexerClass.ABRE_CHA !== token.type){
      throw SyntaxError.createError('{', token);
    }
  }

  checkCloseCurly () {
    const token = this.getToken();
    if(this.lexerClass.FECHA_CHA !== token.type){
      throw SyntaxError.createError('}', token);
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
        throw SyntaxError.createError('[', token);
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
        throw SyntaxError.createError(']', token);
      } else {
        return false;
      }
    }
    return true;
  }

  checkOpenParenthesis (attempt = false) {
    const token = this.getToken();
    if(this.lexerClass.ABRE_PAR !== token.type){
      if (!attempt) {
        throw SyntaxError.createError('(', token);
      } else {
        return false;
      }
    }
    return true;
  }

  checkCloseParenthesis (attempt = false) {
    const token = this.getToken();
    if(this.lexerClass.FECHA_PAR !== token.type){
      if (!attempt) {
        throw SyntaxError.createError(')', token);
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
        throw SyntaxError.createError('new line or \';\'', eosToken);
      }

      if (decl === null) {
        break;
      } else {
        vars = vars.concat(decl);
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
      const typeString = this.parseType();
      return this.parseDeclararion(typeString, true);
    } else if(this.isVariableType(constToken)) {
      this.pos++;
      return this.parseDeclararion(constToken);
    } else {
      return null;
    }

  }

  /*
  * Parses a declarion of the form: type --- id --- (= --- EAnd)?
  * @returns a list of Declararion(const, type, id, initVal?)
  **/
  parseDeclararion (typeString, isConst = false) {
    let initial = null;
    let dim1 = null;
    let dim2 = null;
    const idString = this.parseID();
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
      console.log("= found");
    }

    const commaToken = this.getToken();
    if(commaToken.type === this.lexerClass.VIRGULA) {
      console.log("comma found");
      this.pos++;
      return [{
        isConst: isConst,
        tipo: typeString,
        id: idString,
        lines: dim1,
        columns: dim2,
        initial: initial
      }]
      .concat(this.parseDeclararion(typeString, isConst));
    } else {
       return [{
        isConst: isConst,
        tipo: typeString,
        id: idString,
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
    if(dimToken.type === this.lexerClass.INTEIRO) {
      //parse as int literal
      this.pos++;
      return this.parseIntLiteral(dimToken);
    } else if(dimToken.type === this.lexerClass.ID) {
      //parse as variable
      this.pos++;
      return this.parseVariable(dimToken);
    } else {
      throw SyntaxError.createError('int or ID', dimToken);
    }
  }

  /*
  * Returns an object {type: 'int', value: value}.
  * It checks for binary and hexadecimal integers.
  * @returns object with fields type and value
  **/
  parseIntLiteral (token) {
    const text = token.text;
    let val = null;
    if(text.match('^0b|^0B')) {
      val = parseInt(text.substring(2), 2);
    } else if (text.match('^0x|^0X')) {
      val = parseInt(text.substring(2), 16);
    } else {
      val = parseInt(text);
    }
    return {type: 'int', value: val};
  }

  parseRealLiteral (token) {
    return {type: 'real', value: parseFloat(token.text)};
  }

  /*
  * Returns an object {type: 'variable', value: value}.
  * @returns object with fields type and value
  **/
  parseVariable (token) {
    return {type: 'variable', value: token.text};
  }

  parseFunctions () {
    let list = [];
    while(true) {
      const func = this.parseFunction();
      if(func === null)
        break;
      else
        list.push(func);
    }
    return list;
  }

  /*
  * Returns an object representing a function. It has
  * four attributes: returnType, id, formalParams and block.
  * The block object has two attributes: declarations and commands
  **/
  parseFunction () {
    let formalParams = [];
    const token = this.getToken();
    if(token.type !== this.lexerClass.PR_FUNCAO) {
      //throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.PR_FUNCAO], token);
      return null;
    }
    this.pos++;
    this.consumeNewLines();
    const returnType = this.parseType(true);
    this.consumeNewLines();
    const functionID = this.parseID();
    this.consumeNewLines();
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    if (!this.checkCloseParenthesis(true)) {
      formalParams = this.parseFormalParameters(); // formal parameters 
      this.consumeNewLines();
      this.checkCloseParenthesis();
      this.pos++;
    } else {
      this.pos++;
    }
    this.consumeNewLines();
    const commandsBlock = this.parseFunctionBody();
    return {returnType: returnType, id: functionID, formalParams: formalParams, block: commandsBlock};
  }

  /*
  * Parse the formal parameters of a function.
  * @returns a list of objects with the following attributes: type, id and dimensions.
  **/
  parseFormalParameters () {
    const list = [];
    while(true) {
      let dimensions = 0;
      this.consumeNewLines();
      const typeString = this.parseType();
      this.pos++;
      this.consumeNewLines();
      const idString = this.parseID();
      this.pos++;
      this.consumeNewLines();
      if (this.checkOpenBrace(true)) {
        this.pos++;
        dimensions++;
        this.checkCloseBrace();
        this.pos++;
        if(this.checkOpenBrace(true)) {
          this.pos++;
          dimensions++;
          this.checkCloseBrace();
          this.pos++;
        }
      }
      list.push({type: typeString, id: idString, dimensions: dimensions});
      this.consumeNewLines();
      const commaToken = this.getToken();
      if (commaToken.type !== this.lexerClass.VIRGULA)
        break;
      this.pos++;
    }
    return list;
  }

  parseID () {
    const token = this.getToken();
    if(token.type !== this.lexerClass.ID) {
      throw SyntaxError.createError('ID', token);
    }
    this.pos++;
    return token.text;
  }

  parseType (isFunction = false) {
    const token = this.getToken();
    if(token.type === this.lexerClass.ID && isFunction) {
      return 'void';
    } else if (token.type === this.lexerClass.PR_VAZIO && isFunction) {
      this.pos++;
      return 'void';
    } else if (this.isVariableType(token)) {
      this.pos++;
      switch(token.type) {
        case this.lexerClass.PR_INTEIRO:
          return 'int';
        case this.lexerClass.PR_LOGICO:
          return 'logic';
        case this.lexerClass.PR_REAL:
          return 'real';
        case this.lexerClass.PR_CADEIA:
          return 'string';
        default:
          break;
      }
    }
    
    throw SyntaxError.createError(this.getTypesAsString(isFunction), token);
  }

  parseFunctionBody () {
    let variablesDecl = [];
    this.checkOpenCurly();
    this.pos++;
    while(true) {
      this.consumeNewLines();
      const token = this.getToken();
      if (isVariableType(token)) {
        this.pos++;
        variablesDecl = variablesDecl.concat(this.parseDeclararion(token));
      } else if (token.type === this.lexerClass.ID) {
        this.pos++;
        const equalOrParenthesis = this.getToken();
        if (equalOrParenthesis.type === this.lexerClass.ATRIBUICAO) {

        } else if (equalOrParenthesis.type === this.lexerClass.ABRE_PAR) {

        } else {
          throw SyntaxError.createError("= or (", equalOrParenthesis);
        }
      }
    }
  }

  getTypesAsString (isFunction = false) {
    const types = isFunction ? this.functionTypes : this.variableTypes;
    return types.map( x => this.lexer.literalNames[x])
      .reduce((o, n) => {
        if (o.length <= 0)
          return n;
        else
          return o + ", " + n;
      }, '');
  }
}
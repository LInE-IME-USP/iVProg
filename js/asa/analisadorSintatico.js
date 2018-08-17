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

  getToken (index=null) {
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
        parseOpenCurly();
        this.pos++;
        this.consumeNewLines();
        const globalVars = parseGlobalVariables();
        this.consumeNewLines();
        const functions = parseFunctions();
        this.consumeNewLines();
        parseCloseCurly();
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
      console.log(this.getErrorString(this.lexer.literalNames(this.lexer.PR_PROGRAMA), token));
    }
    return null;
  }

  parseOpenCurly () {
    let token = null;
    if(this.lexer.ABRE_CHA !== (token = this.getToken()).type){
      throw new SintaxError(this.getErrorString('{', token));
    }
  }

  parseCloseCurly () {
    let token = null;
    if(this.lexer.FECHA_CHA !== (token = this.getToken()).type){
      throw new SintaxError(this.getErrorString('}', token));
    }
  }

  parseGlobalVariables () {
    let vars = [];
    while(true) {
      const decl = this.parseHasConst();
      const eosToken = this.getToken();
      if(eosToken.type !== this.lexer.EOS) {
        throw new SintaxError('new line or \';\'', eosToken);
      }
      this.pos++;
      if ( decl === null)
        break;
      else
        vars.push(decl);
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
      return parseDeclararion(true, typeToken);
    } else if(isVariableType(constToken)) {
      return parseDeclararion();
    } else {
      return null;
    }

  }

  /*
  * Parses a declarion of the form: type --- id --- (= --- EAnd)?
  * @returns Declararion(const, type, id, initVal?)
  **/
  parseDeclararion (isConst = false) {
    const typeToken = this.getToken();
    if(!this.isVariableType(typeToken)) {
      throw SintaxError.createError(this.getCommaTypeString(), typeToken);
    }
    this.pos++;
    const idToken = this.getToken();
    if(idToken.type !== this.lexer.ID) {
      throw SintaxError.createError('ID', idToken);
    }
    this.pos++;
    const equalsToken = this.getToken();
    if(equalsToken.type === this.lexer.ATRIBUICAO) {
      //process Expression
    } else {
      return {isConst: isConst, tipo: typeToken.text, id: idToken.text};
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
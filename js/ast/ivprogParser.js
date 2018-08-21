import { CommonTokenStream, InputStream } from 'antlr4/index';
import { ArrayAccess, FunctionCall} from './expressions/';
import { Return, Break, Atribution } from './commands/';
import { SyntaxError } from './SyntaxError';

export class IVProgParser {

  constructor (input, lexerClass) {
    this.lexerClass = lexerClass;
    this.lexer = new lexerClass(new InputStream(input));
    this.tokenStream = new CommonTokenStream(this.lexer);
    this.tokenStream.fill();
    this.pos = 1;
    this.variableTypes = [this.lexerClass.RK_INTEGER,
      this.lexerClass.RK_REAL,
      this.lexerClass.RK_BOOLEAN,
      this.lexerClass.RK_STRING
    ];
    this.functionTypes = this.variableTypes.concat(this.lexerClass.RK_VOID);
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

    if(this.lexerClass.RK_PROGRAM === token.type) {
      this.pos++;
      this.consumeNewLines();
      this.checkOpenCurly();
      this.pos++;
      while(true) {
        this.consumeNewLines();
        const token = this.getToken();
        if (token.type === this.lexerClass.RK_CONST || token.type === this.lexerClass.ID) {
          globalVars = globalVars.concat(this.parseGlobalVariables());
        } else if (token.type === this.lexerClass.RK_FUNCTION) {
          functions = functions.concat(this.parseFunctions());
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
      throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.RK_PROGRAM], token);
    }
  }

  checkOpenCurly () {
    const token = this.getToken();
    if(this.lexerClass.OPEN_CURLY !== token.type){
      throw SyntaxError.createError('{', token);
    }
  }

  checkCloseCurly () {
    const token = this.getToken();
    if(this.lexerClass.CLOSE_CURLY !== token.type){
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
    if(this.lexerClass.OPEN_BRACE !== token.type){
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
    if(this.lexerClass.CLOSE_BRACE !== token.type){
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
    if(this.lexerClass.OPEN_PARENTHESIS !== token.type){
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
    if(this.lexerClass.CLOSE_PARENTHESIS !== token.type){
      if (!attempt) {
        throw SyntaxError.createError(')', token);
      } else {
        return false;
      }
    }
    return true;
  }

  checkEOS (attempt = false)  {
    const eosToken = this.getToken();
    if (eosToken.type !== this.lexerClass.EOS) {
      if (!attempt)
        throw SyntaxError.createError('new line or \';\'', eosToken);
      else
        return false;
    }
    return true;
  }

  parseGlobalVariables () {
    let vars = [];
    while(true) {
      const decl = this.parseMaybeConst();
      const eosToken = this.getToken();
      if (decl !== null) {
        this.checkEOS();
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
  parseMaybeConst () {
    const constToken = this.getToken();
    if(constToken.type === this.lexerClass.RK_CONST) {
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
      this.consumeNewLines();
      dim1 = this.parseArrayDimension();
      this.consumeNewLines();
      this.checkCloseBrace();
      this.pos++;
      if(this.checkOpenBrace(true)) {
        this.pos++;
        this.consumeNewLines();
        dim2 = this.parseArrayDimension();
        this.consumeNewLines();
        this.checkCloseBrace();
        this.pos++;
      }
    }

    const equalsToken = this.getToken();
    if(equalsToken.type === this.lexerClass.EQUAL) {
      this.pos++;
      initial = this.parseExpressionOR();
    }
    
    const commaToken = this.getToken();
    if(commaToken.type === this.lexerClass.COMMA) {
      console.log("comma found");
      this.pos++;
      this.consumeNewLines();
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
  parseArrayDimension () {
    const dimToken = this.getToken();
    if(dimToken.type === this.lexerClass.INTEGER) {
      //parse as int literal
      this.pos++;
      return this.getIntLiteral(dimToken);
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
  getIntLiteral (token) {
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

  getRealLiteral (token) {
    return {type: 'real', value: parseFloat(token.text)};
  }

  getStringLiteral (token) {
    const text = token.text;
    let valor = text.replace("\\b", "\b");
    valor = valor.replace("\\t", "\t");
    valor = valor.replace("\\n", "\n");
    valor = valor.replace("\\r", "\r");
    valor = valor.replace("\\\"", "\"");
    valor = valor.replace("\\\'", "\'");
    valor = valor.replace("\\\\", "\\");
    return {type: 'string', value: valor};
  }

  getBoolLiteral (token) {
    const val = token.type === this.lexerClass.RK_True ? true : false;
    return {type: 'bool', value: val};
  }

  parseArrayLiteral () {

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
    if(token.type !== this.lexerClass.RK_FUNCTION) {
      //throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.PR_FUNCAO], token);
      return null;
    }
    this.pos++;
    const returnType = this.parseType(true);
    const functionID = this.parseID();
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
      const typeString = this.parseType();
      const idString = this.parseID();
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
      const commaToken = this.getToken();
      if (commaToken.type !== this.lexerClass.COMMA)
        break;
      this.pos++;
      this.consumeNewLines();
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
    } else if (token.type === this.lexerClass.RK_VOID && isFunction) {
      this.pos++;
      return 'void';
    } else if (this.isVariableType(token)) {
      this.pos++;
      switch(token.type) {
        case this.lexerClass.RK_INTEGER:
          return 'int';
        case this.lexerClass.RK_LOGIC:
          return 'bool';
        case this.lexerClass.RK_REAL:
          return 'real';
        case this.lexerClass.RK_STRING:
          return 'string';
        default:
          break;
      }
    }
    
    throw SyntaxError.createError(this.getTypesAsString(isFunction), token);
  }

  parseFunctionBody () {
    let variablesDecl = [];
    let commands = [];
    this.checkOpenCurly();
    this.pos++;
    this.consumeNewLines();
    while(true) {
      const token = this.getToken();
      let cmd = null;
      if (this.isVariableType(token)) {
        this.pos++;
        variablesDecl = variablesDecl.concat(this.parseDeclararion(token));
        this.checkEOS();
        this.pos++;
        cmd = -1;
      } else if (token.type === this.lexerClass.ID) {
        cmd = this.parseIDCommand();
      } else if (token.type === this.lexerClass.RK_RETURN) {
        cmd = this.parseReturn();
      } else if (token.type === this.lexerClass.RK_WHILE) {

      } else if (token.type === this.lexerClass.RK_FOR) {

      } else if (token.type === this.lexerClass.RK_BREAK) {
        cmd = this.parseBreak();
      } else if (token.type === this.lexerClass.RK_SWITCH) {
        
      } else if (token.type === this.lexerClass.RK_DO) {
        
      } else if (token.type === this.lexerClass.RK_IF) {
        
      }

      if (cmd === null)
        break;
      if(cmd !== -1)
        commands.push(cmd);
    }
    this.consumeNewLines();
    this.checkCloseCurly();
    this.pos++;
    this.consumeNewLines();
    return {variables: variablesDecl, commands: commands};
  }

  parseBreak () {
    this.pos++;
    this.checkEOS();
    this.pos++;
    return (new Break());
  }

  parseReturn () {
    this.pos++;
    const exp = this.parseExpressionOR();
    this.checkEOS();
    this.pos++;
    return (new Return(exp));
  }

  parseIDCommand () {
    const id = this.parseID();
    const equalOrParenthesis = this.getToken();
    if (equalOrParenthesis.type === this.lexerClass.EQUAL) {
      this.pos++
      const exp = this.parseExpressionOR();
      this.checkEOS();
      this.pos++;
      return (new Atribution(id, exp));
    } else if (equalOrParenthesis.type === this.lexerClass.OPEN_PARENTHESIS) {
      const actualParameters = this.parseActualParameters();
      this.checkEOS();
      this.pos++;
      return (new FunctionCall(id, actualParameters));
    } else {
      throw SyntaxError.createError("= or (", equalOrParenthesis);
    }
  }

  /*
  * Parses an Expression following the structure:
  *
  * EOR  => EAnd ( 'or' EOR)? #expression and
  *
  * EOR   => ENot ('and' EOR)? #expression or
  *
  * ENot  => 'not'? ER #expression not
  *
  * ER    => E ((>=, <=, ==, >, <) E)? #expression relational
  *
  * E     => factor ((+, -) E)? #expression
  *
  * factor=> term ((*, /, %) factor)?
  *
  * term  => literal || arrayAccess || FuncCall || ID || '('EAnd')'
  **/
  parseExpressionOR () {
    const exp1 = this.parseExpressionAND();
    let exp2 = null;
    let or = null;
    const maybeAnd = this.getToken();
    if (maybeAnd.type === this.lexerClass.OR_OPERATOR) {
      this.pos++;
      or = 'or';
      this.consumeNewLines();
      exp2 = this.parseExpressionOR();
    }

    return {left: exp1, op:or, right: exp2};
  }

  parseExpressionAND () {
    const exp1 = this.parseExpressionNot();
    let and = null;
    let exp2 = null;
    const andToken = this.getToken();
    if (andToken.type === this.lexerClass.AND_OPERATOR) {
      this.pos++;
      and = 'and';
      this.consumeNewLines();
      exp2 = this.parseExpressionAND();
    }

    return {left: exp1, op: and, right: exp2};
  }

  parseExpressionNot () {
    let not = null;
    const maybeNotToken = this.getToken();
    if (maybeNotToken.type === this.lexerClass.NOT_OPERATOR) {
      this.pos++;
      not = 'not';
    }
    const eRel = this.parseExpressionRel();

    return {left: null, op: not, right: eRel};
  }

  parseExpressionRel () {
    const exp1 = this.parseExpression();
    let rel = null;
    let exp2 = null;
    const relToken = this.getToken();
    if(relToken.type === this.lexerClass.RELATIONAL_OPERATOR) {
      this.pos++;
      rel = relToken.text; // TODO: source code line/column information
      exp2 = this.parseExpression();
    }

    return {left: exp1, op: rel, right: exp2};
  }

  parseExpression () {
    const factor = this.parseFactor();
    let op = null;
    let exp = null;
    const sumOpToken = this.getToken();
    if(sumOpToken.type === this.lexerClass.SUM_OP) {
      this.pos++;
      op = sumOpToken.text; // TODO: source code line/column information
      exp = this.parseExpression();
    }

    return {left: factor, op: op, right: exp};
  }

  parseFactor () {
    const term = this.parseTerm();
    let op = null;
    let factor = null;
    const multOpToken = this.getToken();
    if(multOpToken.type === this.lexerClass.MULTI_OP) {
      this.pos++;
      op = multOpToken.text; // TODO: source code line/column information
      factor = this.parseFactor();
    }

    return {left: term, op: op, right: factor};
  }

  parseTerm () {
    const token = this.getToken();
    switch(token.type) {
      case this.lexerClass.INTEGER:
        this.pos++;
        return this.getIntLiteral(token);
      case this.lexerClass.REAL:
        this.pos++;
        return this.getRealLiteral(token);
      case this.lexerClass.STRING:
        this.pos++;
        return this.getStringLiteral(token);
      case this.lexerClass.RK_TRUE:
      case this.lexerClass.RK_FALSE:
        this.pos++;
        return this.getBoolLiteral(token);
      case this.lexerClass.ID:
        return this.parseIDTerm();
      case this.lexerClass.OPEN_PARENTHESIS:
        return this.parseParenthesisExp();
    }
  }

  parseIDTerm () {
    const id = this.parseID();
    const last = this.pos;
    if(this.checkOpenBrace(true)) {
      this.pos++;
      const firstIndex = this.parseExpression();
      let secondIndex = null;
      this.consumeNewLines();
      this.checkCloseBrace();
      this.pos++;
      if(this.checkOpenBrace(true)){
        this.pos++;
        secondIndex = this.parseExpression();
        this.consumeNewLines();
        this.checkCloseBrace();
        this.pos++;
      } else {
        this.pos--;
      }

      return new ArrayAccess(id, firstIndex, secondIndex);

    } else if (this.checkOpenParenthesis(true)) {
      this.pos++;
      this.consumeNewLines();
      let actualParameters = [];
      if(!this.checkCloseParenthesis(true)) {
        actualParameters = this.parseActualParameters();
        this.consumeNewLines();
        this.checkCloseParenthesis();
        this.pos++;
      } else {
        this.pos++;
      }
      return new FunctionCall(id, actualParameters);
    } else {
      this.pos = last;
      return id;
    }
  }

  parseParenthesisExp () {
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    const exp = this.parseExpressionOR();
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    return exp;
  }

  parseActualParameters () {
    this.checkOpenParenthesis();
    this.pos++;
    list = [];
    while (true) {
      this.consumeNewLines();
      const exp = this.parseExpressionOR();
      list.push(exp);
      const commaToken = this.getToken();
      if (commaToken.type !== this.lexerClass.COMMA) {
        break;
      } else {
        this.pos++;
        this.consumeNewLines();
      }  
    }
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    return list;
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
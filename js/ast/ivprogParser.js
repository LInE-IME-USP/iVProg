import { CommonTokenStream, InputStream } from 'antlr4/index';
import * as Expressions from './expressions/';
import * as Commands from './commands/';
import { Types } from './types';
import { SyntaxError } from './SyntaxError';

export class IVProgParser {

  // <BEGIN scope consts>
  static get BASE () {
    return 0;
  }
  static get FUNCTION () {
    return 1;
  }
  static get COMMAND () {
    return 2;
  }
  static get BREAKABLE () {
    return 4;
  }
  // </ END scope consts>

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
    this.parsingArrayDimension = 0;
    this.scope = [];
  }

  parseTree () {
    return this.parseProgram();
  }

  getToken (index = this.pos) {
    // if(index === null)
    //   index = this.pos;
    return this.tokenStream.LT(index);
  }

  insideScope (scope) {
    if(this.scope.length <= 0) {
      return IVProgParser.BASE === scope;
    } else {
      return this.scope[this.scope.length-1] === scope;
    }
  }

  pushScope (scope) {
    this.scope.push(scope);
  }

  popScope () {
    return this.scope.pop();
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
        if (token.type === this.lexerClass.RK_CONST || this.isVariableType(token)) {
          globalVars = globalVars.concat(this.parseGlobalVariables());
        } else if (token.type === this.lexerClass.RK_FUNCTION) {
          functions = functions.concat(this.parseFunction());
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

  checkOpenCurly (attempt = false) {
    const token = this.getToken();
    if(this.lexerClass.OPEN_CURLY !== token.type){
      if(!attempt)
        throw SyntaxError.createError('{', token);
      else
        return false;
    }
    return true;
  }

  checkCloseCurly (attempt = false) {
    const token = this.getToken();
    if(this.lexerClass.CLOSE_CURLY !== token.type){
      if(!attempt)
        throw SyntaxError.createError('}', token);
      else
        return false;
    }
    return true;
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

  consumeForSemiColon () {
    const eosToken = this.getToken();
    if (eosToken.type === this.lexerClass.EOS && eosToken.text.match(';')) {
      this.pos++;
      return;  
    }
    throw SyntaxError.createError(';', eosToken);
  }

  parseGlobalVariables () {
    const decl = this.parseMaybeConst();
    const eosToken = this.getToken();
    this.checkEOS();
    this.pos++;
    return decl;
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
      return this.parseDeclaration(typeString, true);
    } else if(this.isVariableType(constToken)) {
      const typeString = this.parseType();
      return this.parseDeclaration(typeString);
    } else {
      throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.RK_CONST] + ' or ' + this.getTypesAsString(), constToken);
    }

  }

  /*
  * Parses a declarion of the form: type --- id --- (= --- EAnd)?
  * @returns a list of Declararion(const, type, id, initVal?)
  **/
  parseDeclaration (typeString, isConst = false) {
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
    let declaration = null;
    if (dim1 !== null) {
      declaration = new Commands.ArrayDeclaration(idString,
        typeString, dim1, dim2, initial, isConst);
    } else {
      declaration = new Commands.Declaration(idString, typeString, initial, isConst);
    }
    const commaToken = this.getToken();
    if(commaToken.type === this.lexerClass.COMMA) {
      console.log("comma found");
      this.pos++;
      this.consumeNewLines();
      return [declaration]
      .concat(this.parseDeclaration(typeString, isConst));
    } else {
       return [declaration]
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
    return new Expressions.IntLiteral(val);
  }

  getRealLiteral (token) {
    return new Expressions.RealLiteral(parseFloat(token.text));
  }

  getStringLiteral (token) {
    const text = token.text;
    let value = text.replace("\\b", "\b");
    value = value.replace("\\t", "\t");
    value = value.replace("\\n", "\n");
    value = value.replace("\\r", "\r");
    value = value.replace("\\\"", "\"");
    value = value.replace("\\\'", "\'");
    value = value.replace("\\\\", "\\");
    return new Expressions.StringLiteral(value);
  }

  getBoolLiteral (token) {
    const val = token.type === this.lexerClass.RK_TRUE ? true : false;
    return new Expressions.BoolLiteral(val);
  }

  parseArrayLiteral () {
    this.checkOpenCurly();
    const beginArray = this.getToken();
    if (this.parsingArrayDimension >= 2) {
      // TODO: better error message
      throw new Error(`Array dimensions exceed maximum size of 2 at line ${beginArray.line}`);
    }
    this.pos++;
    this.parsingArrayDimension++;
    this.consumeNewLines();
    const data = this.parseExpressionList();
    this.consumeNewLines();
    this.checkCloseCurly()
    this.pos++;
    this.parsingArrayDimension--;
    if (this.parsingArrayDimension === 0) {
      if (!data.isValid) {
      // TODO: better error message
      console.log('invalid array');
      throw new Error(`Invalid array at line ${beginArray.line}`);
    }
    }
    return new Expressions.ArrayLiteral(data);
  }

  /*
  * Returns an object {type: 'variable', value: value}.
  * @returns object with fields type and value
  **/
  parseVariable (token) {
    return new Expressions.VariableLiteral(token.text);
  }

  /*
  * Returns an object representing a function. It has
  * four attributes: returnType, id, formalParams and block.
  * The block object has two attributes: declarations and commands
  **/
  parseFunction () {
    this.pushScope(IVProgParser.FUNCTION);
    let formalParams = [];
    const token = this.getToken();
    if(token.type !== this.lexerClass.RK_FUNCTION) {
      //throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.PR_FUNCAO], token);
      return null;
    }
    this.pos++;
    const returnType = this.parseType();
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
    const commandsBlock = this.parseCommandBlock();
    const func = new Commands.Function(functionID, returnType, formalParams, commandsBlock);
    if (functionID === null && !func.isMain) {
      // TODO: better error message
      throw new Error(`Function ${this.lexerClass.MAIN_FUNCTION_NAME} must return void (line ${token.line})`);
    }
    this.popScope();
    return func;
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
    if (this.insideScope(IVProgParser.FUNCTION)) {
      if (token.text === this.lexerClass.MAIN_FUNCTION_NAME){
        return null;
      }
    }
    return token.text;
  }

  parseType () {
    const token = this.getToken();
    if(token.type === this.lexerClass.ID && this.insideScope(IVProgParser.FUNCTION)) {
      return Types.VOID;
    } else if (token.type === this.lexerClass.RK_VOID && this.insideScope(IVProgParser.FUNCTION)) {
      this.pos++;
      return Types.VOID;
    } else if (this.isVariableType(token)) {
      this.pos++;
      switch(token.type) {
        case this.lexerClass.RK_INTEGER:
          return Types.INTEGER;
        case this.lexerClass.RK_LOGIC:
          return Types.BOOLEAN;
        case this.lexerClass.RK_REAL:
          return Types.REAL;
        case this.lexerClass.RK_STRING:
          return Types.STRING;
        default:
          break;
      }
    }
    
    throw SyntaxError.createError(this.getTypesAsString(), token);
  }

  parseCommandBlock (optionalCurly = false) {
    let variablesDecl = [];
    const commands = [];
    let hasOpen = false;
    if (this.checkOpenCurly(optionalCurly)) {
      this.pos++;
      hasOpen = true;
    }
    this.consumeNewLines();
    while(true) {

      const cmd = this.parseCommand();
      if (cmd === null)
        break;
      if(cmd !== -1) {
        if (cmd instanceof Array) {
          variablesDecl = variablesDecl.concat(cmd);
        } else {
          commands.push(cmd);
        }
      }
    }
    this.consumeNewLines();
    if (hasOpen) {
      this.checkCloseCurly()
      this.pos++;
      this.consumeNewLines();
    }
    return new Commands.CommandBlock(variablesDecl, commands);
  }

  parseCommand () {
    const token = this.getToken();
    if (this.isVariableType(token)) {
      if(!this.insideScope(IVProgParser.FUNCTION)) {
        // TODO better error message
        throw new Error(`Cannot declare variable here (line ${token.line})`);
      }
      this.pushScope(IVProgParser.BASE);
      const varType = this.parseType();
      this.popScope();
      const cmd = this.parseDeclaration(varType);
      this.checkEOS();
      this.pos++;
      return cmd;
    } else if (token.type === this.lexerClass.ID) {
      return this.parseIDCommand();
    } else if (token.type === this.lexerClass.RK_RETURN) {
      return this.parseReturn();
    } else if (token.type === this.lexerClass.RK_WHILE) {
      return this.parseWhile();
    } else if (token.type === this.lexerClass.RK_FOR) {
      return this.parseFor();
    } else if (token.type === this.lexerClass.RK_BREAK ) {
      if(!this.insideScope(IVProgParser.BREAKABLE)) {
        // TODO better error message
        throw new Error("Break cannot be used outside of a loop.");
      }
      return this.parseBreak();
    } else if (token.type === this.lexerClass.RK_SWITCH) {
      return this.parseSwitchCase();
    } else if (token.type === this.lexerClass.RK_DO) {
      return this.parseDoWhile();
    } else if (token.type === this.lexerClass.RK_IF) {
      return this.parseIfThenElse();
    } else if (this.checkEOS(true)){
      this.pos++;
      return -1;
    } else {
      return null;
    }
  }

  parseSwitchCase () {
    this.pushScope(IVProgParser.BREAKABLE);
    this.pos++;
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    const exp = this.parseExpressionOR();
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    this.consumeNewLines();
    this.checkOpenCurly();
    this.pos++;
    this.consumeNewLines();
    const casesList = this.parseCases();
    this.consumeNewLines();
    this.checkCloseCurly();
    this.pos++;
    this.consumeNewLines();

    this.popScope();
    return new Commands.Switch(casesList);
  }

  parseDoWhile () {
    this.pos++;
    this.consumeNewLines();
    this.pushScope(IVProgParser.BREAKABLE);
    const commandsBlock = this.parseCommandBlock();
    this.consumeNewLines(); //Maybe not...
    const whileToken = this.getToken();
    if (whileToken.type !== this.lexerClass.RK_WHILE) {
      throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.RK_WHILE], whileToken);
    }
    this.pos++;
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    const condition = this.parseExpressionOR();
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    this.checkEOS();
    this.popScope();
    return new Commands.DoWhile(condition, commandsBlock);
  }

  parseIfThenElse () {
    if(this.insideScope(IVProgParser.BREAKABLE)) {
      this.pushScope(IVProgParser.BREAKABLE);
    } else {
      this.pushScope(IVProgParser.COMMAND);
    }
    this.pos++;
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    const logicalExpression = this.parseExpressionOR();
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    this.consumeNewLines();
    const cmdBlocks = this.parseCommandBlock();

    const maybeElse = this.getToken();
    if(maybeElse.type === this.lexerClass.RK_ELSE) {
      this.pos++;
      this.consumeNewLines();
      const maybeIf = this.getToken();
      let elseBlock = null;
      if(this.checkOpenCurly(true)) {
        elseBlock = this.parseCommandBlock();
      } else if(maybeIf.type === this.lexerClass.RK_IF) {
        elseBlock = this.parseIfThenElse();
      } else {
        // TODO better error message
        throw SyntaxError.createError(`${this.lexer.literalNames[this.lexerClass.RK_IF]} or {`, maybeIf);
      }
      return new Commands.IfThenElse(logicalExpression, cmdBlocks, elseBlock);
    }
    this.popScope();

    return new Commands.IfThenElse(logicalExpression, cmdBlocks, null);
  }

  parseFor () {
    this.pushScope(IVProgParser.BREAKABLE);
    this.pos++;
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    const attribution = this.parseForAssign();
    this.consumeNewLines();
    const condition = this.parseExpressionOR();
    this.consumeForSemiColon();
    const increment = this.parseForAssign(true);
    this.checkCloseParenthesis()
    this.pos++;
    this.consumeNewLines();
    const commandsBlock = this.parseCommandBlock();
    this.popScope();
    return new Commands.For(attribution, condition, increment, commandsBlock);
  }

  parseWhile () {
    this.pushScope(IVProgParser.BREAKABLE);
    this.pos++;
    this.checkOpenParenthesis();
    this.pos++;
    this.consumeNewLines();
    const logicalExpression = this.parseExpressionOR();
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    this.consumeNewLines();
    const cmdBlocks = this.parseCommandBlock();
    this.popScope();
    return new Commands.While(logicalExpression, cmdBlocks);
  }

  parseBreak () {
    this.pos++;
    this.checkEOS();
    this.pos++;
    return (new Commands.Break());
  }

  parseReturn () {
    this.pos++;
    let exp = null;
    if(!this.checkEOS(true)) {
      exp = this.parseExpressionOR();
      this.checkEOS();
    }
    this.pos++;
    return new Commands.Return(exp);
  }

  parseIDCommand () {
    const id = this.parseID();
    const equalOrParenthesis = this.getToken();
    if (equalOrParenthesis.type === this.lexerClass.EQUAL) {
      this.pos++
      const exp = this.parseExpressionOR();
      this.checkEOS();
      this.pos++;
      return (new Commands.Assign(id, exp));
    } else if (equalOrParenthesis.type === this.lexerClass.OPEN_PARENTHESIS) {
      const actualParameters = this.parseActualParameters();
      this.checkEOS();
      this.pos++;
      return (new Expressions.FunctionCall(id, actualParameters));
    } else {
      throw SyntaxError.createError("= or (", equalOrParenthesis);
    }
  }

  parseForAssign (isLast = false) {
    if(!isLast)
      this.consumeNewLines();
    if(this.checkEOS(true)) {
      return null;
    }
    const id = this.parseID();
    const equal = this.getToken();
    if (equal.type !== this.lexerClass.EQUAL) {
      throw SyntaxError.createError('=', equal);
    }
    this.pos++
    const exp = this.parseExpressionOR();
    if(!isLast) {
      this.consumeForSemiColon();
    }
    return new Commands.Assign(id, exp);
  }

  parseCases () {
    const token = this.getToken();
    if(token.type !== this.lexerClass.RK_CASE) {
      throw SyntaxError.createError(this.lexer.literalNames[this.lexerClass.RK_CASE], token);
    }
    this.pos++;
    const nextToken = this.getToken();
    if(nextToken.type === this.lexerClass.RK_DEFAULT) {
      this.pos++;
      const colonToken = this.getToken();
      if (colonToken.type !== this.lexerClass.COLON) {
        throw SyntaxError.createError(':', colonToken);
      }
      this.pos++;
      this.consumeNewLines();
      const block = this.parseCommandBlock(true);
      const defaultCase = new Commands.Case(null);
      defaultCase.setCommands(block.commands);
      return [defaultCase];
    } else {
      const exp = this.parseExpressionOR();
      const colonToken = this.getToken();
      if (colonToken.type !== this.lexerClass.COLON) {
        throw SyntaxError.createError(':', colonToken);
      }
      this.pos++;
      this.consumeNewLines();
      const block = this.parseCommandBlock(true);
      const aCase = new Commands.Case(exp);
      aCase.setCommands(block.commands);
      const caseToken = this.getToken();
      if(caseToken.type === this.lexerClass.RK_CASE) {
        return [aCase].concat(this.parseCases());
      } else {
        return [aCase];
      }
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
    const maybeAnd = this.getToken();
    if (maybeAnd.type === this.lexerClass.OR_OPERATOR) {
      this.pos++;
      const or = 'or';
      this.consumeNewLines();
      const exp2 = this.parseExpressionOR();
      return new Expressions.InfixApp(or, exp1, exp2);
    }
    return exp1;
  }

  parseExpressionAND () {
    const exp1 = this.parseExpressionNot();
    const andToken = this.getToken();
    if (andToken.type === this.lexerClass.AND_OPERATOR) {
      this.pos++;
      const and = 'and';
      this.consumeNewLines();
      const exp2 = this.parseExpressionAND();
      return new Expressions.InfixApp(and, exp1, exp2);
    }
    return exp1;
  }

  parseExpressionNot () {
    const maybeNotToken = this.getToken();
    if (maybeNotToken.type === this.lexerClass.NOT_OPERATOR) {
      this.pos++;
      const not = 'not';
      const exp1 = this.parseExpressionRel();
      return new Expressions.UnaryApp(not, exp1);
    } else {
      return this.parseExpressionRel();
    }
  }

  parseExpressionRel () {
    const exp1 = this.parseExpression();
    const relToken = this.getToken();
    if(relToken.type === this.lexerClass.RELATIONAL_OPERATOR) {
      this.pos++;
      const rel = relToken.text; // TODO: source code line/column information
      const exp2 = this.parseExpression();
      return new Expressions.InfixApp(rel, exp1, exp2);
    }
    return exp1;
  }

  parseExpression () {
    const factor = this.parseFactor();
    const sumOpToken = this.getToken();
    if(sumOpToken.type === this.lexerClass.SUM_OP) {
      this.pos++;
      const op = sumOpToken.text; // TODO: source code line/column information
      const exp = this.parseExpression();
      return new Expressions.InfixApp(op, factor, exp);
    }
    return factor;
  }

  parseFactor () {
    const term = this.parseTerm();
    const multOpToken = this.getToken();
    if(multOpToken.type === this.lexerClass.MULTI_OP) {
      this.pos++;
      const op = multOpToken.text; // TODO: source code line/column information
      const factor = this.parseFactor();
      return new Expressions.InfixApp(op, term, factor);
    }
    return term;
  }

  parseTerm () {
    const token = this.getToken();
    switch(token.type) {
      case this.lexerClass.SUM_OP:
        this.pos++;
        return new Expressions.UnaryApp(token.text, this.parseTerm());
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
      case this.lexerClass.OPEN_CURLY:
        return this.parseArrayLiteral();
      case this.lexerClass.ID:
        return this.parseIDTerm();
      case this.lexerClass.OPEN_PARENTHESIS:
        return this.parseParenthesisExp();
      default:
        throw SyntaxError.createError('Terminal', token);
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

      return new Expressions.ArrayAccess(id, firstIndex, secondIndex);

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
      return new Expressions.FunctionCall(id, actualParameters);
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
    if(this.checkCloseParenthesis(true)) {
      this.pos++;
      return [];
    }
    this.consumeNewLines();
    const list = this.parseExpressionList();
    this.consumeNewLines();
    this.checkCloseParenthesis();
    this.pos++;
    return list;
  }

  parseExpressionList () {
    const list = [];
    while(true) {
      const exp = this.parseExpressionOR();
      list.push(exp);
      const maybeToken = this.getToken();
      if (maybeToken.type !== this.lexerClass.COMMA) {
        break;
      } else {
        this.pos++;
        this.consumeNewLines();
      }
    }
    return list;
  }

  getTypesAsString () {
    const types = this.insideScope(IVProgParser.FUNCTION) ? this.functionTypes : this.variableTypes;
    return types.map( x => this.lexer.literalNames[x])
      .reduce((o, n) => {
        if (o.length <= 0)
          return n;
        else
          return o + ", " + n;
      }, '');
  }
}
import { ProcessorErrorFactory } from './../error/processorErrorFactory';
import { LanguageDefinedFunction } from './../definedFunctions';
import { LanguageService } from './../../services/languageService';
import { ArrayDeclaration } from '../../ast/commands';
import { InfixApp, UnaryApp, FunctionCall, IntLiteral, RealLiteral, StringLiteral, BoolLiteral, VariableLiteral, ArrayLiteral } from '../../ast/expressions';
import { Literal } from '../../ast/expressions/literal';
import { resultTypeAfterInfixOp, resultTypeAfterUnaryOp } from '../compatibilityTable';
import { Types } from '../../ast/types';

export class SemanticAnalyser {

  constructor(ast) {
    this.ast = ast;
    this.lexerClass = LanguageService.getCurrentLexer();
    const lexer = new this.lexerClass(null);
    this.literalNames = lexer.literalNames;
    this.symbolMap = null;
  }

  pushMap () {
    if(this.symbolMap === null) {
      this.symbolMap = {map:{}, next: null};
    } else {
      const n = {map:{}, next: this.symbolMap};
      this.symbolMap = n;
    }
  }

  popMap () {
    if(this.symbolMap !== null) {
      this.symbolMap = this.symbolMap.next;
    }
  }

  insertSymbol (id, typeInfo) {
    this.symbolMap.map[id] = typeInfo;
  }

  findSymbol (id, symMap) {
    if(!symMap.map[id]) {
      if(symMap.next) {
        return this.findSymbol(id, symMap.next);
      }
      throw new Error("variable not defined");
    } else {
      return symMap.map[id];
    }
  }

  findFunction (name) {
    if(name.match(/^\$.+$/)) {
      const fun = LanguageDefinedFunction[name];
      if(!!!fun) {
        throw new Error("!!!Internal Error. Language defined function not implemented -> " + name + "!!!");
      }
      return fun;
    } else {
      const val = this.ast.functions.find( v => v.name === name);
      if (!!!val) {
        // TODO: better error message;
        throw new Error(`Function ${name} is not defined.`);
      }
      return val;
    }
  }

  analyseTree () {
    const globalVars = this.ast.global;
    this.pushMap();
    this.assertDeclarations(globalVars);
    const functions = this.ast.functions;
    for (let i = 0; i < functions.length; i++) {
      const fun = functions[i];
      this.assertFunction(fun);
    }
    return this.ast;
  }

  assertDeclarations (list) {
    for (let i = 0; i < list.length; i++) {
      this.assertDeclaration(list[i]);
    }
  }

  assertDeclaration (declaration) {
    if (declaration instanceof ArrayDeclaration) {
      if(declaration.initial === null) {
        this.insertSymbol(declaration.id, {id: declaration.id, lines: declaration.lines, columns: declaration.columns, type: declaration.type, subtype: declaration.subtype});
        return;
      }
      this.evaluateArrayLiteral(declaration.lines, declaration.columns, declaration.subtype, declaration.initial);
      this.insertSymbol(declaration.id, {id: declaration.id, lines: declaration.lines, columns: declaration.columns, type: declaration.type, subtype: declaration.subtype});

    } else {
      if(declaration.initial === null) {
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type});
        return;
      }
      const resultType = this.evaluateExpressionType(declaration.initial);
      if(declaration.type !== resultType) {
        throw new Error('Invalid type');
      } else {
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type})
      }
    }
  }

  evaluateExpressionType (expression) {
    if(expression instanceof UnaryApp) {
      const op = expression.op;
      const resultType = this.evaluateExpressionType(expression.left);
      return resultTypeAfterUnaryOp(op, resultType);
    } else if (expression instanceof InfixApp) {
      const op = expression.op;
      const resultTypeLeft = this.evaluateExpressionType(expression.left);
      const resultTypeRight = this.evaluateExpressionType(expression.right);
      return resultTypeAfterInfixOp(op, resultTypeLeft, resultTypeRight);
    } else if (expression instanceof Literal) {
      return this.evaluateLiteralType(expression);
    } else if (expression instanceof FunctionCall) {
      const fun = this.findFunction(expression.id);
      if (fun.returnType === Types.VOID) {
        throw new Error("void return");
      }
      this.assertParameters(fun, expression.actualParameters);
      return fun.returnType;
    }
  }

  evaluateLiteralType (literal) {
    if(literal instanceof IntLiteral) {
      return literal.type;
    } else if (literal instanceof RealLiteral) {
      return literal.type;
    } else if (literal instanceof StringLiteral) {
      return literal.type;
    } else if (literal instanceof BoolLiteral) {
      return literal.type;
    } else if (literal instanceof VariableLiteral) {
      const typeInfo = this.findSymbol(literal.id, this.symbolMap);
      if (typeInfo.type === Types.ARRAY) {
        return typeInfo;
      }
      return typeInfo.type;
    } else {
      console.warn("Evaluating type only for an array literal...");
      return Types.UNDEFINED;
    }
  }

  evaluateArrayLiteral (lines, columns, subtype, literal) {
    if (literal instanceof ArrayLiteral) {
      if (columns === null) {
        // it's a vector...
        if (lines !== literal.value.length) {
          throw new Error("invalid array size");
        }
        literal.value.reduce((last, next) => {
          const eType = this.evaluateExpressionType(next);
          if (subtype !== eType || eType !== last) {
            throw new Error("invalid array type");
          }
          return eType;
        });
        return true;
      } else {
        if (columns !== literal.value.length) {
          throw new Error("invalid array size");
        }
        for (let i = 0; i < columns; i++) {
          const anotherArray = literal.value[i];
          this.evaluateArrayLiteral(lines, null, subtype, anotherArray)
        }
      }

    } else {

      const resultType = this.evaluateExpressionType(literal);
      if (!resultType.subtype) {
        throw new Error("initial must be of type array");
      }
      if (resultType.subtype !== subtype) {
        throw new Error("invalid array type");
      } else if (resultType.lines !== lines) {
        throw new Error("invalid array size");
      } else if (resultType.columns !== columns) {
        throw new Error("invalid array size");
      }
      return true;

    }
  }

  assertFunction (fun) {
    this.pushMap();
    this.assertDeclarations(fun.variablesDeclarations);
    if(fun.returnType === Types.VOID) {
      this.assertOptionalReturn(fun);
    } else {
      this.assertReturn(fun);
    }
    this.popMap();
  }

  assertOptionalReturn (fun) {

  }

  assertParameters (fun, actualParametersList) {
    if (fun.formalParameters.length !== actualParametersList.length) {
      throw new Error("wrong number of parameters...");
    }
    for (let i = 0; i < actualParametersList.length; i++) {
      const param = actualParametersList[i];
      const formalParam = fun.formalParameters[i];
      if(formalParam.byRef && !(param instanceof VariableLiteral)) {
        throw new Error("Invalid param type");
      }
      const resultType = this.evaluateExpressionType(param);
      switch (formalParam.dimensions) {
        case 1: {
          if (!resultType.subtype) {
            throw new Error("invalid param type");   
          } else if (resultType.subtype !== formalParam.type) {
            throw new Error("invalid param type");   
          } else if (resultType.lines === null || resultType.columns !== null) {
            throw new Error("invalid param type");
          }
          break;
        }
        case 2: {
          if (!resultType.subtype) {
            throw new Error("invalid param type");   
          } else if (resultType.subtype !== formalParam.type) {
            throw new Error("invalid param type");   
          } else if (resultType.lines === null || resultType.columns === null) {
            throw new Error("invalid param type");
          }
          break;
        }
        default: {
          if (resultType.subtype || resultType !== formalParam.type) {
            throw new Error("invalid param type");
          }
          break;
        }
      }
    }
  }
}

import { ProcessorErrorFactory } from './../error/processorErrorFactory';
import { LanguageDefinedFunction } from './../definedFunctions';
import { LanguageService } from './../../services/languageService';
import { ArrayDeclaration, While, For, Switch, Case, Declaration, Assign, Break, IfThenElse, Return } from '../../ast/commands';
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
    const mainFunc = functions.filter((f) => f.name === null);
    if (mainFunc.length <= 0) {
      throw new Error("no main func...");
    } else if (mainFunc.length > 1) {
      throw new Error("only one main func...");
    }
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
        const lineType = this.evaluateExpressionType(declaration.lines);
        if (lineType !== Types.INTEGER) {
          throw new Error("dim must be int");
        }
        if (declaration.columns !== null) {
          const columnType = this.evaluateExpressionType(declaration.columns);
          if (columnType !== Types.INTEGER) {
            throw new Error("dim must be int");
          }
        }
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
        const dimType = this.evaluateExpressionType(lines);
        if (dimType !== Types.INTEGER) {
          throw new Error("dim must be int");
        }
        if ((lines instanceof IntLiteral) && lines.value !== literal.value.length) {
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
        const dimType = this.evaluateExpressionType(columns);
        if (dimType !== Types.INTEGER) {
          throw new Error("dim must be int");
        }
        if ((columns instanceof IntLiteral) && columns.value !== literal.value.length) {
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
    const optional = fun.returnType === Types.VOID;
    const valid = this.assertReturn(fun, optional);
    if (!valid) {
      throw new Error("function has no accessible return");
    }
    this.popMap();
  }

  assertReturn (fun, optional) {
    return fun.commands.reduce(
      (last, next) => this.checkCommand(fun.returnType, next, optional) || last, optional
    );
  }

  checkCommand (type, cmd, optional) {
    if (cmd instanceof While) {
      const resultType = this.evaluateExpressionType(cmd.expression);
      if (resultType !== Types.BOOLEAN) {
        throw new Error("condition not boolean");
      }
      this.checkCommands(type, cmd.commands, optional);
      return false;
    } else if (cmd instanceof For) {
      this.checkCommand(type, cmd.assignment, optional);
      const resultType = this.evaluateExpressionType(cmd.condition);
      if (resultType !== Types.BOOLEAN) {
        throw new Error("condition not boolean");
      }
      this.checkCommand(type, cmd.increment, optional);
      this.checkCommands(type, cmd.commands, optional);
      return false;
    } else if (cmd instanceof Switch) {
      const sType = this.evaluateExpressionType(cmd.expression);
      let result = optional;
      let hasDefault = false;
      for (let i = 0; i < cmd.cases.length; i++) {
        const aCase = cmd.cases[i];
        if (aCase.expression !== null) {
          const caseType = this.evaluateExpressionType(aCase.expression);
          if (sType !== caseType) {
            throw new Error("invalid type in case");
          }
        } else {
          hasDefault = true;
        }
        result = result && this.checkCommands(type, aCase.commands, result);        
      }
      return result && hasDefault;

    } else if (cmd instanceof Assign) {
      const typeInfo = this.findSymbol(cmd.id, this.symbolMap);
      const exp = cmd.expression;
      if(exp instanceof ArrayLiteral) {
        if(!typeInfo.subtype) {
          throw new Error("type not compatible");
        }
        this.evaluateArrayLiteral(typeInfo.lines, typeInfo.columns, typeInfo.subtype, exp);
      } else {
        if(typeInfo.subtype) {
          throw new Error("type not compatible");
        }
        const resultType = this.evaluateExpressionType(exp);
        if(resultType !== typeInfo.type) {
          throw new Error("type not compatible");
        }
      }
      return optional;
    } else if (cmd instanceof Break) {
      return optional;
    } else if (cmd instanceof IfThenElse) {
      const resultType = this.evaluateExpressionType(cmd.condition);
      if (resultType !== Types.BOOLEAN) {
        throw new Error("condition not boolean");
      }
      console.log(cmd);
      if(cmd.ifFalse instanceof IfThenElse) {
        return this.checkCommands(type, cmd.ifTrue.commands, optional) && this.checkCommand(type, cmd.ifFalse, optional);
      } else {
        return this.checkCommands(type, cmd.ifTrue.commands, optional) && this.checkCommands(type, cmd.ifFalse.commands,optional);
      }

    } else if (cmd instanceof FunctionCall) {
      const fun = this.findFunction(cmd.id);
      this.assertParameters(fun, cmd.actualParameters);
      return optional;
    } else if (cmd instanceof Return) {
      if (cmd.expression === null && type !== Types.VOID) {
        throw new Error('invalid return type');
      } else if (cmd.expression !== null) {
        const resultType = this.evaluateExpressionType(cmd.expression);
        if (resultType !== type) {
          throw new Error('invalid return type');
        } else {
          return true;
        }
      } else {
        return true;
      }
    }
  }

  checkCommands (type, cmds, optional) {
    return cmds.reduce(
      (last, next) => this.checkCommand(type, next, optional) || last, optional
    );
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
          if (resultType.subtype) {
            throw new Error("invalid param type");
          }
          if (formalParam.type !== Types.ALL && resultType !== formalParam.type) {
            throw new Error("invalid param type");
          }
          break;
        }
      }
    }
  }
}

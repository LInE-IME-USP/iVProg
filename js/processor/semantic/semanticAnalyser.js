import { ProcessorErrorFactory } from './../error/processorErrorFactory';
import { LanguageDefinedFunction } from './../definedFunctions';
import { LanguageService } from './../../services/languageService';
import { ArrayDeclaration, While, For, Switch, Case, Declaration, Assign, Break, IfThenElse, Return, ArrayIndexAssign } from '../../ast/commands';
import { InfixApp, UnaryApp, FunctionCall, IntLiteral, RealLiteral, StringLiteral, BoolLiteral, VariableLiteral, ArrayLiteral, ArrayAccess } from '../../ast/expressions';
import { Literal } from '../../ast/expressions/literal';
import { resultTypeAfterInfixOp, resultTypeAfterUnaryOp } from '../compatibilityTable';
import { Types } from '../../typeSystem/types';
import { CompoundType } from '../../typeSystem/compoundType';
import { MultiType } from '../../typeSystem/multiType';

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
      throw new Error("variable not defined "+id);
    } else {
      return symMap.map[id];
    }
  }

  getMainFunction () {
    return this.ast.functions.find(v => v.isMain);
  }

  findFunction (name) {
    if(name.match(/^\$.+$/)) {
      const fun = LanguageDefinedFunction.getFunction(name);
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
        if (!lineType.isCompatible(Types.INTEGER)) {
          throw new Error("dim must be int");
        }
        if (declaration.columns !== null) {
          const columnType = this.evaluateExpressionType(declaration.columns);
          if (!columnType.isCompatible(Types.INTEGER)) {
            throw new Error("dim must be int");
          }
        }
        this.insertSymbol(declaration.id, {id: declaration.id, lines: declaration.lines, columns: declaration.columns, type: declaration.type});
        return;
      }
      this.evaluateArrayLiteral(declaration.lines, declaration.columns, declaration.type, declaration.initial);
      this.insertSymbol(declaration.id, {id: declaration.id, lines: declaration.lines, columns: declaration.columns, type: declaration.type});

    } else {
      if(declaration.initial === null) {
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type});
        return;
      }
      const resultType = this.evaluateExpressionType(declaration.initial);
      if(resultType instanceof MultiType) {
        if(!resultType.isCompatible(declaration.type)) {
          throw new Error('Invalid type');  
        }
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type})
      } else if(!declaration.type.isCompatible(resultType)) {
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
      if (expression.isMainCall) {
        throw new Error("void return used in expression");
      }
      const fun = this.findFunction(expression.id);
      if (fun.returnType.isCompatible(Types.VOID)) {
        throw new Error("void return used in expression");
      }
      this.assertParameters(fun, expression.actualParameters);
      return fun.returnType;
    } else if (expression instanceof ArrayAccess) {
      const arrayTypeInfo = this.findSymbol(expression.id, this.symbolMap);
      if (!(arrayTypeInfo.type instanceof CompoundType)) {
        throw new Error("it's not an array");
      }
      const lineType = this.evaluateExpressionType(expression.line);
      if (!lineType.isCompatible(Types.INTEGER)) {
        throw new Error("line must be integer");
      }
      if (expression.column !== null) {
        if (arrayTypeInfo.columns === null) {
          throw new Error("it's not a matrix");
        }
        const columnType = this.evaluateExpressionType(expression.column);
        if(!columnType.isCompatible(Types.INTEGER)) {
          throw new Error("column must be integer");
        }
      }
      const arrType = arrayTypeInfo.type;
      if(expression.column !== null) {
        // indexing matrix
        return arrType.innerType;
      } else {
        if(arrayTypeInfo.columns === null) {
          return arrType.innerType;
        }
        return new CompoundType(arrType.innerType, 1);
      }
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
      if (typeInfo.type instanceof CompoundType) {
        return typeInfo.type;
      }
      return typeInfo.type;
    } else {
      console.warn("Evaluating type only for an array literal...");
      let last = null;
      if(literal.value.length === 1) {
        last = this.evaluateExpressionType(literal.value[0]);
      } else {
        for (let i = 0; i < literal.value.length; i++) {
          const e = this.evaluateExpressionType(literal.value[i]);
          if(last === null) {
            last = e;
          } else if(!last.isCompatible(e)) {
            throw new Error("invalid value type for array");
          } else {
            last = e;
          }
        }
      }
      if(last instanceof CompoundType) {
        return new CompoundType(last.innerType, last.dimensions + 1);
      }
      return new CompoundType(last, 1);
    }
  }

  evaluateArrayLiteral (lines, columns, type, literal) {
    if (literal instanceof ArrayLiteral) {
      if (columns === null) {
        // it's a vector...
        const dimType = this.evaluateExpressionType(lines);
        if (!dimType.isCompatible(Types.INTEGER)) {
          throw new Error("dim must be int");
        }
        if ((lines instanceof IntLiteral) && !lines.value.eq(literal.value.length)) {
          throw new Error("invalid array size");
        }
        literal.value.reduce((last, next) => {
          const eType = this.evaluateExpressionType(next);
          if (!last.canAccept(eType)) {
            throw new Error("invalid value type for array");
          }
          return last;
        }, type);
        return true;
      } else {
        const dimType = this.evaluateExpressionType(columns);
        if (!dimType.isCompatible(Types.INTEGER)) {
          throw new Error("dim must be int");
        }
        if ((columns instanceof IntLiteral) && !columns.value.eq(literal.value.length)) {
          throw new Error("invalid array size");
        }
        for (let i = 0; i < columns; i++) {
          const anotherArray = literal.value[i];
          this.evaluateArrayLiteral(lines, null, type, anotherArray)
        }
      }

    } else {

      const resultType = this.evaluateExpressionType(literal);
      if (!(resultType instanceof CompoundType)) {
        throw new Error("initial must be of type array");
      }
      if (!type.isCompatible(resultType)) {
        throw new Error("invalid array type");
      }
      return true;

    }
  }

  assertFunction (fun) {
    this.pushMap();
    fun.formalParameters.forEach(formalParam => {
      if(formalParam.type instanceof CompoundType) {
        if(formalParam.type.dimensions > 1) {
          this.insertSymbol(formalParam.id, {id: formalParam.id, lines: -1, columns: -1, type: formalParam.type});
        } else {
          this.insertSymbol(formalParam.id, {id: formalParam.id, lines: -1, columns: null, type: formalParam.type});
        }
      } else {
        this.insertSymbol(formalParam.id, {id: formalParam.id, type: formalParam.type});
      }
    })
    this.assertDeclarations(fun.variablesDeclarations);
    const optional = fun.returnType.isCompatible(Types.VOID);
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
      if (!resultType.isCompatible(Types.BOOLEAN)) {
        throw new Error("condition not boolean");
      }
      this.checkCommands(type, cmd.commands, optional);
      return false;
    } else if (cmd instanceof For) {
      this.checkCommand(type, cmd.assignment, optional);
      const resultType = this.evaluateExpressionType(cmd.condition);
      if (!resultType.isCompatible(Types.BOOLEAN)) {
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
          if (!sType.isCompatible(caseType)) {
            throw new Error("invalid type in case");
          }
        } else {
          hasDefault = true;
        }
        result = result && this.checkCommands(type, aCase.commands, result);        
      }
      return result && hasDefault;

    } else if (cmd instanceof ArrayIndexAssign) {
      const typeInfo = this.findSymbol(cmd.id, this.symbolMap);
      if(!(typeInfo.type instanceof CompoundType)) {
        throw new Error(cmd.id + " is not an array.");
      }
      const exp = cmd.expression;
      const lineExp = cmd.line;
      const lineType = this.evaluateExpressionType(lineExp);
      if (!lineType.isCompatible(Types.INTEGER)) {
        throw new Error("array dimension must be of type int");
      }
      const columnExp = cmd.column;
      if (typeInfo.columns === null && columnExp !== null) {
        throw new Error(cmd.id + " is not a matrix");
      } else if (columnExp !== null) {
        const columnType = this.evaluateExpressionType(columnExp);
        if (!columnType.isCompatible(Types.INTEGER)) {
          throw new Error("array dimension must be of type int");
        }
      }
      // exp can be a arrayLiteral, a single value exp or an array access
      if(exp instanceof ArrayLiteral) {
        this.evaluateArrayLiteral(typeInfo.lines, (columnExp ? typeInfo.columns : null), typeInfo.type, exp);
      } else {
        // cannot properly evaluate since type system is poorly constructed
      }
      return optional;
    } else if (cmd instanceof Assign) {
      const typeInfo = this.findSymbol(cmd.id, this.symbolMap);
      const exp = cmd.expression;
      if(exp instanceof ArrayLiteral) {
        if(!(typeInfo.type instanceof CompoundType)) {
          throw new Error("type not compatible");
        }
        this.evaluateArrayLiteral(typeInfo.lines, typeInfo.columns, typeInfo.type, exp);
      } else {
        const resultType = this.evaluateExpressionType(exp);
        if(!resultType.isCompatible(typeInfo.type)) {
          throw new Error("type not compatible");
        }
      }
      return optional;
    } else if (cmd instanceof Break) {
      return optional;
    } else if (cmd instanceof IfThenElse) {
      const resultType = this.evaluateExpressionType(cmd.condition);
      if (!resultType.isCompatible(Types.BOOLEAN)) {
        throw new Error("condition not boolean");
      }
      if(cmd.ifFalse instanceof IfThenElse) {
        return this.checkCommands(type, cmd.ifTrue.commands, optional) && this.checkCommand(type, cmd.ifFalse, optional);
      } else {
        return this.checkCommands(type, cmd.ifTrue.commands, optional) && this.checkCommands(type, cmd.ifFalse.commands,optional);
      }

    } else if (cmd instanceof FunctionCall) {
      let fun = null;
      if (cmd.isMainCall) {
        fun = this.getMainFunction();
      } else {
        fun = this.findFunction(cmd.id);
      }
      this.assertParameters(fun, cmd.actualParameters);
      return optional;
    } else if (cmd instanceof Return) {
      if (cmd.expression === null && !type.isCompatible(Types.VOID)) {
        throw new Error('invalid return type');
      } else if (cmd.expression !== null) {
        const resultType = this.evaluateExpressionType(cmd.expression);
        if (!type.isCompatible(resultType)) {
          console.log(resultType);
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
      if(formalParam.byRef) {
        if (!(param instanceof VariableLiteral || param instanceof ArrayAccess)) {
          throw new Error("Invalid param type for ref");
        }
      }
      const resultType = this.evaluateExpressionType(param);
      if(resultType instanceof MultiType && formalParam.type instanceof MultiType) {
        let shared = 0
        for (let j = 0; j < resultType.types.length; j++) {
          const element = resultType.types[j];
          if(formalParam.type.types.indexOf(element) !== -1) {
            shared++;
          }
        }
        if(shared <= 0) {
          throw new Error(`Parameter ${formalParam.id} is not compatible with the value given.`);
        }
      } else if (resultType instanceof MultiType) {
        if(!resultType.isCompatible(formalParam.type)) {
          throw new Error(`Parameter ${formalParam.id} is not compatible with the value given.`);
        }
      } else if(!formalParam.type.isCompatible(resultType)) {
        console.log("####");
        console.log(resultType);
        console.log("####");
        console.log(formalParam.type);
        throw new Error(`Parameter ${formalParam.id} is not compatible with the value given.`);
      }

    }
  }
}

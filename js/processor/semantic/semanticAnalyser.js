import { ProcessorErrorFactory } from './../error/processorErrorFactory';
import { LanguageDefinedFunction } from './../definedFunctions';
import { LanguageService } from './../../services/languageService';
import { ArrayDeclaration, While, For, Switch, Assign, Break, IfThenElse, Return, ArrayIndexAssign } from '../../ast/commands';
import { InfixApp, UnaryApp, FunctionCall, IntLiteral, RealLiteral, StringLiteral, BoolLiteral, VariableLiteral, ArrayLiteral, ArrayAccess } from '../../ast/expressions';
import { Literal } from '../../ast/expressions/literal';
import { resultTypeAfterInfixOp, resultTypeAfterUnaryOp } from '../compatibilityTable';
import { Types } from '../../typeSystem/types';
import { CompoundType } from '../../typeSystem/compoundType';
import { MultiType } from '../../typeSystem/multiType';
import { Config } from '../../util/config';
import { Store } from '../store/store';
import { IVProgParser } from '../../ast/ivprogParser';

export class SemanticAnalyser {

  static analyseFromSource (stringCode) {
    const parser = IVProgParser.createParser(stringCode);
    const semantic = new SemanticAnalyser(parser.parseTree());
    return semantic.analyseTree();
  }

  constructor(ast) {
    this.ast = ast;
    this.lexerClass = LanguageService.getCurrentLexer();
    const lexer = new this.lexerClass(null);
    this.literalNames = lexer.literalNames;
    this.symbolMap = null;
    this.currentFunction = null;
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
      return null;
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
        throw ProcessorErrorFactory.not_implemented(name);
      }
      return fun;
    } else {
      const val = this.ast.functions.find( v => v.name === name);
      if (!!!val) {
        return null;
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
      throw ProcessorErrorFactory.main_missing();
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
          throw ProcessorErrorFactory.array_dimension_not_int_full(declaration.sourceInfo);
        }
        if (declaration.columns !== null) {
          const columnType = this.evaluateExpressionType(declaration.columns);
          if (!columnType.isCompatible(Types.INTEGER)) {
            throw ProcessorErrorFactory.array_dimension_not_int_full(declaration.sourceInfo);
          }
        }
        this.insertSymbol(declaration.id, {id: declaration.id, lines: declaration.lines, columns: declaration.columns, type: declaration.type});
        return;
      }
      this.evaluateArrayLiteral(declaration.id, declaration.lines, declaration.columns, declaration.type, declaration.initial);
      this.insertSymbol(declaration.id, {id: declaration.id, lines: declaration.lines, columns: declaration.columns, type: declaration.type});

    } else {
      if(declaration.initial === null) {
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type});
        return;
      }
      const resultType = this.evaluateExpressionType(declaration.initial);
      if(resultType instanceof MultiType) {
        if(!resultType.isCompatible(declaration.type)) {
          const stringInfo = declaration.type.stringInfo();
          const info = stringInfo[0];
          throw ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, declaration.sourceInfo);
        }
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type})
      } else if((!declaration.type.isCompatible(resultType) && !Config.enable_type_casting)
        || (!declaration.type.isCompatible(resultType) && Config.enable_type_casting
        && !Store.canImplicitTypeCast(declaration.type, resultType))) {
        const stringInfo = declaration.type.stringInfo();
        const info = stringInfo[0];
        throw ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, declaration.sourceInfo);
      } else {
        this.insertSymbol(declaration.id, {id: declaration.id, type: declaration.type});
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
        throw ProcessorErrorFactory.void_in_expression_full(LanguageDefinedFunction.getMainFunctionName(), expression.sourceInfo);
      }
      const fun = this.findFunction(expression.id);
      if(fun === null) {
        throw ProcessorErrorFactory.function_missing_full(expression.id, expression.sourceInfo);
      }
      if (fun.returnType.isCompatible(Types.VOID)) {
        throw ProcessorErrorFactory.void_in_expression_full(expression.id, expression.sourceInfo);
      }
      this.assertParameters(fun, expression.actualParameters);
      return fun.returnType;
    } else if (expression instanceof ArrayAccess) {
      const arrayTypeInfo = this.findSymbol(expression.id, this.symbolMap);
      if(arrayTypeInfo === null) {
        throw ProcessorErrorFactory.symbol_not_found_full(expression.id, expression.sourceInfo);
      }
      if (!(arrayTypeInfo.type instanceof CompoundType)) {
        throw ProcessorErrorFactory.invalid_array_access_full(expression.id, expression.sourceInfo);
      }
      const lineType = this.evaluateExpressionType(expression.line);
      if (!lineType.isCompatible(Types.INTEGER)) {
        throw ProcessorErrorFactory.array_dimension_not_int_full(expression.sourceInfo);
      }
      if (expression.column !== null) {
        if (arrayTypeInfo.columns === null) {
          throw ProcessorErrorFactory.invalid_matrix_access_full(expression.id, expression.sourceInfo);
        }
        const columnType = this.evaluateExpressionType(expression.column);
        if(!columnType.isCompatible(Types.INTEGER)) {
          throw ProcessorErrorFactory.array_dimension_not_int_full(expression.sourceInfo);
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
      if(typeInfo === null) {
        throw ProcessorErrorFactory.symbol_not_found_full(literal.id, literal.sourceInfo);
      }
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
            const strInfo = last.stringInfo();
            const info = strInfo[0];
            const strExp = literal.toString();
            throw ProcessorErrorFactory.incompatible_types_array_full(strExp,info.type, info.dim, literal.sourceInfo);
          }
        }
      }
      if(last instanceof CompoundType) {
        return new CompoundType(last.innerType, last.dimensions + 1);
      }
      return new CompoundType(last, 1);
    }
  }

  evaluateArrayLiteral (id, lines, columns, type, literal) {
    /* if (literal instanceof ArrayLiteral) {
      const dimType = this.evaluateExpressionType(lines);
      if (!dimType.isCompatible(Types.INTEGER)) {
        throw ProcessorErrorFactory.array_dimension_not_int_full(literal.sourceInfo);
      }
      if ((lines instanceof IntLiteral)) {
        if (!lines.value.eq(literal.value.length)) {
          if(type.dimensions > 1) {
            throw ProcessorErrorFactory.matrix_line_outbounds_full(id, literal.value.length, lines.value.toNumber(), literal.sourceInfo)
          } else {
            throw ProcessorErrorFactory.vector_line_outbounds_full(id, literal.value.length, lines.value.toNumber(), literal.sourceInfo)
          }
        } else if (lines.value.isNeg()) {
          throw ProcessorErrorFactory.array_dimension_not_positive_full(literal.sourceInfo);
        }
      }
      if (columns === null) {
        // it's a vector...
        literal.value.reduce((last, next) => {
          const eType = this.evaluateExpressionType(next);
          if (!last.canAccept(eType)) {
            const strInfo = last.stringInfo();
            const info = strInfo[0];
            const strExp = literal.toString();
            throw ProcessorErrorFactory.incompatible_types_array_full(strExp,info.type, info.dim, literal.sourceInfo);
          }
          return last;
        }, type);
        return true;
      } else {
        const dimType = this.evaluateExpressionType(columns);
        if (!dimType.isCompatible(Types.INTEGER)) {
          throw ProcessorErrorFactory.array_dimension_not_int_full(literal.sourceInfo);
        }
        if ((columns instanceof IntLiteral)) {
          const columnValue = literal.value[0].value.length;
          if (!columns.value.eq(columnValue)) {
            if(type.dimensions > 1) {
              throw ProcessorErrorFactory.matrix_column_outbounds_full(id, literal.value.length, columns.value.toNumber(), literal.sourceInfo)
            } else {
              throw ProcessorErrorFactory.invalid_matrix_access_full(id, literal.sourceInfo);
            }
          } else if (columns.value.isNeg()) {
            throw ProcessorErrorFactory.array_dimension_not_positive_full(literal.sourceInfo);
          }
          for (let i = 0; i < columns; i++) {
            const anotherArray = literal.value[i];
            this.evaluateArrayLiteral(id, columns, null, type, anotherArray)
          }
        }
      }

    } else {

      const resultType = this.evaluateExpressionType(literal);
      if (!(resultType instanceof CompoundType)) {
        const strInfo = type.stringInfo();
        const info = strInfo[0];
        const strExp = literal.toString();
        throw ProcessorErrorFactory.incompatible_types_array_full(strExp,info.type, info.dim, literal.sourceInfo);
      }
      if (!type.isCompatible(resultType)) {
        const strInfo = type.stringInfo();
        const info = strInfo[0];
        const strExp = literal.toString();
        throw ProcessorErrorFactory.incompatible_types_array_full(strExp,info.type, info.dim, literal.sourceInfo);
      }
      return true;
    } */
    return true;
  }

  assertFunction (fun) {
    this.pushMap();
    this.currentFunction = fun;
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
      throw ProcessorErrorFactory.function_no_return(fun.name);
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
        throw ProcessorErrorFactory.loop_condition_type_full(cmd.expression.toString(), cmd.sourceInfo);
      }
      this.checkCommands(type, cmd.commands, optional);
      return false;
    } else if (cmd instanceof For) {
      this.checkCommand(type, cmd.assignment, optional);
      const resultType = this.evaluateExpressionType(cmd.condition);
      if (!resultType.isCompatible(Types.BOOLEAN)) {
        throw ProcessorErrorFactory.for_condition_type_full(cmd.condition.toString(), cmd.sourceInfo);
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
            const strInfo = sType.stringInfo();
            const info = strInfo[0];
            const strExp = aCase.expression.toString();
            throw ProcessorErrorFactory.invalid_case_type_full(strExp, info.type, info.dim, aCase.sourceInfo);
          }
        } else {
          hasDefault = true;
        }
        result = result && this.checkCommands(type, aCase.commands, result);        
      }
      return result && hasDefault;

    } else if (cmd instanceof ArrayIndexAssign) {
      const typeInfo = this.findSymbol(cmd.id, this.symbolMap);
      if(typeInfo === null) {
        throw ProcessorErrorFactory.symbol_not_found_full(cmd.id, cmd.sourceInfo);
      }
      if(!(typeInfo.type instanceof CompoundType)) {
        throw ProcessorErrorFactory.invalid_array_access_full(cmd.id, cmd.sourceInfo);
      }
      const exp = cmd.expression;
      const lineExp = cmd.line;
      const lineType = this.evaluateExpressionType(lineExp);
      if (!lineType.isCompatible(Types.INTEGER)) {
        throw ProcessorErrorFactory.array_dimension_not_int_full(cmd.sourceInfo);
      }
      const columnExp = cmd.column;
      if (typeInfo.columns === null && columnExp !== null) {
        throw ProcessorErrorFactory.invalid_matrix_access_full(cmd.id, cmd.sourceInfo);
      } else if (columnExp !== null) {
        const columnType = this.evaluateExpressionType(columnExp);
        if (!columnType.isCompatible(Types.INTEGER)) {
          throw ProcessorErrorFactory.array_dimension_not_int_full(cmd.sourceInfo);
        }
      }
      // exp can be a arrayLiteral, a single value exp or an array access
      if(exp instanceof ArrayLiteral) {
        this.evaluateArrayLiteral(cmd.id, typeInfo.lines, (columnExp ? typeInfo.columns : null), typeInfo.type, exp);
      } else {
        // cannot properly evaluate since type system is poorly constructed
      }
      return optional;
    } else if (cmd instanceof Assign) {
      const typeInfo = this.findSymbol(cmd.id, this.symbolMap);
      if(typeInfo === null) {
        throw ProcessorErrorFactory.symbol_not_found_full(cmd.id, cmd.sourceInfo);
      }
      const exp = cmd.expression;
      if(exp instanceof ArrayLiteral) {
        if(!(typeInfo.type instanceof CompoundType)) {
          const stringInfo = typeInfo.type.stringInfo();
          const info = stringInfo[0];
          throw ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, cmd.sourceInfo);
        }
        this.evaluateArrayLiteral(cmd.id, typeInfo.lines, typeInfo.columns, typeInfo.type, exp);
      } else {
        const resultType = this.evaluateExpressionType(exp);
        if((!resultType.isCompatible(typeInfo.type) && !Config.enable_type_casting)
          || (!resultType.isCompatible(typeInfo.type) && Config.enable_type_casting
          && !Store.canImplicitTypeCast(typeInfo.type, resultType))) {
          const stringInfo = typeInfo.type.stringInfo();
          const info = stringInfo[0];
          throw ProcessorErrorFactory.incompatible_types_full(info.type, info.dim, cmd.sourceInfo);
        }
      }
      return optional;
    } else if (cmd instanceof Break) {
      return optional;
    } else if (cmd instanceof IfThenElse) {
      const resultType = this.evaluateExpressionType(cmd.condition);
      if (!resultType.isCompatible(Types.BOOLEAN)) {
        throw ProcessorErrorFactory.if_condition_type_full(cmd.condition.toString(), cmd.sourceInfo);
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
      if(fun === null) {
        throw ProcessorErrorFactory.function_missing_full(cmd.id, cmd.sourceInfo);
      }
      this.assertParameters(fun, cmd.actualParameters);
      return optional;
    } else if (cmd instanceof Return) {
      const funcName = this.currentFunction.isMain ? LanguageDefinedFunction.getMainFunctionName() : this.currentFunction.name
      if (cmd.expression === null && !type.isCompatible(Types.VOID)) {
        const stringInfo = type.stringInfo();
        const info = stringInfo[0];
        throw ProcessorErrorFactory.invalid_void_return_full(funcName, info.type, info.dim, cmd.sourceInfo);
      } else if (cmd.expression !== null) {
        const resultType = this.evaluateExpressionType(cmd.expression);
        if (!type.isCompatible(resultType)) {
          const stringInfo = type.stringInfo();
          const info = stringInfo[0];
          throw ProcessorErrorFactory.invalid_return_type_full(funcName, info.type, info.dim, cmd.sourceInfo);
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
      throw ProcessorErrorFactory.invalid_parameters_size_full(fun.name, actualParametersList.length, fun.formalParameters.length, null);
    }
    for (let i = 0; i < actualParametersList.length; i++) {
      const param = actualParametersList[i];
      const formalParam = fun.formalParameters[i];
      const id = formalParam.id;
      if(formalParam.byRef) {
        if (!(param instanceof VariableLiteral || param instanceof ArrayAccess)) {
          throw ProcessorErrorFactory.invalid_parameter_type_full(id, param.toString(), param.sourceInfo);
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
          if(Config.enable_type_casting && !formalParam.byRef) {
            if(resultType.isCompatible(Types.INTEGER) || resultType.isCompatible(Types.REAL)) {
              if(formalParam.type.isCompatible(Types.INTEGER) || formalParam.type.isCompatible(Types.REAL)) {
                continue;
              }
            }
          }
          throw ProcessorErrorFactory.invalid_parameter_type_full(id, param.toString(), param.sourceInfo);
        }
      } else if (resultType instanceof MultiType) {
        if(!resultType.isCompatible(formalParam.type)) {
          if(Config.enable_type_casting && !formalParam.byRef) {
            if(resultType.isCompatible(Types.INTEGER) || resultType.isCompatible(Types.REAL)) {
              if(formalParam.type.isCompatible(Types.INTEGER) || formalParam.type.isCompatible(Types.REAL)) {
                continue;
              }
            }
          }
          throw ProcessorErrorFactory.invalid_parameter_type_full(id, param.toString(), param.sourceInfo);
        }
      } else if(!formalParam.type.isCompatible(resultType)) {
        if(Config.enable_type_casting && !formalParam.byRef) {
          if (Store.canImplicitTypeCast(formalParam.type, resultType)) {
            continue;
          }
        }
        throw ProcessorErrorFactory.invalid_parameter_type_full(id, param.toString(), param.sourceInfo);
      }

    }
  }
}

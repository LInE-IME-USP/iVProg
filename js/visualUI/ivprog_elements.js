import { Types } from './../ast/types';

export const COMMAND_TYPES = Object.freeze({function:"function", comment:"comment", reader:"reader", writer:"writer", attribution:"attribution", iftrue:"iftrue",
 repeatNtimes:"repeatNtimes", whiletrue:"whiletrue", dowhiletrue:"dowhiletrue", switch:"switch", functioncall:"functioncall"});

export const ARITHMETIC_TYPES = Object.freeze({plus:"plus", minus:"minus", multiplication:"multiplication", division:"division", module:"module"});

export class Variable {

  constructor (type, name, value, dimensions = 0, is_constant = false, rows = 0, columns = 0) {
    this.type = type;
    this.name = name;
    this.value = value;
    this.dimensions = dimensions;
    this.is_constant = is_constant;
    this.rows = rows;
    this.columns = columns;
  }
}

export class Function {

  constructor (name, return_type = Types.VOID, return_dimensions = 0, parameters_list = [], is_main = false, is_hidden = false, variables_list = [], function_comment = null) {
    this.type = COMMAND_TYPES.function;
    this.name = name;
    this.return_type = return_type;
    this.return_dimensions = return_dimensions;
    this.parameters_list = parameters_list;
    this.is_main = is_main;
    this.is_hidden = is_hidden;
    this.variables_list = variables_list;
    this.function_comment = function_comment;
    this.commands = [];
  }
}

export class Comment {
  
  constructor (comment_text) {
    this.type = COMMAND_TYPES.comment;
    this.comment_text = comment_text;
  }
}

export class Reader {
  
  constructor (variable = null, row = null, column = null, variable_value_menu = null) {
    this.type = COMMAND_TYPES.reader;
    this.variable = variable;
    this.row = row;
    this.column = column;
    this.variable_value_menu = variable_value_menu;
  }
}

export class Writer {

  constructor (content) {
    this.type = COMMAND_TYPES.writer;
    this.content = content;
  }
}

export class Attribution {

  constructor (variable, expression) {
    this.type = COMMAND_TYPES.attribution;
    this.variable = variable;
    this.expression = expression;
  }
}

export class Expression {

  constructor (operand1, operand2, operator) {
    this.operand1 = operand1;
    this.operand2 = operand2;
    this.operator = operator;
  }
}

export class IfTrue {

  constructor (expression, commands_block, commands_else) {
    this.type = COMMAND_TYPES.iftrue;
    this.expression = expression;
    this.commands_block = commands_block;
    this.commands_else = commands_else;
  }
}

export class RepeatNTimes {

  constructor (expression1, expression2, expression3, commands_block) {
    this.type = COMMAND_TYPES.repeatNtimes;
    this.expression1 = expression1;
    this.expression2 = expression2;
    this.expression3 = expression3;
    this.commands_block = commands_block;
  }
}

export class WhileTrue {

  constructor (expression, commands_block) {
    this.type = COMMAND_TYPES.whiletrue;
    this.expression = expression;
    this.commands_block = commands_block;
  }
}

export class DoWhileTrue {

  constructor (expression, commands_block) {
    this.type = COMMAND_TYPES.dowhiletrue;
    this.expression = expression;
    this.commands_block = commands_block;
  }
}

export class Switch {

  constructor (variable, cases, commands_blocks) {
    this.type = COMMAND_TYPES.switch;
    this.variable = variable;
    this.cases = cases;
    this.commands_blocks = commands_blocks;
  }
}

export class FunctionCall {

  constructor (function_called, parameters_list) {
    this.type = COMMAND_TYPES.functioncall;
    this.function_called = function_called;
    this.parameters_list = parameters_list;
  }
}

export class VariableValueMenu {
  
  constructor (variable_and_value = 7, content = null, row = null, column = null, include_constant = true) {
    this.variable_and_value = variable_and_value;
    this.content = content;
    this.row = row;
    this.column = column;
    this.include_constant = include_constant;
  }
}

export class Program {

  constructor () {
    this.functions = [];
    this.globals = [];
  }

  addFunction (function_to_add) {
    this.functions.push(function_to_add);
  }

  addVariable (function_to_receive, variable) {
    if (this.functions[function_to_receive].variable === null) {
      this.functions[function_to_receive].variables_list = [];
    }
    this.functions[function_to_receive].variables_list.push(variable);
  }

  addGlobal (variable) {
    this.globals.push(variable);
  }
}

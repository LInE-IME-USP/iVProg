import * as VariableValueMenuManagement from './commands/variable_value_menu';
import { Types } from './types';
import WatchJS from 'melanke-watchjs';
import * as AlgorithmManagement from './algorithm';

export const COMMAND_TYPES = Object.freeze({function:"function", comment:"comment", reader:"reader", writer:"writer", attribution:"attribution", iftrue:"iftrue",
 repeatNtimes:"repeatNtimes", whiletrue:"whiletrue", dowhiletrue:"dowhiletrue", switch:"switch", switchcase:"switchcase", functioncall:"functioncall", break:"break",
 return:"return"});

export const ARITHMETIC_TYPES = Object.freeze({plus:"plus", minus:"minus", multiplication:"multiplication", division:"division", module:"module", none:"none"});

export const EXPRESSION_ELEMENTS = Object.freeze({exp_op_exp:"exp_op_exp", op_exp:"op_exp", par_exp_par:"par_exp_par", start_point:"start_point"});

export const EXPRESSION_TYPES = Object.freeze({exp_conditional:"exp_conditional", exp_logic:"exp_logic", exp_arithmetic:"exp_arithmetic"});

export const ARITHMETIC_COMPARISON = Object.freeze({greater_than:"greater_than", less_than:"less_than", equals_to:"equals_to", not_equals_to:"not_equals_to", greater_than_or_equals_to:"greater_than_or_equals_to", less_than_or_equals_to:"less_than_or_equals_to"});

export const LOGIC_COMPARISON = Object.freeze({equals_to:"equals_to", not_equals_to:"not_equals_to", and:"and", or:"or"});

export const SYSTEM_FUNCTIONS_CATEGORIES = Object.freeze({math:"math", text:"text_t", arrangement:"arrangement", conversion:"conversion"});

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

  constructor (name, return_type = Types.VOID, return_dimensions = 0, parameters_list = [], is_main = false, is_hidden = false, variables_list = [], function_comment = null, commands = []) {
    this.type = COMMAND_TYPES.function;
    this.name = name;
    this.return_type = return_type;
    this.return_dimensions = return_dimensions;
    this.parameters_list = parameters_list;
    this.is_main = is_main;
    this.is_hidden = is_hidden;
    this.variables_list = variables_list;
    this.function_comment = function_comment;
    this.commands = commands;
  }
}

export class SystemFunction {

  constructor (identifier, return_type, return_dimensions, parameters_list, function_comment = null, category) {
    this.type = COMMAND_TYPES.function;
    this.identifier = identifier;
    this.return_type = return_type;
    this.return_dimensions = return_dimensions;
    this.parameters_list = parameters_list;
    this.function_comment = function_comment;
    this.category = category;
  }
}

export class Comment {
  
  constructor (comment_text) {
    this.type = COMMAND_TYPES.comment;
    this.comment_text = comment_text;
  }
}

export class Break {
  
  constructor () {
    this.type = COMMAND_TYPES.break;
  }
}

export class Reader {
  
  constructor (variable_value_menu = new VariableValueMenu()) {
    this.type = COMMAND_TYPES.reader;
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

  constructor (variable, expression = []) {
    this.type = COMMAND_TYPES.attribution;
    this.variable = variable;
    this.expression = expression;
  }
}

export class ExpressionElement {

  constructor (type_exp, itens = []) {
    this.type_exp = type_exp;
    this.itens = itens;
  }
}

export class ConditionalExpression {

  constructor (expression) {
    this.type = EXPRESSION_TYPES.exp_conditional;
    this.expression = expression;
  }
}

export class LogicExpression {

  constructor (has_neg, first_operand, second_operand, operator) {
    this.type = EXPRESSION_TYPES.exp_logic;
    this.has_neg = has_neg;
    this.first_operand = first_operand;
    this.second_operand = second_operand;
    this.operator = operator;
  }
}

export class ArithmeticExpression {

  constructor (first_operand, second_operand, operator) {
    this.type = EXPRESSION_TYPES.exp_arithmetic;
    this.first_operand = first_operand;
    this.second_operand = second_operand;
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

  constructor (var_attribution, var_incrementation, expression1, expression2, expression3, commands_block) {
    this.type = COMMAND_TYPES.repeatNtimes;
    this.var_attribution = var_attribution;
    this.var_incrementation = var_incrementation;
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

  constructor (variable, cases = []) {
    this.type = COMMAND_TYPES.switch;
    this.variable = variable;
    this.cases = cases;
  }
}

export class Return {

 constructor (variable_value_menu) {
    this.type = COMMAND_TYPES.return;
    this.variable_value_menu = variable_value_menu;
  } 
}

export class SwitchCase {

 constructor (variable_value_menu, commands_block = []) {
    this.type = COMMAND_TYPES.switchcase;
    this.variable_value_menu = variable_value_menu;
    this.commands_block = commands_block;
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

  constructor (variable_and_value = 7, content = null, row = null, column = null, include_constant = true, dimensions = 0) {
    this.type = "var_value";
    this.variable_and_value = variable_and_value;
    this.content = content;
    this.row = row;
    this.column = column;
    this.include_constant = include_constant;
    this.dimensions = dimensions;
  }
}

export class FunctionCallMenu {
  
  constructor (function_called = null, parameters_list = []) {
    this.type = "function_call";
    this.function_called = function_called;
    this.parameters_list = parameters_list;
  }
}

export class Program {

  constructor () {
    this.functions = [];
    this.globals = [];
  }

  addFunction (function_to_add) {

    WatchJS.watch(function_to_add.parameters_list, function(){
      if (window.insertContext) {
        setTimeout(function(){ AlgorithmManagement.renderAlgorithm(); }, 300);
        window.insertContext = false;
      } else {
        AlgorithmManagement.renderAlgorithm();
      }
    }, 1);

    WatchJS.watch(function_to_add.variables_list, function(){
      if (window.insertContext) {
        setTimeout(function(){ AlgorithmManagement.renderAlgorithm(); }, 300);
        window.insertContext = false;
      } else {
        AlgorithmManagement.renderAlgorithm();
      }
    }, 1);

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

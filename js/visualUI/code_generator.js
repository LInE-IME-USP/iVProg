import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as Utils from './utils';

export function generate () {

	$('.ivprog_visual_panel').find('.error_icon').remove();

	var code = LocalizedStrings.getUI('program') + ' { ';

	code += globalsCode();

	code += '\n';

	var has_error = false;

	for (var i = 0; i < window.program_obj.functions.length; i ++) {
		var n_code = functionsCode(window.program_obj.functions[i]);
		if (n_code == null) {
			has_error = true;
		}
		code += n_code;
		code += '\n';

	}

	code += '\n}';

	if (has_error) {
		return null;
	} else {
		return code;
	}
}

function functionsCode (function_obj) {
	var ret = '\n\t' + LocalizedStrings.getUI('function') + ' ';

	var has_error = false;

	switch (function_obj.return_type) {
		case Types.INTEGER:
			ret += LocalizedStrings.getUI('type_integer');
			break;
		case Types.REAL:
			ret += LocalizedStrings.getUI('type_real');
			break;
		case Types.TEXT:
			ret += LocalizedStrings.getUI('type_text');
			break;
		case Types.BOOLEAN:
			ret += LocalizedStrings.getUI('type_boolean');
			break;
		case Types.VOID:
			ret += LocalizedStrings.getUI('type_void');
			break;
	}
	ret += ' ';

	if (function_obj.return_dimensions == 1) {
		ret += '[] '
	} else if (function_obj.return_dimensions == 2) {
		ret += '[][] '
	}

	ret += function_obj.name + ' ( ';

	for (var j = 0; j < function_obj.parameters_list.length; j++) {
		ret += parametersCode(function_obj.parameters_list[j]);
		if ((j + 1) < function_obj.parameters_list.length) {
			ret += ',';
		}
	}

	ret += ' )  {';

	for (var j = 0; j < function_obj.variables_list.length; j++) {
		ret += variablesCode(function_obj.variables_list[j]);
	}

	for (var j = 0; j < function_obj.commands.length; j++) {
		//try {
			ret += commandsCode(function_obj.commands[j]);
		/*} catch (err) {

			has_error = true;

			console.error(err.message);

			var todos = $('body').find('.command_container');

			for (var i = 0; i < todos.length; i++) {
				if ($(todos[i]).data('command') == function_obj.commands[j]) {
					$( todos[i] ).prepend( ' <i class="ui icon red exclamation triangle error_icon"></i> ' );
					break;
				}
			}
			
		}*/
		
	}

	ret += '\n\t}';

	if (has_error) {
		return null;
	} else {
		return ret;
	}
}

function commandsCode (command_obj, indentation = 2) {
	switch (command_obj.type) {
		case Models.COMMAND_TYPES.break:
			return breaksCode(command_obj, indentation);

		case Models.COMMAND_TYPES.comment:
			return commentsCode(command_obj, indentation);

		case Models.COMMAND_TYPES.reader:
			return readersCode(command_obj, indentation);

		case Models.COMMAND_TYPES.writer:
			return writersCode(command_obj, indentation);

		case Models.COMMAND_TYPES.functioncall:
			return functioncallsCode(command_obj, indentation);

		case Models.COMMAND_TYPES.attribution:
			return attributionsCode(command_obj, indentation);

		case Models.COMMAND_TYPES.whiletrue:
			return whiletruesCode(command_obj, indentation);

		case Models.COMMAND_TYPES.dowhiletrue:
			return doWhilesCode(command_obj, indentation);

		case Models.COMMAND_TYPES.iftrue:
			return iftruesCode(command_obj, indentation);

		case Models.COMMAND_TYPES.repeatNtimes:
			return repeatNtimesCode(command_obj, indentation);

		case Models.COMMAND_TYPES.switch:
			return switchsCode(command_obj, indentation);

		case Models.COMMAND_TYPES.return:
			return returnsCode(command_obj, indentation);
	}
}

function returnsCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_return');

	if (command_obj.variable_value_menu) {
		try {
			ret += ' ' + elementExpressionCode(command_obj.variable_value_menu);
			//ret += ' ' + variableValueMenuCode(command_obj.variable_value_menu, true);
		} catch(err) {}
	}

	return ret;
}

function breaksCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_break');

	return ret;
}

function switchsCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_code_switch') + ' ( ';

	ret += variableValueMenuCode(command_obj.variable);

	ret += ' ) { ';

	if (command_obj.cases) {
		for (var i = 0; i < command_obj.cases.length; i++) {
			ret += switchcasesCode(command_obj.cases[i], (indentation + 1));
		}
	}

	ret += '\n';
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}
	ret += '} ';

	return ret;
}

function switchcasesCode (switchcase, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_code_case') + ' ';
	ret += variableValueMenuCode(switchcase.variable_value_menu);
	ret += ' :';

	if (switchcase.commands_block) {
		for (var i = 0; i < switchcase.commands_block.length; i++) {
			ret += commandsCode(switchcase.commands_block[i], (indentation + 1));
		}
	}

	return ret;

}

function repeatNtimesCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_for') + ' ( ';

	if (command_obj.var_attribution) {
		ret += variableValueMenuCode(command_obj.var_attribution);
		ret += ' = ';
		ret += variableValueMenuCode(command_obj.expression1);
	}
	ret += ' ; ';


	if (command_obj.expression2) {
		/*switch (command_obj.expression2.expression.type) {
			case Models.EXPRESSION_TYPES.exp_logic:
				ret += logicExpressionCode(command_obj.expression2.expression);
				break;
			case Models.EXPRESSION_TYPES.exp_arithmetic:
				ret += arithmeticExpressionCode(command_obj.expression2.expression);
				break;
		}*/

		ret += elementExpressionCode(command_obj.expression2);
	}

	ret += ' ; ';

	if (command_obj.var_incrementation) {
		ret += variableValueMenuCode(command_obj.var_incrementation);
		ret += ' = ';
		ret += variableValueMenuCode(command_obj.expression3.itens[0]);

		switch (command_obj.expression3.itens[1]) {
			case Models.ARITHMETIC_TYPES.plus:
				ret += ' + ';
				break;
			case Models.ARITHMETIC_TYPES.minus:
				ret += ' - ';
				break;
			case Models.ARITHMETIC_TYPES.multiplication:
				ret += ' * ';
				break;
			case Models.ARITHMETIC_TYPES.division:
				ret += ' / ';
				break;
			case Models.ARITHMETIC_TYPES.module:
				ret += ' % ';
				break;
		}

		ret += variableValueMenuCode(command_obj.expression3.itens[2]);		
	}

	ret += ' )  { ';

	if (command_obj.commands_block) {
		for (var i = 0; i < command_obj.commands_block.length; i++) {
			ret += commandsCode(command_obj.commands_block[i], (indentation + 1));
		}
	}

	ret += '\n';
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += '}';
	return ret;
}

function iftruesCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_if');

	if (!command_obj.expression) {
		Utils.renderErrorMessage(command_obj.expression.dom_object, LocalizedStrings.getUI('inform_valid_expression'));
	} else {
		ret += ' ( ';
		ret += elementExpressionCode(command_obj.expression);
		ret += ' ) ';
	}

	/*switch (command_obj.expression.expression.type) {
		case Models.EXPRESSION_TYPES.exp_logic:
			ret += logicExpressionCode(command_obj.expression.expression);
			break;
		case Models.EXPRESSION_TYPES.exp_arithmetic:
			ret += arithmeticExpressionCode(command_obj.expression.expression);
			break;
	}*/

	ret += ' { ';

	if (command_obj.commands_block) {
		for (var i = 0; i < command_obj.commands_block.length; i++) {
			ret += commandsCode(command_obj.commands_block[i], (indentation + 1));
		}
	}

	ret += '\n';
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += '} ' + LocalizedStrings.getUI('text_else') + ' {';

	if (command_obj.commands_else) {
		for (var i = 0; i < command_obj.commands_else.length; i++) {
			ret += commandsCode(command_obj.commands_else[i], (indentation + 1));
		}
	}

	ret += '\n';
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += '}';

	return ret;
}


function doWhilesCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_code_do') + ' { ';

	if (command_obj.commands_block) {
		for (var i = 0; i < command_obj.commands_block.length; i++) {
			ret += commandsCode(command_obj.commands_block[i], (indentation + 1));
		}
	}

	ret += '\n';
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += '} ' + LocalizedStrings.getUI('text_code_while');

	if (!command_obj.expression) {
		Utils.renderErrorMessage(command_obj.expression.dom_object, LocalizedStrings.getUI('inform_valid_expression'));
	}

	/*switch (command_obj.expression.expression.type) {
		case Models.EXPRESSION_TYPES.exp_logic:
			ret += logicExpressionCode(command_obj.expression.expression);
			break;
		case Models.EXPRESSION_TYPES.exp_arithmetic:
			ret += arithmeticExpressionCode(command_obj.expression.expression);
			break;
	}*/

	if (command_obj.expression) {
		ret += ' ( ';
		ret += elementExpressionCode(command_obj.expression);
		ret += ' ) ';
	}

	return ret;
}


function whiletruesCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_code_while');

	if (!command_obj.expression) {
		Utils.renderErrorMessage(command_obj.expression.dom_object, LocalizedStrings.getUI('inform_valid_expression'));
	}

	/*switch (command_obj.expression.expression.type) {
		case Models.EXPRESSION_TYPES.exp_logic:
			ret += logicExpressionCode(command_obj.expression.expression);
			break;
		case Models.EXPRESSION_TYPES.exp_arithmetic:
			ret += arithmeticExpressionCode(command_obj.expression.expression);
			break;
	}*/
	if (command_obj.expression) {
		ret += ' ( ';
		ret += elementExpressionCode(command_obj.expression);
		ret += ' ) ';
	}

	ret += ' { ';

	if (command_obj.commands_block) {
		for (var i = 0; i < command_obj.commands_block.length; i++) {
			ret += commandsCode(command_obj.commands_block[i], (indentation + 1));
		}
	}

	ret += '\n';
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += '}';

	return ret;
}

function logicExpressionCode (expression) {
	var ret = ' ( ';

	if (expression.first_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		ret += logicExpressionCode(expression.first_operand);
	} else if (expression.first_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		ret += arithmeticExpressionCode(expression.first_operand);
	} else {
		ret += variableValueMenuCode(expression.first_operand);
	}

	if (expression.operator) {
		switch (expression.operator) {
	        case Models.LOGIC_COMPARISON.equals_to:
	        	ret += ' == ';
	        	break;
	        case Models.LOGIC_COMPARISON.not_equals_to:
	        	ret += ' != ';
	        	break;
	        case Models.LOGIC_COMPARISON.and:
	        	ret += ' && ';
	        	break;
	        case Models.LOGIC_COMPARISON.or:
	        	ret += ' || ';
	        	break;
		}

		if (expression.second_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
			ret += logicExpressionCode(expression.second_operand);
		} else if (expression.second_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
			ret += arithmeticExpressionCode(expression.second_operand);
		} else {
			ret += variableValueMenuCode(expression.second_operand);
		}

	}

	ret += ' ) ';

	return ret;
}

function arithmeticExpressionCode (expression) {
	var ret = ' ( ';

	if (expression.first_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		ret += logicExpressionCode(expression.first_operand);
	} else if (expression.first_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		ret += arithmeticExpressionCode(expression.first_operand);
	} else {
		ret += variableValueMenuCode(expression.first_operand);
	}

	switch (expression.operator) {
        case Models.ARITHMETIC_COMPARISON.greater_than:
        	ret += ' > ';
        	break;
        case Models.ARITHMETIC_COMPARISON.less_than:
        	ret += ' < ';
        	break;
        case Models.ARITHMETIC_COMPARISON.equals_to:
        	ret += ' == ';
        	break;
        case Models.ARITHMETIC_COMPARISON.not_equals_to:
        	ret += ' != ';
        	break;
        case Models.ARITHMETIC_COMPARISON.greater_than_or_equals_to:
        	ret += ' >= ';
        	break;
        case Models.ARITHMETIC_COMPARISON.less_than_or_equals_to:
        	ret += ' <= ';
        	break;
	}

	if (expression.second_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		ret += logicExpressionCode(expression.second_operand);
	} else if (expression.second_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		ret += arithmeticExpressionCode(expression.second_operand);
	} else {
		ret += variableValueMenuCode(expression.second_operand);
	}

	ret += ' ) ';

	return ret;
}

function attributionsCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += variableValueMenuCode(command_obj.variable) + ' = ';

	/*for (var i = 0; i < command_obj.expression.length; i++) {
		ret += elementExpressionCode(command_obj.expression[i]);
	}*/
	ret += elementExpressionCode(command_obj.expression);

	return ret;
}

function elementExpressionCode (expression_obj) {

	var ret = ''; 

	for (var i = 0; i < expression_obj.length; i++) {


		if (expression_obj[i].type) {

			ret += variableValueMenuCode(expression_obj[i]);

		} else if (expression_obj[i].type_op) {

			switch(expression_obj[i].item) {

				case Models.ARITHMETIC_TYPES.plus:
					ret += ' + ';
					break;
				case Models.ARITHMETIC_TYPES.minus:
					ret += ' - ';
					break;
				case Models.ARITHMETIC_TYPES.multiplication:
					ret += ' * ';
					break;
				case Models.ARITHMETIC_TYPES.division:
					ret += ' / ';
					break;
				case Models.ARITHMETIC_TYPES.module:
					ret += ' % ';
					break;

				case Models.LOGIC_COMPARISON.equals_to:
					ret += ' == ';
					break;

				case Models.LOGIC_COMPARISON.not_equals_to:
					ret += ' != ';
					break;

				case Models.LOGIC_COMPARISON.and:
					ret += ' ' + LocalizedStrings.getUI('logic_operator_and') + ' ';
					break;

				case Models.LOGIC_COMPARISON.or:
					ret += ' ' + LocalizedStrings.getUI('logic_operator_or') + ' ';
					break;

				case Models.LOGIC_COMPARISON.not:
					ret += ' ' + LocalizedStrings.getUI('logic_operator_not') + ' ';
					break;

				case Models.ARITHMETIC_COMPARISON.greater_than:
					ret += ' > ';
					break;

				case Models.ARITHMETIC_COMPARISON.less_than:
					ret += ' < ';
					break;

				case Models.ARITHMETIC_COMPARISON.greater_than_or_equals_to:
					ret += ' >= ';
					break;

				case Models.ARITHMETIC_COMPARISON.less_than_or_equals_to:
					ret += ' <= ';
					break;
			}

		} else {

			ret += ' ' + expression_obj[i] + ' ';
			
		}

	}

	return ret;

}

function functioncallsCode (command_obj, indentation) {

	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}
	
	ret += variableValueMenuCode(command_obj.function_called);

	return ret;
}

function readersCode (command_obj, indentation) {
	var ret = '\n';
	
	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += LocalizedStrings.getUI('text_command_read') + ' ( ';

	ret += variableValueMenuCode(command_obj.variable_value_menu);

	ret += ' ) ';
	return ret;
}

function variableValueMenuCode (variable_obj, is_return = false) {
	var ret = '';
	try {
		if (variable_obj.function_called) {

			if (variable_obj.function_called.name) {
				ret += variable_obj.function_called.name + ' ( ';
			} else {
				ret += LocalizedStrings.translateInternalFunction(variable_obj.function_called.identifier,variable_obj.function_called.category) + ' ( ';
			}

			if (variable_obj.parameters_list) {
				for (var i = 0; i < variable_obj.parameters_list.length; i++) {
					ret += variableValueMenuCode(variable_obj.parameters_list[i]);
					if ((i + 1) < variable_obj.parameters_list.length) {
						ret += ', ';
					}
				}
			}

			ret += ' )';
		} else if (variable_obj.content.type) {

			ret += variable_obj.content.name;

			if (variable_obj.content.dimensions == 1 && variable_obj.dimensions != 1) {
				ret += ' [ ' + variableValueMenuCode(variable_obj.column) + ' ] ';
			}

			if (variable_obj.content.dimensions == 2 && variable_obj.dimensions != 2) {
				ret += ' [ ' + variableValueMenuCode(variable_obj.row) + ' ] ';
				ret += ' [ ' + variableValueMenuCode(variable_obj.column) + ' ] ';
			}


		} else {
			if (isNaN(variable_obj.content)) {
				ret += '"' + variable_obj.content + '"';
			} else {
				ret += variable_obj.content;
			}
		}
	} catch (err) {

		if (!is_return) {
			Utils.renderErrorMessage(variable_obj.dom_object, LocalizedStrings.getUI('inform_valid_content'));
			throw err;
		}
	}

	return ret;

}

function writersCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}
	
	ret += LocalizedStrings.getUI('text_command_write') + ' ( ';

	/*for (var i = 0; i < command_obj.content.length; i++) {
		ret += variableValueMenuCode(command_obj.content[i]);

		if ((i + 1) < command_obj.content.length) {
			ret += ' + ';
		}
	}*/

	ret += elementExpressionCode(command_obj.content);

	ret += ' ) ';
	return ret;
}

function commentsCode (command_obj, indentation) {
	var ret = '\n';

	for (var i = 0; i < indentation; i++) {
		ret += '\t';
	}

	ret += '// ';

	ret += command_obj.comment_text.content;
	return ret;
}

function parametersCode (parameter_obj) {
	var ret = '';
	switch (parameter_obj.type) {
		case Types.INTEGER:
			ret += ' '+LocalizedStrings.getUI('type_integer')+' ';
			break;
		case Types.REAL:
			ret += ' '+LocalizedStrings.getUI('type_real')+' ';
			break;
		case Types.TEXT:
			ret += ' '+LocalizedStrings.getUI('type_text')+' ';
			break;
		case Types.BOOLEAN:
			ret += ' '+LocalizedStrings.getUI('type_boolean')+' ';
			break;
	}
	ret += parameter_obj.name + '';

	if (parameter_obj.dimensions == 1) {
		ret += ' []'
	} else if (parameter_obj.dimensions == 2) {
		ret += ' [][]'
	}
	return ret;
}

function variablesCode (variable_obj) {
	var ret = '';

	var temp = variable_obj;

	ret += '\n\t\t';

	if (temp.is_constant) {
		ret += 'const ';
	}
	switch (temp.type) {
		case Types.INTEGER:
			ret += LocalizedStrings.getUI('type_integer')+' ';
			break;
		case Types.REAL:
			ret += LocalizedStrings.getUI('type_real')+' ';
			break;
		case Types.TEXT:
			ret += LocalizedStrings.getUI('type_text')+' ';
			break;
		case Types.BOOLEAN:
			ret += LocalizedStrings.getUI('type_boolean')+' ';
			break;
	}
	ret += temp.name + ' ';

	if (temp.dimensions == 1) {
		ret += '[' + temp.columns + '] ';

		switch (temp.type) {
			case Types.INTEGER:
				ret += '= {' + temp.value + '}';
				break;
			case Types.REAL:
				ret += '= {' + temp.value.toFixed(2) + '}';
				break;
			case Types.TEXT:
				ret += '= {';
				for (var j = 0; j < temp.value.length; j++) {
					ret += '"'+temp.value[j]+'"';
					if ((j + 1) < temp.value.length) {
						ret += ',';
					}
				}
				ret += '}';
				break;
			case Types.BOOLEAN:
				ret += '= {';
				for (var j = 0; j < temp.value.length; j++) {
					if (temp.value[j]) {
						ret += LocalizedStrings.getUI('logic_value_true');
					} else {
						ret += LocalizedStrings.getUI('logic_value_false');
					}
					if ((j + 1) < temp.value.length) {
						ret += ',';
					}
				}
				ret += '}';
				break;
		}

	} else if (temp.dimensions == 2) {
		ret += '[' + temp.rows + '][' + temp.columns + '] ';

		switch (temp.type) {
			case Types.INTEGER:
				ret += '= {';

				for (var j = 0; j < temp.rows; j++) {
					ret += '{' + temp.value[j] + '}';

					if ((j + 1) < temp.rows) {
						ret += ',';
					}
				}

				ret += '}';
				break;
			case Types.REAL:
				ret += '= {';

				for (var j = 0; j < temp.rows; j++) {
					ret += '{' + temp.value[j].toFixed(2) + '}';

					if ((j + 1) < temp.rows) {
						ret += ',';
					}
				}

				ret += '}';
				break;
			case Types.TEXT:
				ret += '= {';

				for (var j = 0; j < temp.rows; j++) {
					ret += '{';

					for (var k = 0; k < temp.columns; k++) {
						ret += '"' + temp.value[j][k] + '"';

						if ((k + 1) < temp.columns) {
							ret += ',';
						}
					}

					ret += '}';
					if ((j + 1) < temp.rows) {
						ret += ',';
					}
				}
				ret += '}';
				break;
			case Types.BOOLEAN:
				ret += '= {';
				for (var j = 0; j < temp.rows; j++) {
					ret += '{';

					for (var k = 0; k < temp.columns; k++) {
						
						if (temp.value[j][k]) {
							ret += LocalizedStrings.getUI('logic_value_true');
						} else {
							ret += LocalizedStrings.getUI('logic_value_false');
						}

						if ((k + 1) < temp.columns) {
							ret += ',';
						}
					}

					ret += '}';
					if ((j + 1) < temp.rows) {
						ret += ',';
					}
				}
				ret += '}';
				break;
		}
	} else {

		switch (temp.type) {
			case Types.INTEGER:
				ret += '= ' + temp.value;
				break;
			case Types.REAL:
				ret += '= ' + temp.value.toFixed(2);
				break;
			case Types.TEXT:
				ret += '= "' + temp.value + '"';
				break;
			case Types.BOOLEAN:
				ret += '= ';
				if (temp.value) {
					ret += LocalizedStrings.getUI('logic_value_true');
				} else {
					ret += LocalizedStrings.getUI('logic_value_false');
				}
				break;
		}

	}

	return ret;

}

function globalsCode () {
	var ret = '';

	if (window.program_obj.globals) {
		for (var i = 0; i < window.program_obj.globals.length; i++) {
			var temp = window.program_obj.globals[i];

			ret += '\n\t';

			if (temp.is_constant) {
				ret += 'const ';
			}
			switch (temp.type) {
				case Types.INTEGER:
					ret += LocalizedStrings.getUI('type_integer');
					break;
				case Types.REAL:
					ret += LocalizedStrings.getUI('type_real');
					break;
				case Types.TEXT:
					ret += LocalizedStrings.getUI('type_text');
					break;
				case Types.BOOLEAN:
					ret += LocalizedStrings.getUI('type_boolean');
					break;
			}
			ret += ' ' + temp.name + ' ';

			if (temp.dimensions == 1) {
				ret += '[' + temp.columns + '] ';

				switch (temp.type) {
					case Types.INTEGER:
						ret += '= {' + temp.value + '}';
						break;
					case Types.REAL:
						ret += '= {';
						for (var j = 0; j < temp.value.length; j++) {
							ret += temp.value[j].toFixed(2);
							if ((j + 1) < temp.value.length) {
								ret += ',';
							}
						}
						ret += '}';
						break;
					case Types.TEXT:
						ret += '= {';
						for (var j = 0; j < temp.value.length; j++) {
							ret += '"'+temp.value[j]+'"';
							if ((j + 1) < temp.value.length) {
								ret += ',';
							}
						}
						ret += '}';
						break;
					case Types.BOOLEAN:
						ret += '= {';
						for (var j = 0; j < temp.value.length; j++) {
							if (temp.value[j]) {
								ret += LocalizedStrings.getUI('logic_value_true');
							} else {
								ret += LocalizedStrings.getUI('logic_value_false');
							}
							if ((j + 1) < temp.value.length) {
								ret += ',';
							}
						}
						ret += '}';
						break;
				}

			} else if (temp.dimensions == 2) {
				ret += '[' + temp.rows + '][' + temp.columns + '] ';

				switch (temp.type) {
					case Types.INTEGER:
						ret += '= {';

						for (var j = 0; j < temp.rows; j++) {
							ret += '{' + temp.value[j] + '}';

							if ((j + 1) < temp.rows) {
								ret += ',';
							}
						}

						ret += '}';
						break;
					case Types.REAL:
						ret += '= {';

						for (var j = 0; j < temp.rows; j++) {
							ret += '{';

							for (var k = 0; k < temp.columns; k++) {
								ret += temp.value[j][k].toFixed(2);

								if ((k + 1) < temp.columns) {
									ret += ',';
								}
							}

							ret += '}';
							if ((j + 1) < temp.rows) {
								ret += ',';
							}
						}

						ret += '}';
						break;
					case Types.TEXT:
						ret += '= {';

						for (var j = 0; j < temp.rows; j++) {
							ret += '{';

							for (var k = 0; k < temp.columns; k++) {
								ret += '"' + temp.value[j][k] + '"';

								if ((k + 1) < temp.columns) {
									ret += ',';
								}
							}

							ret += '}';
							if ((j + 1) < temp.rows) {
								ret += ',';
							}
						}
						ret += '}';
						break;
					case Types.BOOLEAN:
						ret += '= {';
						for (var j = 0; j < temp.rows; j++) {
							ret += '{';

							for (var k = 0; k < temp.columns; k++) {
								
								if (temp.value[j][k]) {
									ret += LocalizedStrings.getUI('logic_value_true');
								} else {
									ret += LocalizedStrings.getUI('logic_value_false');
								}

								if ((k + 1) < temp.columns) {
									ret += ',';
								}
							}

							ret += '}';
							if ((j + 1) < temp.rows) {
								ret += ',';
							}
						}
						ret += '}';
						break;
				}
			} else {

				switch (temp.type) {
					case Types.INTEGER:
						ret += '= ' + temp.value;
						break;
					case Types.REAL:
						ret += '= ' + temp.value.toFixed(2);
						break;
					case Types.TEXT:
						ret += '= "' + temp.value + '"';
						break;
					case Types.BOOLEAN:
						ret += '= ';
						if (temp.value) {
							ret += LocalizedStrings.getUI('logic_value_true');;
						} else {
							ret += LocalizedStrings.getUI('logic_value_false');;
						}
						break;
				}

			}

			

		}
	}

	return ret;
}
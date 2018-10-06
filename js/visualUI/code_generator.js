import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands';

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
			ret += LocalizedStrings.getUI('integer');
			break;
		case Types.REAL:
			ret += LocalizedStrings.getUI('real');
			break;
		case Types.TEXT:
			ret += LocalizedStrings.getUI('text');
			break;
		case Types.BOOLEAN:
			ret += LocalizedStrings.getUI('boolean');
			break;
		case Types.VOID:
			ret += LocalizedStrings.getUI('void');
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
		try {
			ret += commandsCode(function_obj.commands[j]);
		} catch (err) {

			has_error = true;

			console.error(err.message);

			var todos = $('body').find('.command_container');
			for (var i = 0; i < todos.length; i++) {

				if ($(todos[i]).data('command') == function_obj.commands[j]) {
					$( todos[i] ).prepend( ' <i class="ui icon red exclamation triangle error_icon"></i> ' );
					break;
				}
			}
			
		}
		
	}

	ret += '\n\t}';

	if (has_error) {
		return null;
	} else {
		return ret;
	}
}

function commandsCode (command_obj) {
	switch (command_obj.type) {
		case Models.COMMAND_TYPES.comment:
			return commentsCode(command_obj);

		case Models.COMMAND_TYPES.reader:
			return readersCode(command_obj);

		case Models.COMMAND_TYPES.writer:
			return writersCode(command_obj);

		case Models.COMMAND_TYPES.functioncall:
			return functioncallsCode(command_obj);

		case Models.COMMAND_TYPES.attribution:
			return attributionsCode(command_obj);
	}
}

function attributionsCode (command_obj) {
	var ret = '\n\t\t';

	ret += variableValueMenuCode(command_obj.variable) + ' = ';

	for (var i = 0; i < command_obj.expression.length; i++) {
		ret += elementExpressionCode(command_obj.expression[i]);
	}

	return ret;
}

function elementExpressionCode (expression_obj) {

	var ret = ''; 

	for (var i = 0; i < expression_obj.itens.length; i++) {


		if (expression_obj.itens[i].type) {

			ret += variableValueMenuCode(expression_obj.itens[i]);

		} else if (expression_obj.itens[i].type_exp) {

			if (expression_obj.itens[i].type_exp == Models.EXPRESSION_ELEMENTS.par_exp_par) {
				ret += ' ( ';
			}

			ret += elementExpressionCode(expression_obj.itens[i]);

			if (expression_obj.itens[i].type_exp == Models.EXPRESSION_ELEMENTS.par_exp_par) {
				ret += ' ) ';
			}

		} else {

			switch (expression_obj.itens[i]) {
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
			
		}

	}

	return ret;

}

function functioncallsCode (command_obj) {

	var ret = '\n\t\t';
	
	ret += variableValueMenuCode(command_obj.function_called);

	return ret;
}

function readersCode (command_obj) {
	var ret = '\n\t\t' + LocalizedStrings.getUI('text_command_read') + ' ( ';
	
	ret += variableValueMenuCode(command_obj.variable_value_menu);

	ret += ' ) ';
	return ret;
}

function variableValueMenuCode (variable_obj) {

	var ret = '';
	if (variable_obj.function_called) {
		ret += variable_obj.function_called.name + ' ( ';

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

		if (variable_obj.content.dimensions == 1) {
			ret += ' [ ' + variableValueMenuCode(variable_obj.column) + ' ] ';
		}

		if (variable_obj.content.dimensions == 2) {
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

	return ret;

}

function writersCode (command_obj) {
	var ret = '\n\t\t' + LocalizedStrings.getUI('text_command_write') + ' ( ';
	
	for (var i = 0; i < command_obj.content.length; i++) {
		ret += variableValueMenuCode(command_obj.content[i]);

		if ((i + 1) < command_obj.content.length) {
			ret += ' + ';
		}
	}

	ret += ' ) ';
	return ret;
}

function commentsCode (command_obj) {
	var ret = '\n\t\t// ';
	ret += command_obj.comment_text.content;
	return ret;
}

function parametersCode (parameter_obj) {
	var ret = '';
	switch (parameter_obj.type) {
		case Types.INTEGER:
			ret += ' '+LocalizedStrings.getUI('integer')+' ';
			break;
		case Types.REAL:
			ret += ' '+LocalizedStrings.getUI('real')+' ';
			break;
		case Types.TEXT:
			ret += ' '+LocalizedStrings.getUI('text')+' ';
			break;
		case Types.BOOLEAN:
			ret += ' '+LocalizedStrings.getUI('boolean')+' ';
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
			ret += LocalizedStrings.getUI('integer')+' ';
			break;
		case Types.REAL:
			ret += LocalizedStrings.getUI('real')+' ';
			break;
		case Types.TEXT:
			ret += LocalizedStrings.getUI('text')+' ';
			break;
		case Types.BOOLEAN:
			ret += LocalizedStrings.getUI('boolean')+' ';
			break;
	}
	ret += temp.name + ' ';

	if (temp.dimensions == 1) {
		ret += '[' + temp.columns + '] ';

		switch (temp.type) {
			case Types.INTEGER:
			case Types.REAL:
				ret += '= {' + temp.value + '}';
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
						ret += LocalizedStrings.getUI("true");
					} else {
						ret += LocalizedStrings.getUI("false");
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
			case Types.REAL:
				ret += '= {';

				for (var j = 0; j < temp.rows; j++) {
					ret += '{' + temp.value[j] + '}';

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
							ret += LocalizedStrings.getUI("true");
						} else {
							ret += LocalizedStrings.getUI("false");
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
			case Types.REAL:
				ret += '= ' + temp.value;
				break;
			case Types.TEXT:
				ret += '= "' + temp.value + '"';
				break;
			case Types.BOOLEAN:
				ret += '= ';
				if (temp.value) {
					ret += LocalizedStrings.getUI("true");
				} else {
					ret += LocalizedStrings.getUI("false");
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
					ret += LocalizedStrings.getUI('integer');
					break;
				case Types.REAL:
					ret += LocalizedStrings.getUI('real');
					break;
				case Types.TEXT:
					ret += LocalizedStrings.getUI('text');
					break;
				case Types.BOOLEAN:
					ret += LocalizedStrings.getUI('boolean');
					break;
			}
			ret += ' ' + temp.name + ' ';

			if (temp.dimensions == 1) {
				ret += '[' + temp.columns + '] ';

				switch (temp.type) {
					case Types.INTEGER:
					case Types.REAL:
						ret += '= {' + temp.value + '}';
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
								ret += LocalizedStrings.getUI("true");
							} else {
								ret += LocalizedStrings.getUI("false");
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
					case Types.REAL:
						ret += '= {';

						for (var j = 0; j < temp.rows; j++) {
							ret += '{' + temp.value[j] + '}';

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
									ret += LocalizedStrings.getUI("true");
								} else {
									ret += LocalizedStrings.getUI("false");
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
					case Types.REAL:
						ret += '= ' + temp.value;
						break;
					case Types.TEXT:
						ret += '= "' + temp.value + '"';
						break;
					case Types.BOOLEAN:
						ret += '= ';
						if (temp.value) {
							ret += LocalizedStrings.getUI("true");;
						} else {
							ret += LocalizedStrings.getUI("false");;
						}
						break;
				}

			}

			

		}
	}

	return ret;
}
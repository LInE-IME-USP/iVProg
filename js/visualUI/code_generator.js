import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals';
import * as VariablesManagement from './variables';
import * as CommandsManagement from './commands';

export function generate () {

	var code = 'programa \n{ ';

	code += globalsCode();

	code += '\n';

	for (var i = 0; i < window.program_obj.functions.length; i ++) {
		code += functionsCode(window.program_obj.functions[i]);
		code += '\n';

	}

	code += '\n}';

	return code;

}

function functionsCode (function_obj) {
	var ret = '\n\tfuncao ';

	switch (function_obj.return_type) {
		case Types.INTEGER:
			ret += 'inteiro ';
			break;
		case Types.REAL:
			ret += 'real ';
			break;
		case Types.TEXT:
			ret += 'texto ';
			break;
		case Types.BOOLEAN:
			ret += 'booleano ';
			break;
		case Types.VOID:
			ret += 'vazio ';
			break;
	}

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

	ret += ' ) ';
	ret += '\n\t{';

	for (var j = 0; j < function_obj.variables_list.length; j++) {
		ret += variablesCode(function_obj.variables_list[j]);
	}

	for (var j = 0; j < function_obj.commands.length; j++) {
		ret += commandsCode(function_obj.commands[j]);
	}

	ret += '\n\t}';


	return ret;
}

function commandsCode (command_obj) {
	switch (command_obj.type) {
		case Models.COMMAND_TYPES.comment:
			return commentsCode(command_obj);

		case Models.COMMAND_TYPES.reader:
			return readersCode(command_obj);

		case Models.COMMAND_TYPES.writer:
			return writersCode(command_obj);
	}
}

function readersCode (command_obj) {
	var ret = '\n\t\tleia ( ';
	
	ret += command_obj.variable_value_menu.content.name;

	if (command_obj.variable_value_menu.content.dimensions == 1) {
		ret += ' [ ' + command_obj.column + ' ] ';
	} else if (command_obj.variable_value_menu.content.dimensions == 2) {
		ret += ' [ ' + command_obj.row + ' ] [ ' + command_obj.column + ' ]';
	}

	ret += ' ) ';
	return ret;
}

function writersCode (command_obj) {
	var ret = '\n\t\tescreva ( ';
	
	ret += command_obj.content[0].content.name;

	if (command_obj.content[0].content.dimensions == 1) {
		ret += ' [ ' + command_obj.content[0].column + ' ] ';
	} else if (command_obj.content[0].dimensions == 2) {
		ret += ' [ ' + command_obj.content[0].row + ' ] [ ' + command_obj.content[0].column + ' ]';
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
			ret += ' inteiro ';
			break;
		case Types.REAL:
			ret += ' real ';
			break;
		case Types.TEXT:
			ret += ' texto ';
			break;
		case Types.BOOLEAN:
			ret += ' booleano ';
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
			ret += 'inteiro ';
			break;
		case Types.REAL:
			ret += 'real ';
			break;
		case Types.TEXT:
			ret += 'texto ';
			break;
		case Types.BOOLEAN:
			ret += 'booleano ';
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
						ret += "verdadeiro";
					} else {
						ret += "falso";
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
							ret += "verdadeiro";
						} else {
							ret += "falso";
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
					ret += "verdadeiro";
				} else {
					ret += "falso";
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
					ret += 'inteiro ';
					break;
				case Types.REAL:
					ret += 'real ';
					break;
				case Types.TEXT:
					ret += 'texto ';
					break;
				case Types.BOOLEAN:
					ret += 'booleano ';
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
								ret += "verdadeiro";
							} else {
								ret += "falso";
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
									ret += "verdadeiro";
								} else {
									ret += "falso";
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
							ret += "verdadeiro";
						} else {
							ret += "falso";
						}
						break;
				}

			}

			

		}
	}

	return ret;
}
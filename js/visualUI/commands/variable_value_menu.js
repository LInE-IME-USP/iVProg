import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as AttribuitionsManagement from './attribution';
import * as WritersManagement from './writer';
import * as RepeatNTimesManagement from './repeatNtimes';

export const VAR_OR_VALUE_TYPES = Object.freeze({only_variable: 1, only_value: 2, only_function: 3, variable_and_function: 4, variable_and_value_opt: 5,
	value_and_function: 6, all: 7});

export function renderMenu (command, ref_object, dom_object, function_obj, size_field = 2, expression_element) {
	var menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom" data-algo="12"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';

	// Verificar se o objeto atual trata-se de uma chamada de função e conferir se possui a quantidade correta de parâmetros
	// Caso não possua, tem que adicionar as variáveis que servirão de parâmetros:


	if (ref_object.function_called) {
		if (ref_object.function_called.parameters_list) {

			if (!ref_object.parameters_list) {
				ref_object.parameters_list = [];
			}

			while (ref_object.function_called.parameters_list.length != ref_object.parameters_list.length) {
				if (ref_object.parameters_list.length > ref_object.function_called.parameters_list.length) {
					ref_object.parameters_list.pop();
				} else {
					ref_object.parameters_list.push(new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true));
				}
			}
		}
	}

	if (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_variable) {

		menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom"><div class="text"></div><i class="dropdown icon"></i><div class="menu menu_only_vars">';
		menu_var_or_value += '</div>';
	} 

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_value_opt) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {
		
		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('variable');
		menu_var_or_value += '<div class="menu menu_only_vars">';
		menu_var_or_value += '</div></div>';
	}

	if (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_value) {
		menu_var_or_value = '<input type="text" class="width-dynamic" size="'+size_field+'" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" />';
	}

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_value_opt) 
		|| (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.value_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {

		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_value+'">'+LocalizedStrings.getUI('text_value')+'</div>';

	}

	if (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_function) {

		menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom"><div class="text"></div><i class="dropdown icon"></i><div class="menu menu_only_functions">';
		menu_var_or_value += '</div>';
	} 

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_function) 
		|| (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.value_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {
		
		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_function+'"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('btn_function');
		menu_var_or_value += '<div class="menu menu_only_functions">';
		menu_var_or_value += '</div></div>';

		if (command.type == Models.COMMAND_TYPES.attribution) {
			menu_var_or_value += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
			menu_var_or_value += '<div class="menu">';
			menu_var_or_value += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
			menu_var_or_value += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
			menu_var_or_value += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
			menu_var_or_value += '</div></div>';
		}
	}

    menu_var_or_value += '</div></div>';

    menu_var_or_value = $(menu_var_or_value);

    dom_object.append(menu_var_or_value);

    addHandlers(command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element);

    addVariablesToMenu(function_obj, menu_var_or_value, ref_object, expression_element);

    addFunctionsToMenu(function_obj, menu_var_or_value, ref_object, expression_element);

	addIVProgFunctionsToMenu(function_obj, menu_var_or_value, ref_object, expression_element);

    if (ref_object.content || ref_object.function_called) {
    	// Verificar se a variável ainda existe:
    	if (isVarInProgram(ref_object.content, function_obj)) {
    		renderPreviousContent(function_obj, menu_var_or_value, ref_object, dom_object, command, expression_element);
    	} else {
    		if (ref_object.content && ref_object.content.type) {
    			ref_object.content = null;
    			appendSelectText(ref_object, menu_var_or_value);
    		} else {
    			renderPreviousContent(function_obj, menu_var_or_value, ref_object, dom_object, command, expression_element);
    		}
    	}
    } else {
    	appendSelectText(ref_object, menu_var_or_value);
    }

}

function appendSelectText (ref_object, menu_var_or_value) {
	switch(ref_object.variable_and_value) {
		case VAR_OR_VALUE_TYPES.only_variable:
			menu_var_or_value.find('.text').append('<i>'+LocalizedStrings.getUI('var_menu_select_var')+'</i>');
			break;
		case VAR_OR_VALUE_TYPES.all:
			menu_var_or_value.find('.text').append('<i>'+LocalizedStrings.getUI('var_menu_select_all')+'</i>');
			break;
		case VAR_OR_VALUE_TYPES.variable_and_function:
			menu_var_or_value.find('.text').append('<i>'+LocalizedStrings.getUI('var_menu_select_all')+'</i>');
			break;
		case VAR_OR_VALUE_TYPES.only_function:
			menu_var_or_value.find('.text').append('<i>'+LocalizedStrings.getUI('var_menu_select_function')+'</i>');
			break;
	}
}

function isVarInProgram (var_obj, function_obj) {
	// Verify in globals:
	if (window.program_obj.globals) {
		for (var i = 0; i < window.program_obj.globals.length; i++) {
			if (window.program_obj.globals[i] == var_obj) {
				return true;
			}
		}
	}
	// Verify in locals:
	if (function_obj.variables_list) {
		for (var i = 0; i < function_obj.variables_list.length; i++) {
			if (function_obj.variables_list[i] == var_obj) {
				return true;
			}
		}
	}
	// Verify in parameters:
	if (function_obj.parameters_list) {
		for (var i = 0; i < function_obj.parameters_list.length; i++) {
			if (function_obj.parameters_list[i] == var_obj) {
				return true;
			}
		}
	}
	return false;
}

export function refreshMenu (menu_var_or_value_dom) {
	console.log('\n\n');
	console.log(menu_var_or_value_dom);
	console.log("olá, fui chamado! note alguns DATAS recuperados: ");
	console.log(menu_var_or_value_dom.data());
	console.log('\n\n\n');

}

function renderPreviousContent (function_obj, menu_var_or_value, ref_object, dom_object, command, expression_element) {

	
	if (ref_object.function_called) {

		menu_var_or_value.remove();
		variableValueMenuCode(command, ref_object, dom_object, function_obj, menu_var_or_value, expression_element);

	} else if (ref_object.content.type) { 

		menu_var_or_value.remove();
		variableValueMenuCode(command, ref_object, dom_object, function_obj, menu_var_or_value, expression_element);

	} else {

		menu_var_or_value.remove();
		variableValueMenuCode(command, ref_object, dom_object, function_obj, menu_var_or_value, expression_element);

	}
}

function variableValueMenuCode (command, variable_obj, dom_object, function_obj, menu_var_or_value, expression_element) {
	
	if (variable_obj.content || variable_obj.function_called) {
    	// Verificar se a variável ainda existe:
    	if (isVarInProgram(variable_obj.content, function_obj)) {
    		
    	} else {
    		if (variable_obj.content && variable_obj.content.type) {
    			variable_obj.content = null;
    			appendSelectText(variable_obj, menu_var_or_value);
    		}
    	}
    } else {
    	appendSelectText(variable_obj, menu_var_or_value);
    }

	if (variable_obj.content == null && variable_obj.function_called == null) {
		renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
		return;
	}

	var ret = '';
	if (variable_obj.function_called) {

		if (variable_obj.function_called.parameters_list == null || variable_obj.function_called.length == 0) {

			menu_var_or_value.find('.text').text(' ');
			dom_object.find('.menu_var_or_value_dom').remove();

			var parameters_menu;
			
			if (variable_obj.function_called.name) {
				parameters_menu = '<div class="parameters_function_called"> '+variable_obj.function_called.name+' <span> ( </span>';
			} else {
				parameters_menu = '<div class="parameters_function_called"> <i>'+LocalizedStrings.getUI(variable_obj.function_called.category)+'.'+LocalizedStrings.getUI(variable_obj.function_called.identifier)+'</i> <span> ( </span>';
			}

			parameters_menu += '<span> ) </span></div>';

			parameters_menu = $(parameters_menu);

			dom_object.append(parameters_menu);

			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

			if (command.type == Models.COMMAND_TYPES.attribution) {
				context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
				context_menu += '<div class="menu">';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
				context_menu += '</div></div>';
			}

			context_menu += '</div></div>';

			context_menu = $(context_menu);

			context_menu.insertAfter( dom_object.find('.parameters_function_called') );

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
				 console.log('S1');
			     if ($selectedItem.data('clear')) {
			     	console.log('PP1');
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
			     	delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
			     }

			     if ($selectedItem.data('exp')) {
			     	AttribuitionsManagement.manageExpressionElements(command, variable_obj, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
			     }
		      }
			});
		} else {

			menu_var_or_value.find('.text').text(' ');
			dom_object.find('.menu_var_or_value_dom').remove();

			var parameters_menu;

			if (variable_obj.function_called.name) {
				parameters_menu = '<div class="parameters_function_called"> '+variable_obj.function_called.name+' <span> ( </span>';
			} else {
				parameters_menu = '<div class="parameters_function_called"> <i>'+LocalizedStrings.getUI(variable_obj.function_called.category)+'.'+LocalizedStrings.getUI(variable_obj.function_called.identifier)+'</i> <span> ( </span>';
			}

			for (var j = 0; j < variable_obj.function_called.parameters_list.length; j++) {
				parameters_menu += '<div class="render_style_param parameter_'+j+'"></div>';
				if ((j + 1) != variable_obj.function_called.parameters_list.length) {
					parameters_menu += ' , ';
				}
			}
			parameters_menu += '<span> ) </span></div>';

			parameters_menu = $(parameters_menu);

			dom_object.append(parameters_menu);

			console.log("\nvariable_obj:");
			console.log(variable_obj);

			for (var j = 0; j < variable_obj.function_called.parameters_list.length; j++) {
				renderMenu(command, new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true), parameters_menu.find('.parameter_'+j), function_obj, 2, expression_element);
			}


			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

			if (command.type == Models.COMMAND_TYPES.attribution) {
				context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
				context_menu += '<div class="menu">';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
				context_menu += '</div></div>';
			}

			context_menu += '</div></div>';

			context_menu = $(context_menu);

			context_menu.insertAfter( parameters_menu );

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
					console.log('S2');
			     if ($selectedItem.data('clear')) {
			     	console.log('PP2');
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
			     	delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
			     }

			     if ($selectedItem.data('exp')) {
			     	AttribuitionsManagement.manageExpressionElements(command, variable_obj, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
			     }
		      }
			});
		}


	} else if (variable_obj.content.type) {

		var variable_render = "";

		if (variable_obj.content.dimensions == 1) {

			variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content.name+'</span>';

			variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ]</span>';
			
			variable_render += '</div>';

			variable_render = $(variable_render);

			dom_object.append(variable_render);


			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

			if (command.type == Models.COMMAND_TYPES.attribution) {
				context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
				context_menu += '<div class="menu">';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
				context_menu += '</div></div>';
			}

			context_menu += '</div></div>';

			context_menu = $(context_menu);

			variable_render.append(context_menu);

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
					console.log('S3');
			     if ($selectedItem.data('clear')) {
			     	console.log('PP3');
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
		     		delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
			     }

			     if ($selectedItem.data('exp')) {
			     	AttribuitionsManagement.manageExpressionElements(command, variable_obj, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
			     }
		      }
			});

			if (!variable_obj.column) {
				variable_obj.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
			}

			variableValueMenuCode(command, variable_obj.column, $(variable_render.find('.column_container')), function_obj, menu_var_or_value, expression_element);

		} else if (variable_obj.content.dimensions == 2) {

			variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content.name+'</span>';

			variable_render += ' <span>[ </span> <div class="row_container"></div> <span> ]</span>';
			variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ] </span>';
			
			variable_render += '</div>';

			variable_render = $(variable_render);

			dom_object.append(variable_render);


			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

			if (command.type == Models.COMMAND_TYPES.attribution) {
				context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
				context_menu += '<div class="menu">';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
				context_menu += '</div></div>';
			}

			context_menu += '</div></div>';

			context_menu = $(context_menu);

			variable_render.append(context_menu);

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
					console.log('S4');
			     if ($selectedItem.data('clear')) {
			     	console.log('PP4');
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
		     		delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
			     }

			     if ($selectedItem.data('exp')) {
			     	AttribuitionsManagement.manageExpressionElements(command, variable_obj, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
			     }
		      }
			});

			if (!variable_obj.column) {
				variable_obj.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
			}
			if (!variable_obj.row) {
				variable_obj.row = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
			}

			variableValueMenuCode(command, variable_obj.row, $(variable_render.find('.row_container')), function_obj, menu_var_or_value, expression_element);
			variableValueMenuCode(command, variable_obj.column, $(variable_render.find('.column_container')), function_obj, menu_var_or_value, expression_element);

		} else {

			variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content.name+'</span>';

			variable_render += '</div>';

			variable_render = $(variable_render);

			dom_object.append(variable_render);


			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

			if (command.type == Models.COMMAND_TYPES.attribution && !dom_object.hasClass('var_attributed')) {
				console.log('dom_object6');
				console.log(dom_object);
				context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
				context_menu += '<div class="menu">';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
				context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
				context_menu += '</div></div>';
			}

			context_menu += '</div></div>';

			context_menu = $(context_menu);

			variable_render.append(context_menu);

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
					console.log('S5');
			     if ($selectedItem.data('clear')) {
			     	console.log('PP5');
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;

			     	delete variable_obj.function_called;
		     		delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
			     }

			     if ($selectedItem.data('exp')) {
			     	AttribuitionsManagement.manageExpressionElements(command, variable_obj, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
			     }
		      }
			});

		}

	} else {

		var variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content+'</span>';
		variable_render += '</div>';

		variable_render = $(variable_render);

		dom_object.append(variable_render);

		var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
		context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

		if (command.type == Models.COMMAND_TYPES.attribution) {
			context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
			context_menu += '<div class="menu">';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
			context_menu += '</div></div>';
		}

		context_menu += '</div></div>';

		context_menu = $(context_menu);

		if (variable_obj.variable_and_value != VAR_OR_VALUE_TYPES.only_value) {
			context_menu.insertAfter( variable_render );
		}

		context_menu.dropdown({
			onChange: function(value, text, $selectedItem) {
				console.log('S6');
		     if ($selectedItem.data('clear')) {
		     	console.log('PP6');
		     	dom_object.text('');

   				variable_obj.content = null;
			    variable_obj.row = null;
			    variable_obj.column = null;

		     	delete variable_obj.function_called;
		     	delete variable_obj.parameters_list;

		     	dom_object.find('.value_rendered').remove();
				dom_object.find('.context_menu_clear').remove();
				dom_object.find('.width-dynamic-minus').remove();

		     	renderMenu(command, variable_obj, dom_object, function_obj, 2, expression_element);
		     }

		     if ($selectedItem.data('exp')) {
		     	AttribuitionsManagement.manageExpressionElements(command, variable_obj, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
		     }
	      }
		});

		variable_render.on('click', function(e) {
			variable_render.remove();
			variable_render.empty();
			variable_render.remove();
			dom_object.empty();
			dom_object.append('<span class="menu_var_or_value_dom"> </span>');
			
			openInputToValue(command, variable_obj, dom_object, menu_var_or_value, function_obj, expression_element);
		});
	}
}

function addIVProgFunctionsToMenu (function_obj, menu_var_or_value, ref_object, expression_element) {
	var sub_menu = menu_var_or_value.find('.menu_only_functions');
	sub_menu.append('<div class="divider"></div><div class="header">'+LocalizedStrings.getUI('text_header_ivprog_functions')+'</div>');
	sub_menu.append('<div class="item"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('text_menu_functions_math')+'<div class="menu menu_math_functions"></div></div>');
	sub_menu.append('<div class="item"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('text_menu_functions_text')+'<div class="menu menu_text_functions"></div></div>');
	sub_menu.append('<div class="item"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('text_menu_functions_arrangement')+'<div class="menu menu_arrangement_functions"></div></div>');
	sub_menu.append('<div class="item"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('text_menu_functions_conversion')+'<div class="menu menu_conversion_functions"></div></div>');

	// Insert Math functions:
	for (var i = 0; i < window.system_functions.length; i++) {
		var t = $('<div class="item"></div>');
		t.data('function_reference', window.system_functions[i]);
		t.data('option', VAR_OR_VALUE_TYPES.only_function);
		t.text(LocalizedStrings.getUI(window.system_functions[i].identifier));
		
		switch(window.system_functions[i].category) {
			case Models.SYSTEM_FUNCTIONS_CATEGORIES.math:
				sub_menu.find('.menu_math_functions').append(t);
				break;
		}	
	}
}

function addFunctionsToMenu (function_obj, menu_var_or_value, ref_object, expression_element) {
	var sub_menu = menu_var_or_value.find('.menu_only_functions');
	sub_menu.text('');

	for (var i = 0; i < window.program_obj.functions.length; i++) {
		var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_function+'">' + window.program_obj.functions[i].name + ' </div>');
		temp.data('function_reference', window.program_obj.functions[i]);
		sub_menu.append(temp);
	}
}

function addVariablesToMenu (function_obj, menu_var_or_value, ref_object, expression_element) {

	var sub_menu = menu_var_or_value.find('.menu_only_vars');
	sub_menu.text('');

	if (window.program_obj.globals) {

		if (ref_object.include_constant) {
			for (var i = 0; i < window.program_obj.globals.length; i++) {
				var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + window.program_obj.globals[i].name + ' </div>');
				temp.data('variable_reference', window.program_obj.globals[i]);
				sub_menu.append(temp);
			}
		} else {
			for (var i = 0; i < window.program_obj.globals.length; i++) {
				if (!window.program_obj.globals[i].is_constant) {
					var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + window.program_obj.globals[i].name + ' </div>');
					temp.data('variable_reference', window.program_obj.globals[i]);
					sub_menu.append(temp);
				}
			}
		}
	}

	if (function_obj.parameters_list) {
		for (var i = 0; i < function_obj.parameters_list.length; i++) {
			var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + function_obj.parameters_list[i].name + ' </div>');
			temp.data('variable_reference', function_obj.parameters_list[i]);
			sub_menu.append(temp);
		}
	}

	if (function_obj.variables_list) {
		for (var i = 0; i < function_obj.variables_list.length; i++) {
			var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + function_obj.variables_list[i].name + ' </div>');
			temp.data('variable_reference', function_obj.variables_list[i]);
			sub_menu.append(temp);
		}
	}

}

function addHandlers (command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element) {

	if (ref_object.variable_and_value != VAR_OR_VALUE_TYPES.only_value) {
		menu_var_or_value.dropdown({
		  onChange: function(value, text, $selectedItem) {
				console.log('S7');
				console.log(value);
				console.log(text);
				console.log($selectedItem);
		  	dom_object.find('.var_name').remove();
		     switch ($selectedItem.data('option')) {
		     	case VAR_OR_VALUE_TYPES.only_function:
		     		openInputToFunction(command, ref_object, dom_object, menu_var_or_value, function_obj, $($selectedItem).data('function_reference'), expression_element);
		     		break;

		     	case VAR_OR_VALUE_TYPES.only_value:
		     		openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element);
		     		break;

		     	case VAR_OR_VALUE_TYPES.only_variable:
		     		openInputToVariable(command, ref_object, dom_object, menu_var_or_value, function_obj, $($selectedItem).data('variable_reference'), expression_element);
		     		break;
		     }

		     if ($selectedItem.data('exp')) {
		     	AttribuitionsManagement.manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
		     }

		     if (command.type == Models.COMMAND_TYPES.repeatNtimes) {
		     	RepeatNTimesManagement.manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
		     }
	      }
	    });
	}

	dom_object.find('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    $(this).focus();

	    var tmpStr = $(this).val();
		$(this).val('');
		$(this).val(tmpStr);

	    $(this).css({
	        width: inputWidth
	    })
	}).trigger('input');


	if (command.type == Models.COMMAND_TYPES.comment) {
		dom_object.parent().on('click', function(e) {
			dom_object.find('.value_rendered').remove();
			dom_object.find('.value_rendered').empty();
			dom_object.find('.value_rendered').remove();
			dom_object.empty();
			dom_object.append('<span class="menu_var_or_value_dom"> </span>');
			
			openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element)
		});
	}
	
}

function openInputToFunction (command, ref_object, dom_object, menu_var_or_value, function_obj, function_selected, expression_element) {
	
	ref_object.function_called = function_selected;
	ref_object.parameters_list = [];


	if (function_selected.parameters_list != null && function_selected.parameters_list.length > 0) {

		menu_var_or_value.find('.text').text(' ');
		dom_object.find('.menu_var_or_value_dom').remove();

		var parameters_menu;
		if (function_selected.name) {
			parameters_menu = '<div class="parameters_function_called"> '+function_selected.name+' <span> ( </span>';
		} else {
			parameters_menu = '<div class="parameters_function_called"> <i>'+LocalizedStrings.getUI(function_selected.category)+'.'+LocalizedStrings.getUI(function_selected.identifier)+'</i> <span> ( </span>';
		}
		for (var j = 0; j < function_selected.parameters_list.length; j++) {
			parameters_menu += '<div class="render_style_param parameter_'+j+'"></div>';
			if ((j + 1) != function_selected.parameters_list.length) {
				parameters_menu += ' , ';
			}
		}
		parameters_menu += '<span> ) </span></div>';

		parameters_menu = $(parameters_menu);

		dom_object.append(parameters_menu);

		for (var j = 0; j < function_selected.parameters_list.length; j++) {
			var temp = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
			ref_object.parameters_list.push(temp);
			renderMenu(command, temp, parameters_menu.find('.parameter_'+j), function_obj, 2, expression_element);
		}


		var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
		context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

		if (command.type == Models.COMMAND_TYPES.attribution) {
			context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
			context_menu += '<div class="menu">';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
			context_menu += '</div></div>';
		}

		context_menu += '</div></div>';

		context_menu = $(context_menu);

		context_menu.insertAfter( dom_object.find('.parameters_function_called') );

		context_menu.dropdown({
			onChange: function(value, text, $selectedItem) {
				console.log('S8');
				console.log($selectedItem);
		     if ($selectedItem.data('clear')) {
		     	console.log('PP7');
		     	dom_object.text('');

		     	ref_object.content = null;
		     	ref_object.row = null;
		     	ref_object.column = null;
		     	delete ref_object.function_called;
		     	delete ref_object.parameters_list;

		     	renderMenu(command, ref_object, dom_object, function_obj, 2, expression_element);
		     }

		     if ($selectedItem.data('exp')) {
		     	AttribuitionsManagement.manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
		     }
	      }
		});

	} else {
		menu_var_or_value.find('.text').text(' ');
		dom_object.find('.menu_var_or_value_dom').remove();

		var parameters_menu;

		if (function_selected.name) {
			parameters_menu = '<div class="parameters_function_called"> '+function_selected.name+' <span> ( </span>';
		} else {
			parameters_menu = '<div class="parameters_function_called"> <i>'+LocalizedStrings.getUI(function_selected.category)+'.'+LocalizedStrings.getUI(function_selected.identifier)+'</i> <span> ( </span>';
		}
		
		parameters_menu += '<span> ) </span></div>';

		parameters_menu = $(parameters_menu);

		dom_object.append(parameters_menu);

		var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
		context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

		if (command.type == Models.COMMAND_TYPES.attribution) {
			context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
			context_menu += '<div class="menu">';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
			context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
			context_menu += '</div></div>';
		}

		context_menu += '</div></div>';

		context_menu = $(context_menu);

		context_menu.insertAfter( dom_object.find('.parameters_function_called') );

		context_menu.dropdown({
			onChange: function(value, text, $selectedItem) {
				console.log('S9');
		     if ($selectedItem.data('clear')) {
		     	console.log('PP8');
		     	dom_object.text('');

		     	ref_object.content = null;
		     	ref_object.row = null;
		     	ref_object.column = null;
		     	delete ref_object.function_called;
		     	delete ref_object.parameters_list;

		     	renderMenu(command, ref_object, dom_object, function_obj, 2, expression_element);
		     }

		     if ($selectedItem.data('exp')) {
		     	AttribuitionsManagement.manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
		     }
	      }
		});
	}

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj);
	}
}

function openInputToVariable (command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected, expression_element) {
	
	ref_object.content = variable_selected;

	menu_var_or_value.find('.text').text(' ');
	dom_object.find('.menu_var_or_value_dom').remove();

	var variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_selected.name+'</span>';

	if (variable_selected.dimensions == 1) {
		variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ]</span>';
	}
	if (variable_selected.dimensions == 2) {
		variable_render += ' <span>[ </span> <div class="row_container"></div> <span> ]</span> ';
		variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ]</span>';
	}


	variable_render += '</div>';

	variable_render = $(variable_render);

	dom_object.append(variable_render);

	if (variable_selected.dimensions == 1) {
		ref_object.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.column, variable_render.find('.column_container'), function_obj, 2, expression_element);
	}
	if (variable_selected.dimensions == 2) {
		ref_object.row = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.row, variable_render.find('.row_container'), function_obj, 2, expression_element);

		ref_object.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.column, variable_render.find('.column_container'), function_obj, 2, expression_element);
	}

	var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

	if (command.type == Models.COMMAND_TYPES.attribution && !dom_object.hasClass('var_attributed')) {
		console.log("dom_object 10: ");
		console.log(dom_object);
		context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
		context_menu += '<div class="menu">';
		context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
		context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
		context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
		context_menu += '</div></div>';
	}

	context_menu += '</div></div>';

	context_menu = $(context_menu);

	context_menu.insertAfter( dom_object.find('.variable_rendered') );

	context_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
			console.log('S10');
	     if ($selectedItem.data('clear')) {
	     	console.log('PP9');
	     	dom_object.text('');

	     	ref_object.content = null;
	     	ref_object.row = null;
	     	ref_object.column = null;

	     	delete ref_object.function_called;
		    delete ref_object.parameters_list;

	     	renderMenu(command, ref_object, dom_object, function_obj, 2, expression_element);
	     }

	     if ($selectedItem.data('exp')) {
	     	AttribuitionsManagement.manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
	     }

	     if (command.type == Models.COMMAND_TYPES.repeatNtimes) {
	     	RepeatNTimesManagement.manageClearExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
	     }
      }
	});

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected);
	}
}


function openInputToValue (command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element) {

	if (ref_object.content == null) {
		ref_object.content = "";
	}

	menu_var_or_value.find('.text').text(' ');
	var field = $('<input type="text" size="2" class="width-dynamic-minus" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" />');
	field.insertBefore(dom_object.find('.menu_var_or_value_dom'));
	var rendered = $('<div class="value_rendered"></div>');
	rendered.insertBefore(field);

	field.focus();
	field.val(ref_object.content);

	var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';

	if (command.type == Models.COMMAND_TYPES.attribution) {
		context_menu += '<div class="item"><i class="dropdown icon"></i>' + LocalizedStrings.getUI('text_change');
		context_menu += '<div class="menu">';
		context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
		context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
		context_menu += '<div class="item" data-exp="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
		context_menu += '</div></div>';
	}

	context_menu += '</div></div>';

	context_menu = $(context_menu);

	dom_object.find('.menu_var_or_value_dom').remove();

	if (ref_object.variable_and_value != VAR_OR_VALUE_TYPES.only_value) {
		context_menu.insertAfter( field );
	}

	context_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
			console.log('S11');
	     if ($selectedItem.data('clear')) {
	     	console.log('PP10');
	     	dom_object.text('');

	     	dom_object.find('.value_rendered').remove();
			dom_object.find('.context_menu_clear').remove();
			dom_object.find('.width-dynamic-minus').remove();

			ref_object.content = null;
		    ref_object.row = null;
		    ref_object.column = null;

			delete ref_object.function_called;
		    delete ref_object.parameters_list;

	     	renderMenu(command, ref_object, dom_object, function_obj, 2, expression_element);
	     }

	     if ($selectedItem.data('exp')) {
	     	AttribuitionsManagement.manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element);
	     }
      }
	});

	dom_object.find('.width-dynamic-minus').focusout(function() {
		if ($(this).val().trim()) {
			ref_object.content = $(this).val().trim();
		}

		rendered.text(ref_object.content);
		$(this).remove();

	});

	dom_object.find('.width-dynamic-minus').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				ref_object.content = $(this).val().trim();
			}
			rendered.text(ref_object.content);

			$(this).remove();
		}
		if(code == 27) {
			rendered.text(ref_object.content);

			$(this).remove();
		}
	});

	if (command.type == Models.COMMAND_TYPES.comment) {
		rendered.parent().on('click', function(e) {
			console.log("TTT14");
			rendered.remove();
			rendered.empty();
			rendered.remove();
			dom_object.empty();
			dom_object.append('<span class="menu_var_or_value_dom"> </span>');
			
			openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element)
		});
	}

	rendered.on('click', function(e) {
		console.log("TTT2");
		rendered.remove();
		rendered.empty();
		rendered.remove();
		dom_object.empty();
		dom_object.append('<span class="menu_var_or_value_dom"> </span>');
		
		openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj, expression_element)
	});

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj);
	}
}


$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};

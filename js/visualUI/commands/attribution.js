import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as VariableValueMenuManagement from './variable_value_menu';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui attribution created_element"> <i class="ui icon small arrow left"></i> <span> x = 1 + 1 </span></div>');
}

export function renderCommand (command, function_obj) {
	
	var el = $('<div class="ui attribution command_container"><i class="ui icon small arrow left command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="var_attributed"></div> <span class="text_attr_receives span_command_spec">'+LocalizedStrings.getUI('text_receives')+'</span> '
		 + '<div class="expression_elements"></div> </div>');
	el.data('command', command);

	VariableValueMenu.renderMenu(command, command.variable, el.find('.var_attributed'), function_obj);

	if (!command.expression || command.expression.length < 1) {
		var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.op_exp, [Models.ARITHMETIC_TYPES.none, 
		new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

		command.expression.push(exp);
	}

	addHandlers(command, function_obj, el);

	renderExpressionElements(command, function_obj, el);

	return el;

}

export function manageExpressionElements (command, ref_object, dom_object, menu_var_or_value, function_obj, selectedItem, expression_element) {
	
	var index_to_move = expression_element.itens.indexOf(ref_object);

	switch (selectedItem.data('exp')) {
		case Models.EXPRESSION_ELEMENTS.exp_op_exp:

			var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.exp_op_exp, [expression_element.itens[index_to_move],
	     		Models.ARITHMETIC_TYPES.plus, 
				new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

			expression_element.itens[index_to_move] = exp;

			break;

		case Models.EXPRESSION_ELEMENTS.op_exp:

			var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.op_exp, [Models.ARITHMETIC_TYPES.plus, 
				expression_element.itens[index_to_move] ]);

			expression_element.itens[index_to_move] = exp;

			break;

		case Models.EXPRESSION_ELEMENTS.par_exp_par:

			var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.par_exp_par, [expression_element.itens[index_to_move]]);

			expression_element.itens[index_to_move] = exp;

			break;
	}

	renderExpressionElements(command, function_obj, dom_object);

}

function renderExpressionElements (command, function_obj, el) {
	var expression_div = el.find('.expression_elements');
	var command_container;

	if (el.hasClass("command_container") == false) {
		var hier = el.parentsUntil(".command_container");
		for (var i = 0; i < hier.length; i++) {
			if ($(hier[i]).hasClass("command_container")) {
				command_container = $(hier[i]);
				break;
			}
			if ($(hier[i]).hasClass("expression_elements")) {
				expression_div = $(hier[i]);
				break;
			}
		}
	}

	if (command_container) {
		expression_div = command_container.find('.expression_elements');
	}

	expression_div.text('');

	for (var i = 0; i < command.expression.length; i++) {

		var temp = $('<div class="expression_element"></div>');
		temp.data('ref_element', command.expression[i]);
		temp.data('ref_index', i);

		expression_div.append(temp);

		renderElement(command, function_obj, temp, command.expression[i]);
	}
}

function renderOperator (command, function_obj, temp_op, expression_element, index_op) {

	var context_menu = '<div class="ui dropdown"><div class="text">';

	switch (expression_element.itens[index_op]) {
		case Models.ARITHMETIC_TYPES.plus:
			context_menu += '+';
			break;

		case Models.ARITHMETIC_TYPES.minus:
			context_menu += '-';
			break;

		case Models.ARITHMETIC_TYPES.multiplication:
			context_menu += '*';
			break;

		case Models.ARITHMETIC_TYPES.division:
			context_menu += '/';
			break;

		case Models.ARITHMETIC_TYPES.module:
			context_menu += '%';
			break;

		case Models.ARITHMETIC_TYPES.none:
			context_menu += '...';
			break;
	}
	
	context_menu += '</div><div class="menu">';
	context_menu += '<div class="item" data-value="'+Models.ARITHMETIC_TYPES.plus+'">+</div>';
	context_menu += '<div class="item" data-value="'+Models.ARITHMETIC_TYPES.minus+'">-</div>';
	context_menu += '<div class="item" data-value="'+Models.ARITHMETIC_TYPES.multiplication+'">*</div>';
	context_menu += '<div class="item" data-value="'+Models.ARITHMETIC_TYPES.division+'">/</div>';
	context_menu += '<div class="item" data-value="'+Models.ARITHMETIC_TYPES.module+'">%</div>';
	context_menu += '<div class="item" data-value="'+Models.ARITHMETIC_TYPES.none+'" data-text="...">Nenhum</div>';
	context_menu += '</div></div>';

	context_menu = $(context_menu);

	temp_op.append(context_menu);

	context_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
	     expression_element.itens[index_op] = value;
      }
	});

}

function renderMenuAddExpression (command, function_obj, el, dom_append_menu, expression_append_new_expression) {

	if (el.hasClass("command_container") == false) {
		var hier = el.parentsUntil(".commands_list_div");

		for (var i = 0; i < hier.length; i++) {
			if ($(hier[i]).hasClass("command_container")) {
				el = $(hier[i]);
				break;
			}
		}
	}

	if (dom_append_menu.hasClass("expression_elements") == false) {
		var hier = el.parentsUntil(".commands_list_div");

		for (var i = 0; i < hier.length; i++) {
			if ($(hier[i]).hasClass("expression_elements")) {
				dom_append_menu = $(hier[i]);
				break;
			}
		}
	}

	var context_menu = '<div class="ui dropdown"><div class="text"></div><i class="ui icon arrow alternate circle right outline"></i><div class="menu">';
	context_menu += '<div class="item" data-value="'+Models.EXPRESSION_ELEMENTS.exp_op_exp+'">EXP OP EXP</div>';
	context_menu += '<div class="item" data-value="'+Models.EXPRESSION_ELEMENTS.op_exp+'">OP EXP</div>';
	context_menu += '<div class="item" data-value="'+Models.EXPRESSION_ELEMENTS.par_exp_par+'">( EXP )</div>';
	context_menu += '</div></div>';

	context_menu = $(context_menu);

	dom_append_menu.append(context_menu);

	context_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
	     switch (value) {
	     	case Models.EXPRESSION_ELEMENTS.exp_op_exp:

	     	var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.exp_op_exp, [new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true),
	     		Models.ARITHMETIC_TYPES.plus, 
				new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

			expression_append_new_expression.push(exp);
			break;

		case Models.EXPRESSION_ELEMENTS.op_exp:
			var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.op_exp, [Models.ARITHMETIC_TYPES.plus, 
				new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

			expression_append_new_expression.push(exp);
			break;

		case Models.EXPRESSION_ELEMENTS.par_exp_par:

			var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.par_exp_par, [new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

			expression_append_new_expression.push(exp);

			break;
		}
		
		renderExpressionElements(command, function_obj, el);
      }
	});

}

function renderElement (command, function_obj, el, expression_element) {

	switch (expression_element.type_exp) {
		case Models.EXPRESSION_ELEMENTS.exp_op_exp:

			var temp_op = $('<div class="component_element"></div>');
			var temp_exp_1 = $('<div class="component_element"></div>');
			var temp_exp_2 = $('<div class="component_element"></div>');

			el.append(temp_exp_1);
			el.append(temp_op);
			el.append(temp_exp_2);
			
			if (expression_element.itens[0].type) {
				VariableValueMenu.renderMenu(command, expression_element.itens[0], temp_exp_1, function_obj, 2, expression_element);
			} else {
				renderElement(command, function_obj, temp_exp_1, expression_element.itens[0]);
			}

			renderOperator(command, function_obj, temp_op, expression_element, 1);

			if (expression_element.itens[2].type) {
				VariableValueMenu.renderMenu(command, expression_element.itens[2], temp_exp_2, function_obj, 2, expression_element);
			} else {
				renderElement(command, function_obj, temp_exp_2, expression_element.itens[2]);
			}

			break;

		case Models.EXPRESSION_ELEMENTS.op_exp:
			var temp_op = $('<div class="component_element"></div>');
			var temp_exp = $('<div class="component_element"></div>');

			el.append(temp_op);
			el.append(temp_exp);

			renderOperator(command, function_obj, temp_op, expression_element, 0);

			if (expression_element.itens[1].type) {
				VariableValueMenu.renderMenu(command, expression_element.itens[1], temp_exp, function_obj, 2, expression_element);
			} else {
				renderElement(command, function_obj, temp_exp, expression_element.itens[1]);
			}
			break;

		case Models.EXPRESSION_ELEMENTS.par_exp_par:

			var temp_par_1 = $('<div class="component_element"> ( </div>');
			var temp_exp = $('<div class="component_element"></div>');
			var temp_par_2 = $('<div class="component_element"> ) </div>');

			el.append(temp_par_1);
			el.append(temp_exp);

			for (var j = 0; j < expression_element.itens.length; j++) {
				if (expression_element.itens[j].type) {
					VariableValueMenu.renderMenu(command, expression_element.itens[j], temp_exp, function_obj, 2, expression_element);
				} else {
					renderElement(command, function_obj, temp_exp, expression_element.itens[j]);
				}
			}
			

			//renderMenuAddExpression(command, function_obj, el, el, expression_element.itens);

			el.append(temp_par_2);

			break;
	}

}


function renderExpression (command, function_obj, el) {

	var expression_div = el.find('.expression_elements');
	expression_div.text('');

	var menu_add_item = $('<div class="menu_add_item"></div>');
	menu_add_item.data('index_add', 0);

	expression_div.append(menu_add_item);
	
	for (var i = 0; i < command.expression.length; i++) {

		if (command.expression[i].type) {

			var temp = $('<div class="expression_element"></div>');
			temp.data('ref_element', command.expression[i]);
			temp.data('ref_index', i);

			expression_div.append(temp);

			VariableValueMenu.renderMenu(command, command.expression[i], temp, function_obj);

		} else if (command.expression[i] == "(" || command.expression[i] == ")") {

			var temp = $('<div class="expression_element">'+command.expression[i]+'</div>');
			temp.data('ref_element', command.expression[i]);
			temp.data('ref_index', i);

			expression_div.append(temp);

		} else {

			var temp = '<div class="expression_element">';

			switch(command.expression[i]) {
				case Models.ARITHMETIC_TYPES.plus:
					temp += '+';
					break;
				case Models.ARITHMETIC_TYPES.minus:
					temp += '-';
					break;
				case Models.ARITHMETIC_TYPES.multiplication:
					temp += '*';
					break;
				case Models.ARITHMETIC_TYPES.division:
					temp += '/';
					break;
				case Models.ARITHMETIC_TYPES.module:
					temp += '%';
					break;
			}

			temp += '</div>';
			temp = $(temp);
			temp.data('ref_element', command.expression[i]);
			temp.data('ref_index', i);

			expression_div.append(temp);

		}
		
		var menu_add_item_seq = $('<div class="menu_add_item"></div>');
		var index_temp = (i + 1);
		menu_add_item_seq.data('index_add', index_temp);
		expression_div.append(menu_add_item_seq);

	}

	addMenuItens(command, function_obj, el);

}

function addMenuItens (command, function_obj, expression_div) {
	var divs_expression = expression_div.find('.menu_add_item');

	for (var i = 0; i < divs_expression.length; i++) {

		var temp = $(divs_expression[i]).data('index_add');

		var context_menu = '<div class="ui dropdown context_menu_clear"><i class="ui icon plus square outline"></i><div class="menu">';
		context_menu += '<div class="item" data-option="value" data-index="'+temp+'">'+LocalizedStrings.getUI('text_value')+'</div>';
		context_menu += '<div class="item" data-option="operator" data-index="'+temp+'">'+LocalizedStrings.getUI('text_operator')+'</div>';
		context_menu += '<div class="item" data-option="parentheses" data-index="'+temp+'">'+LocalizedStrings.getUI('text_parentheses')+'</div>';
		context_menu += '</div></div>';

		context_menu = $(context_menu);

		$(divs_expression[i]).append(context_menu);

		context_menu.dropdown({
	      on: 'hover',
	      onChange: function(value, text, $selectedItem) {
    		switch ($selectedItem.data('option')) {
    			case "value":
    				command.expression.splice($selectedItem.data('index'), 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
    				renderExpression(command, function_obj, expression_div);
    				break;
    			case "operator":
    				command.expression.splice($selectedItem.data('index'), 0, Models.ARITHMETIC_TYPES.plus);
    				renderExpression(command, function_obj, expression_div);
    				break;
    			case "parentheses":
    				command.expression.splice($selectedItem.data('index'), 0, "(");
    				command.expression.splice($selectedItem.data('index') + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
    				command.expression.splice($selectedItem.data('index') + 2, 0, ")");
    				renderExpression(command, function_obj, expression_div);
    				break;
    		 }
        	}
	    });

	}
}


function addHandlers (command, function_obj, attribution_dom) {

	attribution_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, attribution_dom)) {
			attribution_dom.fadeOut(400, function() {
				attribution_dom.remove();
			});
		}
	});

	attribution_dom.find('.button_refresh_attribution').on('click', function() {
		renderExpressionElements(command, function_obj, attribution_dom);
	});
}

export function renderMenuOperations (command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected) {

	/*console.log("recebido o seguinte DOM: ");
	console.log(dom_object);

	if (dom_object.hasClass('var_attributed')) {
		return;
	} else {
		var hier = dom_object.parentsUntil(".command_container");
		for (var i = 0; i < hier.length; i++) {
			if ($(hier[i]).hasClass('var_attributed') || $(hier[i]).hasClass('parameters_function_called')) {
				return;
			}
		}


		
	}

	dom_object.find('.context_menu_clear').remove();


	var menu_operations = '<div class="ui dropdown menu_operations"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';

	for (var tm in Models.ARITHMETIC_TYPES) {

		menu_operations += '<div class="item" data-option="'+tm+'">'+LocalizedStrings.getUI('btn_arithmetic_' + tm)+'</div>';
    }
    menu_operations += '<div class="item" data-option="clear">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	
    menu_operations += '</div></div>';

    menu_operations = $(menu_operations);

    dom_object.append(menu_operations);

    menu_operations.dropdown({
    	onChange: function(value, text, $selectedItem) {
    		switch ($($selectedItem).data('option')) {
    			case "clear":
	    			$(dom_object).text('');
		     	 	VariableValueMenu.renderMenu(command, ref_object, dom_object, function_obj);
    				break;
    			default:
    				createExpressionAround(command, ref_object, dom_object, function_obj);
    				menu_operations.find('.text').text('');
    		}
        }
    });*/
}

function createExpressionAround (command, ref_object, dom_object, function_obj) {
	$('<span> ( </span>').insertBefore(dom_object);
	$('<span> ) </span>').insertAfter(dom_object);

	VariableValueMenu.renderMenu(command, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), dom_object, function_obj);
}


import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as CommandsManagement from '../commands';
import * as ConditionalExpressionManagement from './conditional_expression';
import * as VariableValueMenu from './variable_value_menu';
import * as ContextualizedMenu from './contextualized_menu';

export function createFloatingCommand () {
	return $('<div class="ui repeatNtimes created_element"> <i class="ui icon small sync"></i> <span> para (x = 0; x < 10; x ++) { } </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '<div class="ui repeatNtimes command_container"> <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="ui context_menu"></div> <span> ' + LocalizedStrings.getUI('text_for') + ' ( </span>  <div class="ui attribution_expression"><div class="ui variable_attribution"></div> <span class="text_receives"></span> <div class="ui var_value_expression"></div> </div> ; <div class="conditional_expression"></div> ;  <div class="ui incrementation_field"><div class="ui incrementation_variable"></div> <span class="text_inc_receives"></span> <div class="ui first_operand"></div><div class="ui operator"></div><div class="ui second_operand"></div></div> ) { </span>';
	ret += '<div class="ui block_commands">';

	ret += '</div>';
	ret += '<span> }</span>';
	ret += '</div>';
	
	var el = $(ret);
	el.data('command', command);

	addHandlers(command, function_obj, el);

	ContextualizedMenu.renderMenu(command, el.find('.context_menu'), function_obj, el);

	VariableValueMenu.renderMenu(command, command.var_attribution, el.find('.variable_attribution'), function_obj);

	ConditionalExpressionManagement.renderExpression(command, command.expression2, function_obj, el.find('.conditional_expression'));

	VariableValueMenu.renderMenu(command, command.var_incrementation, el.find('.incrementation_variable'), function_obj);	

	return el;
}

export function manageExpressionElements (command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element) {

	if (dom_object.hasClass('variable_attribution')) {
		$(dom_object).parent().find('.text_receives').text(LocalizedStrings.getUI('text_receives'));

		command.expression1 = new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true);

		$(dom_object).parent().find('.var_value_expression').empty();
		
		VariableValueMenu.renderMenu(command, command.expression1, $(dom_object).parent().find('.var_value_expression'), function_obj);
	}

	if (dom_object.hasClass('incrementation_variable')) {
		$(dom_object).parent().find('.text_inc_receives').text(LocalizedStrings.getUI('text_receives'));
		
		var exp = new Models.ExpressionElement(Models.EXPRESSION_ELEMENTS.exp_op_exp, 
				[new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true),
	     		Models.ARITHMETIC_TYPES.plus, 
				new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

		command.expression3 = exp;

		VariableValueMenu.renderMenu(command, command.expression3.itens[0], $(dom_object).parent().find('.first_operand'), function_obj);

		renderOperator(command, function_obj, $(dom_object).parent().find('.operator'), command.expression3, 1);

		VariableValueMenu.renderMenu(command, command.expression3.itens[2], $(dom_object).parent().find('.second_operand'), function_obj);		

	}

}


export function manageClearExpressionElements (command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element) {

	if (dom_object.hasClass('variable_attribution')) {
		$(dom_object).parent().find('.text_receives').text('');
		command.expression1 = null;
		$(dom_object).parent().find('.var_value_expression').empty();
	}

	if (dom_object.hasClass('incrementation_variable')) {
		$(dom_object).parent().find('.text_inc_receives').text('');
		command.expression3 = null;
		$(dom_object).parent().find('.first_operand').empty();
		$(dom_object).parent().find('.operator').empty();
		$(dom_object).parent().find('.second_operand').empty();
	}
}

function addHandlers (command, function_obj, repeatNtimes_dom) {

	repeatNtimes_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, repeatNtimes_dom)) {
			repeatNtimes_dom.remove();
		}
	});
}

function renderOperator (command, function_obj, temp_op, expression_element, index_op) {

	var menu_operator = $('<div class="ui dropdown"><div class="text"></div></div>');
	menu_operator.dropdown({
	    values: [
	      {
	        name     : '+',
	        value    : Models.ARITHMETIC_TYPES.plus,
	        selected : (expression_element.itens[index_op] == Models.ARITHMETIC_TYPES.plus)
	      },
	      {
	        name     : '-',
	        value    : Models.ARITHMETIC_TYPES.minus,
	        selected : (expression_element.itens[index_op] == Models.ARITHMETIC_TYPES.minus)
	      },
	      {
	        name     : '*',
	        value    : Models.ARITHMETIC_TYPES.multiplication,
	        selected : (expression_element.itens[index_op] == Models.ARITHMETIC_TYPES.multiplication)
	      },
	      {
	        name     : '/',
	        value    : Models.ARITHMETIC_TYPES.division,
	        selected : (expression_element.itens[index_op] == Models.ARITHMETIC_TYPES.division)
	      },
	      {
	        name     : '%',
	        value    : Models.ARITHMETIC_TYPES.module,
	        selected : (expression_element.itens[index_op] == Models.ARITHMETIC_TYPES.module)
	      }
	    ],
	    onChange: function(value, text, $selectedItem) {
	    	expression_element.itens[index_op] = value;
	    }
	  });
	temp_op.append(menu_operator);
}
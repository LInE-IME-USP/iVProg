import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as CommandsManagement from '../commands';
import * as ConditionalExpressionManagement from './conditional_expression';
import * as VariableValueMenu from './variable_value_menu';

export function createFloatingCommand () {
	return $('<div class="ui repeatNtimes created_element"> <i class="ui icon small sync"></i> <span> para (x = 0; x < 10; x ++) { } </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '<div class="ui repeatNtimes command_container"> <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i> <span> ' + LocalizedStrings.getUI('text_for') + ' ( </span>  <div class="ui attribution_expression"><div class="ui variable_attribution"></div> <span class="text_receives"></span> <div class="ui var_value_expression"></div> </div> ; <div class="conditional_expression"></div> ;  ??? ) { </span>';
	ret += '<div class="ui block_commands">';

	ret += '</div>';
	ret += '<span> }</span>';
	ret += '</div>';
	
	var el = $(ret);
	el.data('command', command);

	addHandlers(command, function_obj, el);

	VariableValueMenu.renderMenu(command, command.var_attribution, $(el).find('.variable_attribution'), function_obj);

	ConditionalExpressionManagement.renderExpression(command, command.expression2, function_obj, el.find('.conditional_expression'));

	return el;
}

export function manageExpressionElements(command, ref_object, dom_object, menu_var_or_value, function_obj, $selectedItem, expression_element) {
	console.log("debugging: ");
	console.log("command");
	console.log(command);
	console.log("ref_object");
	console.log(ref_object);
	console.log("dom_object");
	console.log(dom_object);
	console.log("menu_var_or_value");
	console.log(menu_var_or_value);
	console.log("function_obj");
	console.log(function_obj);
	console.log("$selectedItem");
	console.log($selectedItem);
	console.log("expression_element");
	console.log(expression_element);

	if (dom_object.hasClass('variable_attribution')) {
		$(dom_object).parent().find('.text_receives').text(LocalizedStrings.getUI('text_receives'));

		command.expression1 = new Models.VariableValueMenu(VariableValueMenu.VAR_OR_VALUE_TYPES.all, null, null, null, true);

		$(dom_object).parent().find('.var_value_expression').empty();
		
		VariableValueMenu.renderMenu(command, command.expression1, $(dom_object).parent().find('.var_value_expression'), function_obj);
	}

}

function addHandlers (command, function_obj, repeatNtimes_dom) {

	repeatNtimes_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, repeatNtimes_dom)) {
			repeatNtimes_dom.remove();
		}
	});
}
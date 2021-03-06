import { Types } from '../types';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as VariableValueMenu from './variable_value_menu';
import * as CommandsManagement from '../commands';
import * as GenericExpressionManagement from './generic_expression';

export function createFloatingCommand () {
	return $('<div class="ui return created_element"> <i class="ui icon small reply"></i> <span> '+LocalizedStrings.getUI('text_return')+' </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui return command_container"> <i class="ui icon small reply command_drag"></i> <i class="ui icon times red button_remove_command"></i> <span class="span_command_spec"> '+LocalizedStrings.getUI('text_return')+' </span>  <div class="expression_elements"></div></div>');
	el.data('command', command);

	addHandlers(command, function_obj, el);

	if (function_obj.return_type != Types.VOID) {
		//VariableValueMenu.renderMenu(command, command.variable_value_menu, el.find('.var_value_menu_div'), function_obj);
		GenericExpressionManagement.renderExpression(command, function_obj, el.find('.expression_elements'), command.variable_value_menu);
	} else {
		el.find('.expression_elements').remove();
		command.variable_value_menu = null;
	}
	
	return el;
}

function addHandlers (command, function_obj, return_dom) {

	return_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, return_dom)) {
			return_dom.fadeOut(400, function() {
				return_dom.remove();
			});
		}
	});
}

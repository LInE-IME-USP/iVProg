import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui return created_element"> <i class="ui icon small reply"></i> <span> '+LocalizedStrings.getUI('text_return')+' </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui return command_container"> <i class="ui icon small reply"></i> <i class="ui icon times red button_remove_command"></i> <span class="span_command_spec"> '+LocalizedStrings.getUI('text_return')+' </span>  <div class="var_value_menu_div"></div></div>');
	el.data('command', command);

	addHandlers(command, function_obj, el);

	VariableValueMenu.renderMenu(command, command.variable_value_menu, el.find('.var_value_menu_div'), function_obj);

	return el;
}

function addHandlers (command, function_obj, return_dom) {

	return_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, return_dom)) {
			return_dom.remove();
		}
	});
}

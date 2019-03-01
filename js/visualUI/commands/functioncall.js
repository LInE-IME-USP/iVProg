import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui functioncall created_element"> <i class="hand point right icon"></i> <span> funcao() </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui functioncall command_container"> <i class="hand point right icon command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="var_value_menu_div"></div> </div>');
	el.data('command', command);

	VariableValueMenu.renderMenu(command, command.function_called, el.find('.var_value_menu_div'), function_obj);

	addHandlers(command, function_obj, el);

	return el;
}

function addHandlers (command, function_obj, functioncall_dom) {

	functioncall_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, functioncall_dom)) {
			functioncall_dom.fadeOut(400, function() {
				functioncall_dom.remove();
			});
		}
	});
}
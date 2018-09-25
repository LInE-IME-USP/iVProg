import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui reader created_element"> <i class="ui icon small download"></i> <span> '+LocalizedStrings.getUI('text_command_read')+' var </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = '<div class="ui reader command_container"> <i class="ui icon small download command_drag"></i> <i class="ui icon times red button_remove_command"></i> <span>'+LocalizedStrings.getUI('text_command_read')+' ( </span> <div class="var_value_menu_div"></div> <span class="close_parentheses">)</span> </div>';
	
	el = $(el);
	el.data('command', command);

	VariableValueMenu.renderMenu(command, command.variable_value_menu, el.find('.var_value_menu_div'), function_obj);

	addHandlers(command, function_obj, el);

	return el;
}

function addHandlers (command, function_obj, reader_dom) {

	reader_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, reader_dom)) {
			reader_dom.remove();
		}
	});
}
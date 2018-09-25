import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui repeatNtimes created_element"> <i class="ui icon small sync"></i> <span> para (x = 0; x < 10; x ++) { } </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '<div class="ui repeatNtimes command_container"> <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i> <span> para (x = 0; x < 10; x ++) { </span>';
	ret += '<div class="ui block_commands">';

	ret += '</div>';
	ret += '<span> }</span>';
	ret += '</div>';
	
	var el = $(ret);
	el.data('command', command);

	addHandlers(command, function_obj, el);

	return el;
}



function addHandlers (command, function_obj, repeatNtimes_dom) {

	repeatNtimes_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, repeatNtimes_dom)) {
			repeatNtimes_dom.remove();
		}
	});
}
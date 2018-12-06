import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui comment created_element"> <i class="ui icon small quote left"></i> <span> '+LocalizedStrings.getUI('text_break')+' </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui comment command_container"> <i class="ui icon small quote left"></i> <i class="ui icon times red button_remove_command"></i> <span>'+LocalizedStrings.getUI('text_break')+'</span> </div>');
	el.data('command', command);

	addHandlers(command, function_obj, el);

	return el;
}

function addHandlers (command, function_obj, break_dom) {

	break_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, break_dom)) {
			break_dom.remove();
		}
	});
}

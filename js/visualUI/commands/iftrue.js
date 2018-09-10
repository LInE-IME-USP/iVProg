import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui iftrue created_element"> <i class="ui icon small random"></i> <span> if (x < 1) { } </span></div>');
}

export function renderCommand (command) {
	var ret = '';
	ret += '<div class="ui iftrue"> <i class="ui icon small random command_drag"></i> <span> if (x < 1) { </span>';
	ret += '<div class="ui block_commands" data-if="true">';

	/*if ((writer_obj.commands_block == null)
			|| (writer_obj.commands_block.length == 0)) {
	} else {
		for (ki = 0; ki < writer_obj.commands_block.length; ki ++) {
			ret += renderElementCommandGeneric(writer_obj.commands_block[ki], function_index, ki, iftrue_index, (fullpath + ',' + ki));
		}
	}*/

	ret += '</div>';
	ret += '<span> } else { </span>';

	ret += '<div class="ui block_commands" data-else="true">';

	/*if ((writer_obj.commands_else == null)
			|| (writer_obj.commands_else.length == 0)) {
	} else {
		for (ki = 0; ki < writer_obj.commands_else.length; ki ++) {
			ret += renderElementCommandGeneric(writer_obj.commands_else[ki], function_index, ki, iftrue_index, (fullpath + ',' + ki));
		}
	}*/

	ret += '</div>';

	ret += '<span> }</span>';
	ret += '</div>';

	var el = $(ret);
	$(el).data('command', command);
	return el;
}
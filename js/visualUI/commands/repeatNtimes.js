import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui repeatNtimes created_element"> <i class="ui icon small sync"></i> <span> para (x = 0; x < 10; x ++) { } </span></div>');
}

export function renderCommand (command) {
	var ret = '<div class="ui repeatNtimes"> <i class="ui icon small random command_drag"></i> <span> para (x = 0; x < 10; x ++) { </span>';
	ret += '<div class="ui block_commands">';

	ret += '</div>';
	ret += '<span> }</span>';
	ret += '</div>';
	
	var el = $(ret);
	$(el).data('command', command);
	return el;
}

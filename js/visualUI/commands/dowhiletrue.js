import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui dowhiletrue created_element"> <i class="ui icon small sync"></i> <span> faça {<br>} enquanto(x < 10) </span></div>');
}

export function renderCommand (command) {
	var ret = '';
	ret += '<div class="ui dowhiletrue"> <i class="ui icon small random command_drag"></i> <span> faça  { </span>';
	ret += '<div class="ui block_commands" data-subblock="" data-idcommand="">';
	ret += '</div>';
	ret += '<span> } enquanto (x < 10); </span>';
	ret += '</div>';

	var el = $(ret);
	$(el).data('command', command);
	return el;
}
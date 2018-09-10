import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui writer created_element"> <i class="ui icon small upload"></i> <span> '+LocalizedStrings.getUI('text_command_write')+' var </span></div>');
}

export function renderCommand (command) {
	var ret = '';
	ret += '<div class="ui writer"> <i class="ui icon small upload command_drag"></i> <span>'+LocalizedStrings.getUI('text_command_write')+' ( </span>';

	ret += '<span> ) </span></div>';

	var el = $(ret);
	$(el).data('command', command);
	return el;
}


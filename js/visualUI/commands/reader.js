import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui reader created_element"> <i class="ui icon small download"></i> <span> '+LocalizedStrings.getUI('text_command_read')+' var </span></div>');
}

export function renderCommand (command) {
	var ret = '<div class="ui reader"> <i class="ui icon small download command_drag"></i> <span>'+LocalizedStrings.getUI('text_command_read')+' ( </span><span class="close_parentheses">)</span> </div>';
	
	var el = $(ret);
	$(el).data('command', command);
	return el;
}


import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui functioncall created_element"> <i class="hand point right icon"></i> <span> funcao() </span></div>');
}

export function renderCommand (command) {
	var el = $('<div class="ui functioncall"> <i class="hand point right icon command_drag"></i> <span> funcao() </span></div>');
	$(el).data('command', command);
	return el;
}
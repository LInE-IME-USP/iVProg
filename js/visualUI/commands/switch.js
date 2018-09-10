import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui switch created_element"> <i class="ui icon small random"></i> <span> escolha (x) { <br> caso 1: <br> caso 2: <br> } </span></div>');
}

export function renderCommand (command) {
	var ret = '';
	ret += '<div class="ui switch"> <i class="ui icon small random command_drag" ></i> <span> escolha (x) { <br> caso 1: <br> caso 2: <br> }</span>';
	ret += '</div>';

	var el = $(ret);
	$(el).data('command', command);
	return el;
}


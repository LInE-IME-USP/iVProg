import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui attribution created_element"> <i class="ui icon small arrow left"></i> <span> x = 1 + 1 </span></div>');
}

export function renderCommand (command) {
	var el = $('<div class="ui attribution"> <i class="ui icon small arrow left command_drag"></i> <span> x =  1 + 1</span></div>');
	$(el).data('command', command);
	return el;
}
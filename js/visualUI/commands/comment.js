import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';

export function createFloatingCommand () {
	return $('<div class="ui comment created_element"> <i class="ui icon small quote left"></i> <span class="span_comment_text" "> '+LocalizedStrings.getUI('text_comment')+' </span></div>');
}

export function renderCommand (command) {
	var el = $('<div class="ui comment"> <i class="ui icon small quote left"></i> <span class="span_comment_text"> ' + command.comment_text + ' </span></div>');
	$(el).data('command', command);
	return el;
}

import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui comment created_element"> <i class="ui icon small quote left"></i> <span class="span_comment_text" "> '+LocalizedStrings.getUI('text_comment')+' </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui comment command_container"> <i class="ui icon small quote left"></i> <i class="ui icon times red button_remove_command"></i> <div class="var_value_menu_div"></div> <div class="div_comment_text">'+'</div> </div>');
	el.data('command', command);

	addHandlers(command, function_obj, el);

	renderTextComment(command, function_obj, el);

	return el;
}

function renderTextComment (command, function_obj, el) {
	VariableValueMenu.renderMenu(command, command.comment_text, el.find('.var_value_menu_div'), function_obj, 20);
}

function addHandlers (command, function_obj, comment_dom) {

	comment_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, comment_dom)) {
			comment_dom.remove();
		}
	});
}

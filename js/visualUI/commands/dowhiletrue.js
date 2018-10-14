import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as CommandsManagement from '../commands';
import * as ConditionalExpressionManagement from './conditional_expression';
import * as ContextualizedMenu from './contextualized_menu';

export function createFloatingCommand () {
	return $('<div class="ui dowhiletrue created_element"> <i class="ui icon small sync"></i> <span> faça {<br>} enquanto(x < 10) </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui dowhiletrue command_container"> <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="ui context_menu"></div> <span> ' + LocalizedStrings.getUI('text_code_do') + '  { </span>';
	ret += '<div class="ui block_commands" data-subblock="" data-idcommand="">';
	ret += '</div>';
	ret += '<span> } ' + LocalizedStrings.getUI('text_code_while') + ' </span> <div class="conditional_expression"></div>';
	ret += '</div>';

	var el = $(ret);
	el.data('command', command);

	addHandlers(command, function_obj, el);

	ContextualizedMenu.renderMenu(command, el.find('.context_menu'), function_obj, el);

	ConditionalExpressionManagement.renderExpression(command, command.expression, function_obj, el.find('.conditional_expression'));

	return el;
}


function addHandlers (command, function_obj, dowhiletrue_dom) {

	dowhiletrue_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, dowhiletrue_dom)) {
			dowhiletrue_dom.remove();
		}
	});
}
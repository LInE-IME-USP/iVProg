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
	return $('<div class="ui dowhiletrue created_element"> <i class="ui icon small sync"></i> <span> '+ LocalizedStrings.getUI('text_command_do') +' {<br>} ' + LocalizedStrings.getUI('text_code_while') +'(x < 10) </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui dowhiletrue command_container"> <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="ui context_menu"></div>  <span class="span_command_spec"> ' + LocalizedStrings.getUI('text_command_do') + ' </span>';
	ret += '<div class="ui block_commands" data-subblock="" data-idcommand="">';
	ret += '</div>';
	ret += ' <span class="span_command_spec"> ' + LocalizedStrings.getUI('text_code_while') + ' </span> <div class="conditional_expression"></div>';
	ret += '</div>';

	var el = $(ret);
	el.data('command', command);

	addHandlers(command, function_obj, el);

	ContextualizedMenu.renderMenu(command, el.find('.context_menu'), function_obj, el);

	ConditionalExpressionManagement.renderExpression(command, command.expression, function_obj, el.find('.conditional_expression'));

	if (command.commands_block) {
		for (var j = 0; j < command.commands_block.length; j++) {
		    CommandsManagement.renderCommand(command.commands_block[j], $(el.find('.block_commands')[0]), 3, function_obj);
		}
	}

	return el;
}


function addHandlers (command, function_obj, dowhiletrue_dom) {

	dowhiletrue_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, dowhiletrue_dom)) {
			dowhiletrue_dom.remove();
		}
	});
}
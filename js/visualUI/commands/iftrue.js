import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as CommandsManagement from '../commands';
import * as ConditionalExpressionManagement from './conditional_expression';

export function createFloatingCommand () {
	return $('<div class="ui iftrue created_element"> <i class="ui icon small random"></i> <span> if (x < 1) { } </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui iftrue command_container"><div class="ui" data-if="true">  <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i> <i class="ui icon redo alternate blue button_refresh_attribution"></i>';
	ret += '<span> ' + LocalizedStrings.getUI('text_if') + '</span>';
	ret += ' <div class="conditional_expression"></div>';
	ret += '<span> { </span> ';
	ret += '<div class="ui block_commands commands_if conditional_comands_block" data-if="true">';

	/*if ((writer_obj.commands_block == null)
			|| (writer_obj.commands_block.length == 0)) {
	} else {
		for (ki = 0; ki < writer_obj.commands_block.length; ki ++) {
			ret += renderElementCommandGeneric(writer_obj.commands_block[ki], function_index, ki, iftrue_index, (fullpath + ',' + ki));
		}
	}*/

	ret += '</div></div>';
	ret += '<div class="ui" data-else="true"> <span> } ' + LocalizedStrings.getUI('text_else') + ' { </span>';

	ret += '<div class="ui block_commands commands_else conditional_comands_block" data-else="true">';

	/*if ((writer_obj.commands_else == null)
			|| (writer_obj.commands_else.length == 0)) {
	} else {
		for (ki = 0; ki < writer_obj.commands_else.length; ki ++) {
			ret += renderElementCommandGeneric(writer_obj.commands_else[ki], function_index, ki, iftrue_index, (fullpath + ',' + ki));
		}
	}*/

	ret += '</div>';

	ret += '<span> }</span></div>';
	ret += '</div>';

	var el = $(ret);
	el.data('command', command);
	el.find('.block_commands').data('command', command);

	addHandlers(command, function_obj, el);

	ConditionalExpressionManagement.renderExpression(command, command.expression, function_obj, el.find('.conditional_expression'));

	el.find('.button_refresh_attribution').on('click', function() {
		el.find('.conditional_expression').empty();
		ConditionalExpressionManagement.renderExpression(command, command.expression, function_obj, el.find('.conditional_expression'));		
	});

	return el;
}


function addHandlers (command, function_obj, iftrue_dom) {

	iftrue_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, iftrue_dom)) {
			iftrue_dom.remove();
		}
	});
}
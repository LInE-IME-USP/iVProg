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
	ret += '<div class="ui iftrue command_container"><div class="ui data_block_if" data-if="true">  <i class="ui icon small random command_drag"></i> <i class="ui icon times red button_remove_command"></i>';
	ret += '<span class="span_command_spec"> ' + LocalizedStrings.getUI('text_if') + '</span>';
	ret += ' <span class="span_command_spec"> ( </span> <div class="conditional_expression"></div> <span class="span_command_spec"> ) </span>';
	ret += '<span> </span> ';
	ret += '<div class="ui block_commands commands_if conditional_comands_block" data-if="true">';
 	ret += '</div></div>';
	ret += '<div class="ui data_block_else" data-else="true"> <span class="span_command_spec"> ' + LocalizedStrings.getUI('text_else') + ' </span>';
	ret += '<div class="ui block_commands commands_else conditional_comands_block" data-else="true">';
	ret += '</div>';
	ret += '<span></span></div>';
	ret += '</div>';

	var el = $(ret);
	el.data('command', command);
	el.find('.block_commands').data('command', command);
	el.find('.data_block_if').data('command', command);
	el.find('.data_block_else').data('command', command);
	el.find('.commands_if').data('command', command);

	addHandlers(command, function_obj, el);

	ConditionalExpressionManagement.renderExpression(command, command.expression, function_obj, el.find('.conditional_expression'));

	if (command.commands_block) {
		for (var j = 0; j < command.commands_block.length; j++) {
		    CommandsManagement.renderCommand(command.commands_block[j], $(el.find('.commands_if')[0]), 3, function_obj);
		}
	}
	if (command.commands_else) {
		for (var j = 0; j < command.commands_else.length; j++) {
		    CommandsManagement.renderCommand(command.commands_else[j], $(el.find('.commands_else')[0]), 3, function_obj);
		}
	}

	return el;
}


function addHandlers (command, function_obj, iftrue_dom) {

	iftrue_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, iftrue_dom)) {
			iftrue_dom.fadeOut();
		}
	});
}
import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as CommandsManagement from '../commands';
import * as VariableValueMenu from './variable_value_menu';
import * as ContextualizedMenu from './contextualized_menu';

export function createFloatingCommand () {
	return $('<div class="ui switch created_element"> <i class="ui icon small random"></i> <span> '+LocalizedStrings.getUI('text_code_switch')+' ( x ) { <br> '+LocalizedStrings.getUI('text_code_case')+' 1: <br> '+LocalizedStrings.getUI('text_code_case')+' 2: <br> } </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui switch command_container"> <i class="ui icon small random command_drag" ></i> <i class="ui icon times red button_remove_command"></i> <div class="ui context_menu"></div> <span> '+LocalizedStrings.getUI('text_code_switch')+' ( <div class="ui variable_to_switch"></div> ) <div class="ui all_cases_div"></div></span>';
	ret += '</div>';

	var el = $(ret);
	el.data('command', command);

	addHandlers(command, function_obj, el);

	ContextualizedMenu.renderMenu(command, el.find('.context_menu'), function_obj, el);

	VariableValueMenu.renderMenu(command, command.variable, el.find('.variable_to_switch'), function_obj);

	if (command.cases) {
		for (var i = 0; i < command.cases.length; i++) {
			renderCase(command.cases[i], command, function_obj, el.find('.all_cases_div'));
		}
	}

	return el;
}

export function renderCase (switchcase, command, function_obj, el) {

	var casediv = $('<div class="ui case_div"><i class="ui icon times red button_remove_command"></i><span>'+LocalizedStrings.getUI('text_code_case')+'</span> <div class="ui variable_case"></div>: <div class="case_commands_block"></div></div>');

	VariableValueMenu.renderMenu(command, switchcase.variable_value_menu, casediv.find('.variable_case'), function_obj);

	casediv.data('switchcase', switchcase);
	casediv.find('.case_commands_block').data('switchcase', switchcase);

	el.append(casediv);

	if (switchcase.commands_block) {
		for (var j = 0; j < switchcase.commands_block.length; j++) {
		    CommandsManagement.renderCommand(switchcase.commands_block[j], $(casediv.find('.case_commands_block')[0]), 3, function_obj);
		}
	}

	casediv.find('.button_remove_command').on('click', function() {
		for (var i = 0; i < command.cases.length; i++) {
			if (switchcase == command.cases[i]) {
				delete command.cases[i];
				command.cases.splice(i, 1);
				casediv.remove();
				break;
			}
		}
	});

}

function addHandlers (command, function_obj, switch_dom) {

	switch_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, switch_dom)) {
			switch_dom.fadeOut(400, function() {
				switch_dom.remove();
			});
		}
	});
}
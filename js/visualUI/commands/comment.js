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
	var el = $('<div class="ui comment command_container"> <i class="ui icon small quote left"></i> <i class="ui icon times red button_remove_command"></i> <div class="var_value_menu_div"></div> <div class="div_comment_text">'+command.comment_text.content+'</div> </div>');
	el.data('command', command);

	addHandlers(command, function_obj, el);

	return el;
}

function addHandlers (command, function_obj, comment_dom) {

	comment_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, comment_dom)) {
			comment_dom.remove();
		}
	});

	comment_dom.find('.div_comment_text').on('click', function() {
		comment_dom.find('.div_comment_text').text('');
		VariableValueMenu.renderMenu(command, command.comment_text, comment_dom.find('.var_value_menu_div'), function_obj, 20);
		comment_dom.find('.width-dynamic').val(command.comment_text.content);

		comment_dom.find('.width-dynamic').focusout(function() {
			if ($(this).val().trim()) {
				command.comment_text.content = $(this).val().trim();
			}
			comment_dom.find('.div_comment_text').text(command.comment_text.content);
			$(this).remove();
		});

		comment_dom.find('.width-dynamic').on('keydown', function(e) {
			var code = e.keyCode || e.which;
			if(code == 13) {
				if ($(this).val().trim()) {
					command.comment_text.content = $(this).val().trim();
				}
				comment_dom.find('.div_comment_text').text(command.comment_text.content);
				$(this).remove();
			}
			if(code == 27) {
				comment_dom.find('.div_comment_text').text(command.comment_text.content);

				$(this).remove();
			}
		});

	});

}

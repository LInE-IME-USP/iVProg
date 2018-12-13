import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as VariableValueMenuManagement from './variable_value_menu';
import * as CommandsManagement from '../commands';

export function createFloatingCommand () {
	return $('<div class="ui writer created_element"> <i class="ui icon small upload"></i> <span> '+LocalizedStrings.getUI('text_command_write')+' var </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui writer command_container"> <i class="ui icon small upload command_drag"></i> <i class="ui icon times red button_remove_command"></i> <span class="span_command_spec">'+LocalizedStrings.getUI('text_command_write')+' ( </span><div class="all_elements_write"></div> <span class="close_parentheses span_command_spec">)</span> </div>';

	var el = $(ret);
	el.data('command', command);

	for (var i = 0; i < command.content.length; i ++) {
		var new_div_item = $( '<div class="var_value_menu_div"></div>' );
		el.find('.all_elements_write').append(new_div_item);
		VariableValueMenu.renderMenu(command, command.content[i], new_div_item, function_obj);

		addHandlerIconAdd(el.find('.all_elements_write'), command, function_obj);
	}
	addHandlers(command, function_obj, el);
	return el;
}

function addHandlers (command, function_obj, writer_dom) {

	writer_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, writer_dom)) {
			writer_dom.fadeOut();
		}
	});
}

function addHandlerIconAdd (dom_object, command, function_obj, insert_after = false, after_which = null) {
	var icon_add_item = $( '<i class="ui icon plus square outline icon_add_item_to_writer"></i> ' );
	if (!insert_after) {
		dom_object.append(icon_add_item);
	} else {
		icon_add_item.insertAfter(after_which);
	}
	
	icon_add_item.on('click', function(e) {
		var new_div_item = $( '<div class="var_value_menu_div" style="display:none;"></div>' );
		new_div_item.insertAfter(icon_add_item);
		var new_related_menu = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);

		VariableValueMenu.renderMenu(command, new_related_menu, new_div_item, function_obj);

		addHandlerIconAdd(dom_object, command, function_obj, true, new_div_item);
		var pos = 1;
		dom_object.find('.icon_add_item_to_writer').each(function() {
			if ($(this).get(0) === icon_add_item.get(0)) {
				command.content.splice(pos, 0, new_related_menu);
			} else {
				pos ++;
			}
		});
		new_div_item.fadeIn();
	});
}

export function addContent (command, ref_object, dom_object, menu_var_or_value, function_obj, ref_object_content) {
	
	if (dom_object.hasClass('var_value_menu_div')) {
		var icon_add_item = $( '<i class="ui icon plus square outline icon_add_item_to_writer"></i> ' );
		icon_add_item.insertAfter(dom_object);

		icon_add_item.on('click', function(e) {
			var new_div_item = $( '<div class="var_value_menu_div"></div>' );
			new_div_item.insertAfter(icon_add_item);
			var new_related_menu = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);

			VariableValueMenu.renderMenu(command, new_related_menu, new_div_item, function_obj);

			command.content.push(new_related_menu);
		});
	}
}
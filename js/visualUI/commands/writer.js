import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as VariableValueMenuManagement from './variable_value_menu';

export function createFloatingCommand () {
	return $('<div class="ui writer created_element"> <i class="ui icon small upload"></i> <span> '+LocalizedStrings.getUI('text_command_write')+' var </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui writer"> <i class="ui icon small upload command_drag"></i> <span>'+LocalizedStrings.getUI('text_command_write')+' ( </span><div class="var_value_menu_div"></div> <span class="close_parentheses">)</span> </div>';

	var el = $(ret);
	$(el).data('command', command);

	VariableValueMenu.renderMenu(command, command.content, $(el).find('.var_value_menu_div'), function_obj);

	return el;
}

export function addContent (command, ref_object, dom_object, menu_var_or_value, function_obj, ref_object_content) {
	
	if ($(dom_object).hasClass('var_value_menu_div')) {
		var icon_add_item = $( '<i class="ui icon plus square outline icon_add_item_to_writer"></i> ' );
		$(icon_add_item).insertAfter(dom_object);

		$( icon_add_item ).on('click', function(e) {
			var new_div_item = $( '<div class="var_value_menu_div"></div>' );
			$(new_div_item).insertAfter(icon_add_item);
			var new_related_menu = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);
			VariableValueMenu.renderMenu(command, new_related_menu, new_div_item, function_obj);
		});
	}
	
}
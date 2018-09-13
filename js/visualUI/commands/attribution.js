import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';
import * as VariableValueMenuManagement from './variable_value_menu';

export function createFloatingCommand () {
	return $('<div class="ui attribution created_element"> <i class="ui icon small arrow left"></i> <span> x = 1 + 1 </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui attribution"> <i class="ui icon small arrow left command_drag"></i>  <div class="var_attributed"></div> <span class="text_attr_receives">'+LocalizedStrings.getUI('text_receives')+'</span> '
		 + '<div class="expression_operand_1"></div> </div>');
	$(el).data('command', command);


	VariableValueMenu.renderMenu(command, command.variable, $(el).find('.var_attributed'), function_obj);

	VariableValueMenu.renderMenu(command, command.expression, $(el).find('.expression_operand_1'), function_obj);

	return el;
}

export function renderMenuOperations (command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected) {

	if ($(dom_object).hasClass('var_attributed')) {
		return;
	}

	$(dom_object).find('.context_menu_clear').remove();


	var menu_operations = '<div class="ui dropdown menu_operations"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';

	for (var tm in Models.ARITHMETIC_TYPES) {

		menu_operations += '<div class="item" data-option="'+tm+'">'+LocalizedStrings.getUI('btn_arithmetic_' + tm)+'</div>';
    }
    menu_operations += '<div class="item" data-option="clear">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	
    menu_operations += '</div></div>';

    menu_operations = $(menu_operations);

    $(dom_object).append(menu_operations);

    $(menu_operations).dropdown({
    	onChange: function(value, text, $selectedItem) {
    		switch ($($selectedItem).data('option')) {
    			case "clear":
	    			$(dom_object).text('');
		     	 	VariableValueMenu.renderMenu(command, ref_object, dom_object, function_obj);
    				break;
    			default:
    				createExpressionAround(command, ref_object, dom_object, function_obj);
    				$(menu_operations).find('.text').text('');
    		}
        }
    });
}

function createExpressionAround (command, ref_object, dom_object, function_obj) {
	$('<span> ( </span>').insertBefore(dom_object);
	$('<span> ) </span>').insertAfter(dom_object);

	VariableValueMenu.renderMenu(command, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), dom_object, function_obj);
}
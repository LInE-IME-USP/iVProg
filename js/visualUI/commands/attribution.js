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
	return $('<div class="ui attribution created_element"> <i class="ui icon small arrow left"></i> <span> x = 1 + 1 </span></div>');
}

export function renderCommand (command, function_obj) {

	var el = $('<div class="ui attribution command_container"> <i class="ui icon small arrow left command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="var_attributed"></div> <span class="text_attr_receives">'+LocalizedStrings.getUI('text_receives')+'</span> '
		 + '<div class="expression_operand_1"></div> </div>');
	el.data('command', command);

	VariableValueMenu.renderMenu(command, command.variable, el.find('.var_attributed'), function_obj);
	command.expression[0].content = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);
	VariableValueMenu.renderMenu(command, command.expression[0].content, el.find('.expression_operand_1'), function_obj);

	addHandlers(command, function_obj, el);

	return el;

	/*var el = $('<div class="ui attribution command_container"> <i class="ui icon small arrow left command_drag"></i> <i class="ui icon times red button_remove_command"></i> <div class="var_attributed"></div> <span class="text_attr_receives">'+LocalizedStrings.getUI('text_receives')+'</span> '
		 + '<div class="expression_operand_1"></div> </div>');
	$(el).data('command', command);


	VariableValueMenu.renderMenu(command, command.variable, $(el).find('.var_attributed'), function_obj);

	VariableValueMenu.renderMenu(command, command.expression, $(el).find('.expression_operand_1'), function_obj);

	addHandlers(command, function_obj, el);

	return el;*/
}

function addHandlers (command, function_obj, attribution_dom) {

	attribution_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, attribution_dom)) {
			attribution_dom.remove();
		}
	});
}

export function renderMenuOperations (command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected) {

	console.log("recebido o seguinte DOM: ");
	console.log(dom_object);

	if (dom_object.hasClass('var_attributed')) {
		return;
	} else {
		var hier = dom_object.parentsUntil(".command_container");
		for (var i = 0; i < hier.length; i++) {
			if ($(hier[i]).hasClass('var_attributed') || $(hier[i]).hasClass('parameters_function_called')) {
				return;
			}
		}


		
	}

	dom_object.find('.context_menu_clear').remove();


	var menu_operations = '<div class="ui dropdown menu_operations"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';

	for (var tm in Models.ARITHMETIC_TYPES) {

		menu_operations += '<div class="item" data-option="'+tm+'">'+LocalizedStrings.getUI('btn_arithmetic_' + tm)+'</div>';
    }
    menu_operations += '<div class="item" data-option="clear">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	
    menu_operations += '</div></div>';

    menu_operations = $(menu_operations);

    dom_object.append(menu_operations);

    menu_operations.dropdown({
    	onChange: function(value, text, $selectedItem) {
    		switch ($($selectedItem).data('option')) {
    			case "clear":
	    			$(dom_object).text('');
		     	 	VariableValueMenu.renderMenu(command, ref_object, dom_object, function_obj);
    				break;
    			default:
    				createExpressionAround(command, ref_object, dom_object, function_obj);
    				menu_operations.find('.text').text('');
    		}
        }
    });
}

function createExpressionAround (command, ref_object, dom_object, function_obj) {
	$('<span> ( </span>').insertBefore(dom_object);
	$('<span> ) </span>').insertAfter(dom_object);

	VariableValueMenu.renderMenu(command, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), dom_object, function_obj);
}


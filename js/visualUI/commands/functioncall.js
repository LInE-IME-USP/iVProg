import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenu from './variable_value_menu';

export function createFloatingCommand () {
	return $('<div class="ui functioncall created_element"> <i class="hand point right icon"></i> <span> funcao() </span></div>');
}

export function renderCommand (command, function_obj) {
	var el = $('<div class="ui functioncall"> <i class="hand point right icon command_drag"></i> <div class="var_value_menu_div"></div> </div>');
	$(el).data('command', command);

	VariableValueMenu.renderMenu(command, command.function_called, $(el).find('.var_value_menu_div'), function_obj);

	return el;
}
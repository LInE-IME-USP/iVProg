import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as CommandsManagement from '../commands';
import * as VariableValueMenuManagement from './variable_value_menu';
import * as SwitchManagement from './switch';

export function renderMenu (command, dom_where_render, function_obj, dom_command) {

	var menu_div = '<div class="ui dropdown menu_commands" ><i class="icon code"></i> <div class="menu"> ';

	if ((command.type == Models.COMMAND_TYPES.repeatNtimes) 
		|| (command.type == Models.COMMAND_TYPES.whiletrue) 
		|| (command.type == Models.COMMAND_TYPES.dowhiletrue)) {

		menu_div += '<a class="item" data-command="'+Models.COMMAND_TYPES.break+'"><i class="download icon"></i> '+LocalizedStrings.getUI('btn_break')+' </a>';

	} else {

		menu_div += '<a class="item" data-command="'+Models.COMMAND_TYPES.break+'"><i class="download icon"></i> '+LocalizedStrings.getUI('btn_break')+' </a>';
		menu_div += '<a class="item" data-command="'+Models.COMMAND_TYPES.switchcase+'"><i class="download icon"></i> '+LocalizedStrings.getUI('btn_case')+' </a>';

	}

	menu_div += '</div></div>';

	menu_div = $(menu_div);
  	
	dom_where_render.append(menu_div);

	addHandlers(command, dom_where_render, function_obj, dom_command);
}

function addHandlers (command, dom_where_render, function_obj, dom_command) {

	dom_where_render.find('.menu_commands').dropdown({
      on: 'hover'
    });
	
	dom_where_render.find('.menu_commands a').on('click', function(evt){

		if ((command.type == Models.COMMAND_TYPES.repeatNtimes) 
			|| (command.type == Models.COMMAND_TYPES.whiletrue) 
			|| (command.type == Models.COMMAND_TYPES.dowhiletrue)) {

				if (command.commands_block == null || command.commands_block.length == 0) {

			      command.commands_block = [];

			      var new_cmd = CommandsManagement.genericCreateCommand($(this).data('command'));
			      command.commands_block.push(new_cmd);

			      CommandsManagement.renderCommand(new_cmd, dom_command.find('.block_commands'), 3, function_obj);

			    } else {
			      CommandsManagement.createFloatingCommand(function_obj, dom_command.find('.block_commands'), $(this).data('command'), evt);
			    }

		} else {

			switch ($(this).data('command')) {
				case Models.COMMAND_TYPES.break:
					CommandsManagement.createFloatingCommand(function_obj, dom_command.find('.block_commands'), $(this).data('command'), evt);
					break;

				case Models.COMMAND_TYPES.switchcase:
					addCaseToSwitch(command, dom_where_render, function_obj, dom_command);
					break;
			}

		}

	});
}

function addCaseToSwitch (command, dom_where_render, function_obj, dom_command) {
	
	if ((command.cases == null)) {
		command.cases = [];
	}
	
	var sc = new Models.SwitchCase(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));

	command.cases.push(sc);

	SwitchManagement.renderCase(sc, command, function_obj, dom_command.find('.all_cases_div'));

}




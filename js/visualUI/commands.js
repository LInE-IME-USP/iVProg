import $ from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as GlobalsManagement from './globals';
import * as VariablesManagement from './variables';
import * as CommentsManagement from './commands/comment';
import * as ReadersManagement from './commands/reader';
import * as WritersManagement from './commands/writer';
import * as AttributionsManagement from './commands/attribution';
import * as IftruesManagement from './commands/iftrue';
import * as RepeatNtimesManagement from './commands/repeatNtimes';
import * as WhiletruesManagement from './commands/whiletrue';
import * as DowhiletruesManagement from './commands/dowhiletrue';
import * as SwitchesManagement from './commands/switch';
import * as FunctioncallsManagement from './commands/functioncall';
import * as VariableValueMenuManagement from './commands/variable_value_menu';

var has_element_created_draged = false;
var which_element_is_draged = null;

export function createFloatingCommand (function_obj, function_container, command_type, mouse_event) {
	var floatingObject;

	switch (command_type) {
		case Models.COMMAND_TYPES.comment:
			floatingObject = CommentsManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.reader:
			floatingObject = ReadersManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.writer:
			floatingObject = WritersManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.attribution:
			floatingObject = AttributionsManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.iftrue:
			floatingObject = IftruesManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.repeatNtimes:
			floatingObject = RepeatNtimesManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.whiletrue:
			floatingObject = WhiletruesManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.dowhiletrue:
			floatingObject = DowhiletruesManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.switch:
			floatingObject = SwitchesManagement.createFloatingCommand();
			break;

		case Models.COMMAND_TYPES.functioncall:
			floatingObject = FunctioncallsManagement.createFloatingCommand();
			break;
	}

	$(floatingObject).draggable().appendTo("body");

	$( floatingObject ).mouseup(function(evt) {
	  manageCommand(function_obj, function_container, evt, command_type);
	});
	
	floatingObject.css("position", "absolute");
	mouse_event.type = "mousedown.draggable";
	mouse_event.target = floatingObject[0];
	floatingObject.css("left", mouse_event.pageX - 15);
	floatingObject.css("top", mouse_event.pageY - 15);
	floatingObject.trigger(mouse_event);
}

// before_after_inside: 1 -> before, 2 -> after, 3 -> inside
function renderCommand (command, element_reference, before_after_inside, function_obj) {
	var createdElement;
	switch (command.type) {
		case Models.COMMAND_TYPES.comment:
			createdElement = CommentsManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.reader:
			createdElement = ReadersManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.writer:
			createdElement = WritersManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.attribution:
			createdElement = AttributionsManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.functioncall:
			createdElement = FunctioncallsManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.iftrue:
			createdElement = IftruesManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.repeatNtimes:
			createdElement = RepeatNtimesManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.whiletrue:
			createdElement = WhiletruesManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.dowhiletrue:
			createdElement = DowhiletruesManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.switch:
			createdElement = SwitchesManagement.renderCommand(command);
			break;

	}

	switch (before_after_inside) {
		case 1:
			$( createdElement ).insertBefore(element_reference);
			break;

		case 2:
			$( createdElement ).insertAfter(element_reference);
			break;

		case 3: 
			$( element_reference ).append(createdElement);
			break;
	}

}

function genericCreateCommand (command_type) {

	switch (command_type) {
		case Models.COMMAND_TYPES.comment:
			return new Models.Comment(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_value, LocalizedStrings.getUI('text_comment'), null, null, false));

		case Models.COMMAND_TYPES.reader:
			return new Models.Reader(null, null, null, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_variable, null, null, null, false));

		case Models.COMMAND_TYPES.writer:
			return new Models.Writer(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));

		case Models.COMMAND_TYPES.attribution:
			return new Models.Attribution(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_variable, null, null, null, false), 
				new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));

		case Models.COMMAND_TYPES.functioncall:
			return new Models.FunctionCall(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_function, null, null, null, false), null);

		case Models.COMMAND_TYPES.iftrue:
			return new Models.IfTrue(null, null, null);

		case Models.COMMAND_TYPES.repeatNtimes:
			return new Models.RepeatNTimes(null, null, null, null);

		case Models.COMMAND_TYPES.whiletrue:
			return new Models.WhileTrue(null, null);

		case Models.COMMAND_TYPES.dowhiletrue:
			return new Models.DoWhileTrue(null, null);

		case Models.COMMAND_TYPES.switch:
			return new Models.Switch(null, null, null);
	}
}

function manageCommand (function_obj, function_container, event, command_type) {
	$( ".created_element" ).each(function( index ) { 
		$(this).remove();
	});

	var el = document.elementFromPoint(event.clientX, event.clientY);
	console.log('soltou no: ');
	console.log(el);
	console.log($(el).data('fun'));

	// Primeiro verificar se ele soltou no espaço da função correta:
	var hier = $(el).parentsUntil(".all_functions");
	var esta_correto = false;
	var esta_na_div_correta = false;
	if ($(el).hasClass("commands_list_div")) {
		esta_na_div_correta = true;
	} 
	for (var i = 0; i < hier.length; i++) {
		if ($(hier[i]).hasClass("commands_list_div")) {
			esta_na_div_correta = true;
		} 
		if ($(hier[i]).data('fun') == function_obj) {
			esta_correto = true;
			break;
		}
	}
	if (!esta_correto) {
		has_element_created_draged = false;
		which_element_is_draged = null;
		return;
	} else {
		if (!esta_na_div_correta) {
			has_element_created_draged = false;
			which_element_is_draged = null;
			return;
		}
	}

	// Agora é descobrir qual o escopo para adicionar o comando:

	// Se o elemento clicado possuir o atributo "fun", então, é direto na div dos comandos:
	if (typeof $(el).data('fun') !== 'undefined') {

		// Se a lista de comandos estiver vazia, então é o primeiro.
		// Portanto, ele deve soltar o elemento obrigatoriamente no objeto vazio
		if (($(el).data('fun').commands == null)  || ($(el).data('fun').commands.length == 0)) {
				// pode adicionar 
				$(el).data('fun').commands = [];

				var new_cmd = genericCreateCommand(command_type);

				$(el).data('fun').commands.push(new_cmd);

				renderCommand(new_cmd, $(function_container).find('.commands_list_div'), 3, function_obj);
			
		} else { // Entra nesse else, caso já existam outros comandos no bloco:

			findNearbyCommandToAddInFunctionScope(el, event, $(function_container).find('.commands_list_div'), function_obj, command_type);
		}

	} else {
		console.log("soltou em um comando");
		// descobrir em qual comando ele soltou: 
		var hier_find = $(el).parentsUntil(".commands_list_div");
		var hierarquia_bottom_up = [];
		if (typeof $(el).data('command') !== 'undefined') {
			hierarquia_bottom_up.push($(el).data('command'));
		}
		for (var i = 0; i < hier_find.length; i++) {
			if (typeof $(hier_find[i]).data('command') !== 'undefined') {
				hierarquia_bottom_up.push($(hier_find[i]).data('command'));
			}
		}
		console.log("comando em que soltou: ");
		console.log(hierarquia_bottom_up[0]);
		console.log("hierarquia de baixo para cima na árvore, de onde ele soltou: ");
		for (var i = 0; i < hierarquia_bottom_up.length; i++) {
			console.log(hierarquia_bottom_up[i]);
		}
	}
	
	has_element_created_draged = false;
	which_element_is_draged = null;
}

function findNearbyCommandToAddInFunctionScope (el, event, node_list_commands, function_obj, command_type) {

	var all_sub = $(node_list_commands).find('div');

	var menor_distancia = 999999999;
	var elemento_menor_distancia = null;
	var antes = true;

	var t_bot;
	var t_top;
	// Descobrindo o elemento mais próximo:
	for (var i = 0; i < all_sub.length; i++) {
		
		t_top = all_sub[i].getBoundingClientRect().top;
		t_bot = all_sub[i].getBoundingClientRect().top + all_sub[i].getBoundingClientRect().height;

		if ((t_top - event.clientY) < menor_distancia) {
			menor_distancia = event.clientY - t_top;
			elemento_menor_distancia = all_sub[i];
		}
	}

	var borda_inferior = elemento_menor_distancia.parentNode.getBoundingClientRect().top + elemento_menor_distancia.parentNode.getBoundingClientRect().height;
	
	// Está mais próximo da borda de baixo, ou seja.. inserir por último:
	if ((borda_inferior - event.clientY) < menor_distancia) {
		
		var recentComand = genericCreateCommand(command_type);
		function_obj.commands.push(recentComand);
		//
		renderCommand(recentComand, node_list_commands, 3, function_obj);
	} else {

		var recentComand = genericCreateCommand(command_type);

		var index = function_obj.commands.indexOf($(elemento_menor_distancia).data('command'));

		if (index > -1) {
		    function_obj.commands.splice(index, 0, recentComand);
		}

		renderCommand(recentComand, elemento_menor_distancia, 1, function_obj);
	}
}

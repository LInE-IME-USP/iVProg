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

export function removeCommand (command, function_obj, dom_obj) {
	if (function_obj.commands.indexOf(command) > -1) {
		function_obj.commands.splice(function_obj.commands.indexOf(command), 1);
		return true;
	}

	// Utilize dois parantNode, pois o primeiro é o div de comandos
	if ($(dom_obj[0].parentNode.parentNode).data('command').commands_block.indexOf(command) > -1) {
		$(dom_obj[0].parentNode.parentNode).data('command').commands_block.splice
		($(dom_obj[0].parentNode.parentNode).data('command').commands_block.indexOf(command), 1);
		return true;
	}
	return false;
}

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

	floatingObject.draggable().appendTo("body");

	floatingObject.mouseup(function(evt) {
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
export function renderCommand (command, element_reference, before_after_inside, function_obj) {
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
			createdElement = IftruesManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.repeatNtimes:
			createdElement = RepeatNtimesManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.whiletrue:
			createdElement = WhiletruesManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.dowhiletrue:
			createdElement = DowhiletruesManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.switch:
			createdElement = SwitchesManagement.renderCommand(command, function_obj);
			break;

	}

	switch (before_after_inside) {
		case 1:
			createdElement.insertBefore(element_reference);
			break;

		case 2:
			createdElement.insertAfter(element_reference);
			break;

		case 3: 
			element_reference.append(createdElement);
			break;
	}

}

export function genericCreateCommand (command_type) {

	switch (command_type) {
		case Models.COMMAND_TYPES.comment:
			return new Models.Comment(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_value, LocalizedStrings.getUI('text_comment'), null, null, false));

		case Models.COMMAND_TYPES.reader:
			return new Models.Reader(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_variable, null, null, null, false));

		case Models.COMMAND_TYPES.writer:
			return new Models.Writer([new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true)]);

		case Models.COMMAND_TYPES.attribution:
			return new Models.Attribution(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_variable, null, null, null, false), 
				[]);

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

	var el = $(document.elementFromPoint(event.clientX, event.clientY));
	console.log('soltou no: ');
	console.log(el);
	console.log(el.data('fun'));

	// Primeiro verificar se ele soltou no espaço da função correta:
	var hier = el.parentsUntil(".all_functions");
	var esta_correto = false;
	var esta_na_div_correta = false;
	if (el.hasClass("commands_list_div")) {
		esta_na_div_correta = true;
	} 
	for (var i = 0; i < hier.length; i++) {
		var temp = $(hier[i]);
		if (temp.hasClass("commands_list_div")) {
			esta_na_div_correta = true;
		} 
		if (temp.data('fun') == function_obj) {
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
	if (typeof el.data('fun') !== 'undefined') {

		// Se a lista de comandos estiver vazia, então é o primeiro.
		// Portanto, ele deve soltar o elemento obrigatoriamente no objeto vazio
		if ((el.data('fun').commands == null)  || (el.data('fun').commands.length == 0)) {
				// pode adicionar 
				el.data('fun').commands = [];

				var new_cmd = genericCreateCommand(command_type);

				el.data('fun').commands.push(new_cmd);

				renderCommand(new_cmd, $(function_container).find('.commands_list_div'), 3, function_obj);
			
		} else { // Entra nesse else, caso já existam outros comandos no bloco:

			findNearbyCommandToAddInFunctionScope(el, event, $(function_container).find('.commands_list_div'), function_obj, command_type);
		}

	} else {
		console.log("soltou em um comando");
		// descobrir em qual comando ele soltou: 
		var hier_find = el.parentsUntil(".commands_list_div");
		var hierarquia_bottom_up = [];
		if (typeof el.data('command') !== 'undefined') {
			hierarquia_bottom_up.push(el.data('command'));
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

		// se a hierarquia possuir apenas um elemento, então está na raiz dos comandos: 
		if (hierarquia_bottom_up.length == 1) {
			var sub_elemento = false;
			for (var i = 0; i < hier_find.length; i++) {
				if (typeof $(hier_find[i]).data('command') !== 'undefined') {
					findBeforeOrAfterCommandToAdd(hier_find[i], event, function_obj, command_type);
					sub_elemento = true;
					break;
				}
			}
			if (!sub_elemento) {
				findBeforeOrAfterCommandToAdd(el[0], event, function_obj, command_type);
			}
		} else {
			// caso exista mais de um elemento na hierarquia:
			if (typeof $(el).data('command') !== 'undefined') {
				console.log("PPP1");
				insertCommandInBlockHierar(el[0], event, function_obj, command_type, hier_find, hierarquia_bottom_up);
			} else {
				var sub_elemento = false;
				for (var i = 0; i < hier_find.length; i++) {
					if (typeof $(hier_find[i]).data('command') !== 'undefined') {
						insertCommandInBlockHierar(hier_find[i], event, function_obj, command_type, hier_find, hierarquia_bottom_up);
						sub_elemento = true;
						break;
					}
				}
			}
			
		}
	}
	
	has_element_created_draged = false;
	which_element_is_draged = null;
}

function insertCommandInBlockHierar (el, event, function_obj, command_type, hier_dom, hier_obj) {
	var el_jq = $(el);
	var command_parent = el_jq.data('command');
	
	if ((el_jq.data('command').type == Models.COMMAND_TYPES.repeatNtimes) ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.whiletrue)  ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.dowhiletrue) ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.iftrue) ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.switch) ) {

		if ((el_jq.data('command').type == Models.COMMAND_TYPES.repeatNtimes) ||
			(el_jq.data('command').type == Models.COMMAND_TYPES.whiletrue)  ||
			(el_jq.data('command').type == Models.COMMAND_TYPES.dowhiletrue) ) {

			// Se não tiver outro comando ainda no bloco, só adiciona: 
			if (command_parent.commands_block == null || command_parent.commands_block.length == 0) {
				command_parent.commands_block = [];

				var recentComand = genericCreateCommand(command_type);
				command_parent.commands_block.push(recentComand);

				renderCommand(recentComand, el_jq.find('.block_commands'), 3, function_obj);
			} else { // Se já tem algum comando no bloco:
				findNearbyCommandToAddInBlockScope(el, event, el, function_obj, command_type, command_parent);
			}

		} else {
			// QUANDO FOR BLOCO DO TIPO IF OU SWITCH/CASE:
		}

	} else {
		// entra neste bloco, se soltou o comando sobre outro comando dentro de um subbloco:
		findBeforeOrAfterCommandToAddInsertBlock(el, event, function_obj, command_type);
	}
}



function findNearbyCommandToAddInBlockScope (el, event, node_list_commands, function_obj, command_type, command_parent) {

	var all_sub = $(node_list_commands).find('div.command_container');

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
		command_parent.commands_block.push(recentComand);
		//
		renderCommand(recentComand, node_list_commands, 3, function_obj);

	} else {

		var recentComand = genericCreateCommand(command_type);

		var index = command_parent.commands_block.indexOf($(elemento_menor_distancia).data('command'));

		if (index > -1) {
		    command_parent.commands_block.splice(index, 0, recentComand);
		}

		renderCommand(recentComand, elemento_menor_distancia, 1, function_obj);
	}
}



function findBeforeOrAfterCommandToAddInsertBlock (el, event, function_obj, command_type) {

	var el_jq = $(el);
	var command_parent = $(el.parentNode.parentNode).data('command');
	var command_target = el_jq.data('command');

	var menor_distancia = 999999999;
	var antes = true;

	var t_bot;
	var t_top;

	t_top = el.getBoundingClientRect().top;
	t_bot = el.getBoundingClientRect().top + el.getBoundingClientRect().height;

	var d_top = event.clientY - t_top; // distancia topo
	var d_bot = t_bot - event.clientY; // distancia baixo

	// Está mais próximo da borda de baixo, ou seja.. inserir por último:

	if (d_top < d_bot) {
		
		var recentComand = genericCreateCommand(command_type);

		var index = command_parent.commands_block.indexOf(command_target);

		if (index > -1) {
		    command_parent.commands_block.splice(index, 0, recentComand);
		}

		renderCommand(recentComand, el, 1, function_obj);

	} else {
		var recentComand = genericCreateCommand(command_type);

		var index = command_parent.commands_block.indexOf(command_target);

		if (index > -1) {
		    command_parent.commands_block.splice((index + 1), 0, recentComand);
		}

		renderCommand(recentComand, el, 2, function_obj);
	}
}

function insertCommandInBlock (el, event, function_obj, command_type) {
	var el_jq = $(el);
	var command_parent = el_jq.data('command');

	if ((el_jq.data('command').type == Models.COMMAND_TYPES.repeatNtimes) ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.whiletrue)  ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.dowhiletrue) ) {

		// Se não tiver outro comando ainda no bloco, só adiciona: 
		if (command_parent.commands_block == null || command_parent.commands_block.length == 0) {
			command_parent.commands_block = [];

			var recentComand = genericCreateCommand(command_type);
			command_parent.commands_block.push(recentComand);

			renderCommand(recentComand, el_jq.find('.block_commands'), 3, function_obj);
		} else { // Se já tem algum comando no bloco:
			findInBlockCorrectPlace(el, event, function_obj, command_type);
		}

	} else {
		console.log("PPP2");
	}
}

function findInBlockCorrectPlace (el, event, function_obj, command_type) {
	var el_jq = $(el);
	var all_sub = el_jq.find('div.command_container');

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

		var command_parent = el_jq.data('command');
		
		command_parent.commands_block.push(recentComand);

		renderCommand(recentComand, $(el_jq.find('.block_commands')[0]), 3, function_obj);

	} else {

		var recentComand = genericCreateCommand(command_type);

		var command_parent = el_jq.data('command');
		
		var index = command_parent.commands_block.indexOf($(elemento_menor_distancia).data('command'));

		if (index > -1) {
		    command_parent.commands_block.splice(index, 0, recentComand);
		    renderCommand(recentComand, elemento_menor_distancia, 1, function_obj);
		}
	}
}

function findBeforeOrAfterCommandToAdd (el, event, function_obj, command_type) {
	switch ($(el).data('command').type) {
		case Models.COMMAND_TYPES.iftrue:
		case Models.COMMAND_TYPES.switch:
		case Models.COMMAND_TYPES.repeatNtimes:
		case Models.COMMAND_TYPES.whiletrue:
		case Models.COMMAND_TYPES.dowhiletrue:
			insertCommandInBlock(el, event, function_obj, command_type);
			return;
	}

	var menor_distancia = 999999999;
	var antes = true;

	var t_bot;
	var t_top;

	t_top = el.getBoundingClientRect().top;
	t_bot = el.getBoundingClientRect().top + el.getBoundingClientRect().height;

	var d_top = event.clientY - t_top; // distancia topo
	var d_bot = t_bot - event.clientY; // distancia baixo

	// Está mais próximo da borda de baixo, ou seja.. inserir por último:
	if (d_top < d_bot) {
		
		var recentComand = genericCreateCommand(command_type);

		var index = function_obj.commands.indexOf($(el).data('command'));

		if (index > -1) {
		    function_obj.commands.splice(index, 0, recentComand);
		}

		renderCommand(recentComand, el, 1, function_obj);

	} else {
		var recentComand = genericCreateCommand(command_type);

		var index = function_obj.commands.indexOf($(el).data('command'));

		if (index > -1) {
		    function_obj.commands.splice((index + 1), 0, recentComand);
		}

		renderCommand(recentComand, el, 2, function_obj);
	}
}

function findNearbyCommandToAddInFunctionScope (el, event, node_list_commands, function_obj, command_type) {

	var all_sub = $(node_list_commands).find('div.command_container');

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

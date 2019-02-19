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
import * as BreaksManagement from './commands/break';
import * as ReturnsManagement from './commands/return';

var has_element_created_draged = false;
var which_element_is_draged = null;

export function removeCommand (command, function_obj, dom_obj) {
	if (function_obj.commands.indexOf(command) > -1) {
		function_obj.commands.splice(function_obj.commands.indexOf(command), 1);
		return true;
	}

	// Utilize dois parantNode, pois o primeiro é o div de comandos
	try {
		if (dom_obj.parent().parent().data('command').commands_block.indexOf(command) > -1) {
			dom_obj.parent().parent().data('command').commands_block.splice
			(dom_obj.parent().parent().data('command').commands_block.indexOf(command), 1);
			return true;
		}	
	} catch (err) {}
	
	try {
		if (dom_obj.parent().parent().data('command').type == Models.COMMAND_TYPES.iftrue) {
			if (dom_obj.parent().parent().data('command').commands_else.indexOf(command) > -1) {
				dom_obj.parent().parent().data('command').commands_else.splice
				(dom_obj.parent().parent().data('command').commands_else.indexOf(command), 1);
				return true;
			}
		}
	} catch (err) {}

	if (dom_obj.parent().data('switchcase')) {
		console.log("o que encontrei: ");
		console.log(dom_obj.parent().data('switchcase'));
		dom_obj.parent().data('switchcase').commands_block.splice(dom_obj.parent().data('switchcase').commands_block.indexOf(command), 1);
		return true;
	}

	return false;
}

export function createFloatingCommand (function_obj, function_container, command_type, mouse_event) {
	var floatingObject;

	switch (command_type) {
		case Models.COMMAND_TYPES.break:
			floatingObject = BreaksManagement.createFloatingCommand();
			break;

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

		case Models.COMMAND_TYPES.return:
			floatingObject = ReturnsManagement.createFloatingCommand();
			break;
	}

	floatingObject.draggable({
		drag: function(evt) {
	        borderMouseDragCommand(function_obj, function_container, evt);
	    },
	    stop: function(evt) {
	    	function_container.find('.over_command_drag').each(function( index ) {
				$(this).removeClass('over_command_drag');
			});
	    }
	}).appendTo("body");

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

function borderMouseDragCommand (function_obj, function_container, evt) {

	function_container.find('.over_command_drag').each(function( index ) {
		$(this).removeClass('over_command_drag');
	});

	var prev = null;

	function_container.find('.commands_list_div').each(function( index ) { 
		prev = $(this);
		if (prev) {
			var objLeft = prev.offset().left;
	        var objTop = prev.offset().top;
	        var objRight = objLeft + prev.width();
	        var objBottom = objTop + prev.height();
	        if (evt.pageX > objLeft && evt.pageX < objRight && evt.pageY > objTop && evt.pageY < objBottom) {
	        	prev.addClass("over_command_drag"); 
	        }
	    }
	});

	function_container.find('.command_container').each(function( index ) { 
		var obj = $(this);
		var objLeft = obj.offset().left;
        var objTop = obj.offset().top;
        var objRight = objLeft + obj.width();
        var objBottom = objTop + obj.height();
        if (evt.pageX > objLeft && evt.pageX < objRight && evt.pageY > objTop && evt.pageY < objBottom) {
        	if (prev) {
        		prev.removeClass('over_command_drag');
        	}
        	obj.addClass("over_command_drag"); 
        	return;
        }
	});
}

// before_after_inside: 1 -> before, 2 -> after, 3 -> inside
export function renderCommand (command, element_reference, before_after_inside, function_obj) {
	var createdElement;
	switch (command.type) {
		case Models.COMMAND_TYPES.comment:
			createdElement = CommentsManagement.renderCommand(command, function_obj);
			break;

		case Models.COMMAND_TYPES.break:
			createdElement = BreaksManagement.renderCommand(command, function_obj);
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

		case Models.COMMAND_TYPES.return:
			createdElement = ReturnsManagement.renderCommand(command, function_obj);
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

		case Models.COMMAND_TYPES.break:
			return new Models.Break();

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
			return new Models.IfTrue(new Models.ConditionalExpression(null), null, null);

		case Models.COMMAND_TYPES.repeatNtimes:
			return new Models.RepeatNTimes(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_variable, null, null, null, false), 
											new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.only_variable, null, null, null, false),
										     null, new Models.ConditionalExpression(null), null, null);

		case Models.COMMAND_TYPES.whiletrue:
			return new Models.WhileTrue(new Models.ConditionalExpression(null), null);

		case Models.COMMAND_TYPES.dowhiletrue:
			return new Models.DoWhileTrue(new Models.ConditionalExpression(null), null);

		case Models.COMMAND_TYPES.switch:

			var sc = [new Models.SwitchCase(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true))];

			return new Models.Switch(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.variable_and_function, null, null, null, true), sc);

		case Models.COMMAND_TYPES.return:
			return new Models.Return(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
	}
}

function dragTrash (event) {

	var trash = $('<i class="ui icon trash alternate outline"></i>');
	$('body').append(trash);
	trash.css('position', 'absolute');
	trash.css('top', event.clientY);
	trash.css('left', event.clientX - 20);
	trash.css('font-size', '3em');
	trash.css('display', 'none');

	trash.fadeIn( 200, function() {
		trash.fadeOut( 200, function() {
			trash.remove();
		} );
    });
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
		dragTrash(event);
		return;
	} else {
		if (!esta_na_div_correta) {
			has_element_created_draged = false;
			which_element_is_draged = null;
			dragTrash(event);
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

		// Se for do tipo break, verificar se está no contexto correto: 
		// Caso não esteja no contexto, apenas retorna sem dar continuidade:
		var is_correct_context = false;
		if (command_type == Models.COMMAND_TYPES.break) {
			for (var i = 0; i < hierarquia_bottom_up.length; i++) {
				if ((hierarquia_bottom_up[i].type == Models.COMMAND_TYPES.repeatNtimes)
					|| (hierarquia_bottom_up[i].type == Models.COMMAND_TYPES.whiletrue)
					|| (hierarquia_bottom_up[i].type == Models.COMMAND_TYPES.dowhiletrue)
					|| (hierarquia_bottom_up[i].type == Models.COMMAND_TYPES.switch)) {

					is_correct_context = true;
					break;

				}
			}

			if (!is_correct_context) {
				console.error("Context not allowed to insert BREAK COMMAND!");
				return;
			}
		}

		

		// se a hierarquia possuir apenas um elemento, então está na raiz dos comandos: 
		if (hierarquia_bottom_up.length == 1) {
			console.log('QQ1');
			var sub_elemento = false;
			for (var i = 0; i < hier_find.length; i++) {
				if (typeof $(hier_find[i]).data('command') !== 'undefined') {
					console.log('QQ2');
					findBeforeOrAfterCommandToAdd(hier_find[i], event, function_obj, command_type);
					sub_elemento = true;
					break;
				}
			}
			if (!sub_elemento) {
				console.log('QQ3');
				findBeforeOrAfterCommandToAdd(el[0], event, function_obj, command_type);
			}
		} else {
			console.log('QQ4');
			// caso exista mais de um elemento na hierarquia:
			if (typeof $(el).data('command') !== 'undefined') {
				console.log('QQ5');
				console.log("PPP1");
				insertCommandInBlockHierar(el[0], event, function_obj, command_type, hier_find, hierarquia_bottom_up);
			} else {
				console.log('QQ6');
				var sub_elemento = false;
				for (var i = 0; i < hier_find.length; i++) {
					if (typeof $(hier_find[i]).data('command') !== 'undefined') {
						console.log('QQ7');
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

	renderAlgorithm();
}

function insertCommandInBlockHierar (el, event, function_obj, command_type, hier_dom, hier_obj) {
	var el_jq = $(el);
	var command_parent = el_jq.data('command');
	
	if ((el_jq.data('command').type == Models.COMMAND_TYPES.repeatNtimes) ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.whiletrue)  ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.dowhiletrue) ||
		(el_jq.data('command').type == Models.COMMAND_TYPES.switch) ) {

		console.log('QQ17');

		if ((el_jq.data('command').type == Models.COMMAND_TYPES.repeatNtimes) ||
			(el_jq.data('command').type == Models.COMMAND_TYPES.whiletrue)  ||
			(el_jq.data('command').type == Models.COMMAND_TYPES.dowhiletrue) ) {

			console.log('QQ18');

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
			addCommandToSwitchCase(event, function_obj, command_type);
		}

	} else {
		console.log('QQ19');
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
	var temp_parent = $(el.parentNode.parentNode);

	var is_in_else = false;

	if (!command_parent) {
		command_parent = el_jq.data('command');
		temp_parent = el_jq;
		var hier = el_jq.parentsUntil(".command_container");

		for (var i = 0; i < hier.length; i++) {
			var temp = $(hier[i]);
			if (typeof temp.data('else') != 'undefined') {
				is_in_else = true;
			}
			if (typeof temp.data('command') != 'undefined') {
				command_parent = temp.data('command');
				temp_parent = temp;
			}
		}
	}


	var hier = el_jq.parentsUntil(".command_container");
	for (var i = 0; i < hier.length; i++) {
		var temp = $(hier[i]);
		if (typeof temp.data('else') != 'undefined') {
			is_in_else = true;
		}
	}

	if (command_parent == command_target) {
		var hier = el_jq.parentsUntil(".command_container");

		for (var i = 0; i < hier.length; i++) {
			var temp = $(hier[i]);
			if (typeof temp.data('else') !== 'undefined') {
				is_in_else = true;
				break;
			}
		}
	}

	if ((command_parent.type != Models.COMMAND_TYPES.iftrue) && (command_parent.type != Models.COMMAND_TYPES.switch)) {
		var hier = temp_parent.parentsUntil(".all_cases_div");
		console.log("vou procurar!!");
		for (var i = 0; i < hier.length; i++) {
			console.log("estou vasculhando...");
			var temp = $(hier[i]);
			if (typeof temp.data('switchcase') !== 'undefined') {
				console.log("encontrei");
				command_parent = temp.data('switchcase');
				is_in_else = false;
				break;
			}
		}
	}

	console.log('debugging:');
	console.log('el_jq');
	console.log(el_jq);
	console.log('command_parent');
	console.log(command_parent);
	console.log('command_target');
	console.log(command_target);

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

		console.log('MMM1');

		if (is_in_else) {

			console.log('MMM2');

			if (command_parent == command_target) {
				console.log('MMM3');
				if (command_parent.commands_else == null || command_parent.commands_else.length == 0) {
					command_parent.commands_else = [];

					var recentComand = genericCreateCommand(command_type);
					command_parent.commands_else.push(recentComand);

					renderCommand(recentComand, el_jq, 3, function_obj);
				} else { // Se já tem algum comando no bloco:
					findInBlockCorrectPlace(el_jq, event, function_obj, command_type, true);
				}
				return;
			}
			console.log('MMM7');
			var index = command_parent.commands_else.indexOf(command_target);

			if (index > -1) {
			    command_parent.commands_else.splice(index, 0, recentComand);
			}

			renderCommand(recentComand, el, 1, function_obj);
		} else {
			console.log('MMM4');
			if (command_parent == command_target) {
				console.log('Nxxxx5');
				if (command_parent.commands_block == null || command_parent.commands_block.length == 0) {
					command_parent.commands_block = [];
					console.log('SSS4');
					var recentComand = genericCreateCommand(command_type);
					command_parent.commands_block.push(recentComand);

					renderCommand(recentComand, el_jq, 3, function_obj);
				} else {
					console.log('SSS5');
					findInBlockCorrectPlace(el_jq, event, function_obj, command_type);
				}
				
				
				return;
			}
			console.log('MMM6');

			var index = command_parent.commands_block.indexOf(command_target);

			if (index > -1) {
			    command_parent.commands_block.splice(index, 0, recentComand);
			}

			renderCommand(recentComand, el, 1, function_obj);
		}

		

	} else {
		console.log('XXX1');
		var recentComand = genericCreateCommand(command_type);

		if (is_in_else) {

			if (command_parent == command_target) {
				console.log('MMM3');
				if (command_parent.commands_else == null || command_parent.commands_else.length == 0) {
					command_parent.commands_else = [];
					console.log('SSS1');
					var recentComand = genericCreateCommand(command_type);
					command_parent.commands_else.push(recentComand);

					renderCommand(recentComand, el_jq, 3, function_obj);
				} else { // Se já tem algum comando no bloco:
					console.log('SSS2');
					findInBlockCorrectPlace(el_jq, event, function_obj, command_type, true);
				}
				return;
			}

			console.log('XXX2');
			var index = command_parent.commands_else.indexOf(command_target);

			if (index > -1) {
			    command_parent.commands_else.splice((index + 1), 0, recentComand);
			}

			renderCommand(recentComand, el, 2, function_obj);

		} else {

			if (command_parent == command_target) {
				console.log('Nxxxx78');
				if (command_parent.commands_block == null || command_parent.commands_block.length == 0) {
					command_parent.commands_block = [];

					var recentComand = genericCreateCommand(command_type);
					command_parent.commands_block.push(recentComand);
					console.log('SSS6');
					renderCommand(recentComand, el_jq, 3, function_obj);
				} else {
					console.log('SSS7');
					findInBlockCorrectPlace(el_jq, event, function_obj, command_type);
				}
				
				
				return;
			}

			console.log('XXX3');
			var index = command_parent.commands_block.indexOf(command_target);

			if (index > -1) {
			    command_parent.commands_block.splice((index + 1), 0, recentComand);
			}

			renderCommand(recentComand, el, 2, function_obj);
		}

		
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

	} else if (el_jq.data('command').type == Models.COMMAND_TYPES.iftrue) {

		console.log('QQ9');
		
		// no if ou no else?
		var correct_div = $(document.elementFromPoint(event.pageX, event.pageY));
		var is_in_if = true;
		if (correct_div.data('if')) {
			is_in_if = true;
		} else if (correct_div.data('else')) {
			is_in_if = false;
		} else {
			var hier = correct_div.parentsUntil(".command_container");
			for (var i = 0; i < hier.length; i++) {
				var temp = $(hier[i]);
				if (typeof temp.data('if') !== 'undefined') {
					is_in_if = true;
					break;
				}
				if (typeof temp.data('else') !== 'undefined') {
					is_in_if = false;
					break;
				}
			}
		}

		if (is_in_if) {
			if (command_parent.commands_block == null || command_parent.commands_block.length == 0) {
				command_parent.commands_block = [];

				var recentComand = genericCreateCommand(command_type);
				command_parent.commands_block.push(recentComand);

				renderCommand(recentComand, el_jq.find('.commands_if'), 3, function_obj);
			} else { // Se já tem algum comando no bloco:
				findInBlockCorrectPlace(el_jq.find('.commands_if'), event, function_obj, command_type);
			}

		} else {
			if (command_parent.commands_else == null || command_parent.commands_else.length == 0) {
				command_parent.commands_else = [];

				var recentComand = genericCreateCommand(command_type);
				command_parent.commands_else.push(recentComand);

				renderCommand(recentComand, el_jq.find('.commands_else'), 3, function_obj);
			} else { // Se já tem algum comando no bloco:
				findInBlockCorrectPlace(el_jq.find('.commands_else'), event, function_obj, command_type, true);
			}

		}

	} else { // é do tipo switch
		console.log("está tentando inserir em um switch que está na raiz!");
		addCommandToSwitchCase(event, function_obj, command_type);
	}
}

function addCommandToSwitchCase (event, function_obj, command_type) {

	var el = $(document.elementFromPoint(event.clientX, event.clientY));

	var which_case = el.data('switchcase');
	var case_div = el;
	
	if (!which_case) {
		var hier_find = el.parentsUntil(".all_cases_div");
		for (var i = 0; i < hier_find.length; i++) {
			if (typeof $(hier_find[i]).data('switchcase') !== 'undefined') {
				which_case = $(hier_find[i]).data('switchcase');
				case_div = $(hier_find[i]);
				break;
			}
		}
	}

	if (which_case.commands_block == null || which_case.commands_block.length < 1) {
		which_case.commands_block = [];

		var recentComand = genericCreateCommand(command_type);
		which_case.commands_block.push(recentComand);
		renderCommand(recentComand, case_div.find('.case_commands_block'), 3, function_obj);
	} else {
		findInBlockCorrectPlaceInSwitchCase(which_case, case_div, event, function_obj, command_type);
	}

}




function findInBlockCorrectPlaceInSwitchCase (which_case, case_div, event, function_obj, command_type) {

	var all_sub = case_div.find('div.command_container');

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

		which_case.commands_block.push(recentComand);

		renderCommand(recentComand, $(case_div.find('.case_commands_block')[0]), 3, function_obj);

	} else {

		var recentComand = genericCreateCommand(command_type);

		var index = which_case.commands_block.indexOf($(elemento_menor_distancia).data('command'));

		if (index > -1) {
		    which_case.commands_block.splice(index, 0, recentComand);
		    renderCommand(recentComand, elemento_menor_distancia, 1, function_obj);
		}
	}
}

function findInBlockCorrectPlace (el, event, function_obj, command_type, is_in_else = false) {
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

	console.log("menor_distancia: ");
	console.log(elemento_menor_distancia);
	
	// Está mais próximo da borda de baixo, ou seja.. inserir por último:
	if ((borda_inferior - event.clientY) < menor_distancia) {

		console.log('QQ11');
		
		var recentComand = genericCreateCommand(command_type);

		var command_parent = el_jq.data('command');

		if (is_in_else) {
			console.log('QQ15');
			command_parent.commands_else.push(recentComand);
			console.log('el_jq');
			console.log(el_jq);
			console.log("$(el_jq.find('.commands_else')[0]):: ");
			console.log($(el_jq.find('.commands_else')[0]));

			renderCommand(recentComand, el_jq, 3, function_obj);

		} else {
			console.log('QQ16');
			command_parent.commands_block.push(recentComand);

			renderCommand(recentComand, $(el_jq.find('.block_commands')[0]), 3, function_obj);
		}

	} else {

		console.log('QQ12');

		var recentComand = genericCreateCommand(command_type);

		var command_parent = el_jq.data('command');

		if (is_in_else) {

			var index = command_parent.commands_else.indexOf($(elemento_menor_distancia).data('command'));

			if (index > -1) {
			    command_parent.commands_else.splice(index, 0, recentComand);
			    renderCommand(recentComand, elemento_menor_distancia, 1, function_obj);
			}

		} else {
			var index = command_parent.commands_block.indexOf($(elemento_menor_distancia).data('command'));

			if (index > -1) {
			    command_parent.commands_block.splice(index, 0, recentComand);
			    renderCommand(recentComand, elemento_menor_distancia, 1, function_obj);
			}

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

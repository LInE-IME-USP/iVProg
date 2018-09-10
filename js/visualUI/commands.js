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
function renderCommand (command, element_reference, before_after_inside) {
	var createdElement;
	switch (command.type) {
		case Models.COMMAND_TYPES.comment:
			createdElement = CommentsManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.reader:
			createdElement = ReadersManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.writer:
			createdElement = WritersManagement.renderCommand(command);
			break;

		case Models.COMMAND_TYPES.attribution:
			createdElement = AttributionsManagement.renderCommand(command);
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

		case Models.COMMAND_TYPES.functioncall:
			createdElement = FunctioncallsManagement.renderCommand(command);
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
			return new Models.Comment(null);

		case Models.COMMAND_TYPES.reader:
			return new Models.Reader(null, null, null);

		case Models.COMMAND_TYPES.writer:
			return new Models.Writer(null);

		case Models.COMMAND_TYPES.attribution:
			return new Models.Attribution(null, null);

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

		case Models.COMMAND_TYPES.functioncall:
			return new Models.FunctionCall(null, null);
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

				renderCommand(new_cmd, $(function_container).find('.commands_list_div'), 3);
			
		} else { // Entra nesse else, caso já existam outros comandos no bloco:

			findNearbyCommandToAddInFunctionScope(el, event, $(function_container).find('.commands_list_div'), function_obj, command_type);
		}
		return;

	} else { // Se entrar nesse bloco 'else', quer dizer que o usuário não soltou o elemento necessariamente na div específica da função
			 // portanto, devemos procurar nos elementos DOM, em que lugar da função, ele soltou o comando

		/*var hier = $(el).parentsUntil(".all_functions");
		for (i = 0; i < hier.length; i++) {
			if ($(hier[i]).data('fun') == function_to_add) {

				programa.funcoes[function_to_add].comandos.push(createElementGenericFunction());

				break;
			}
		}*/

		//findPositionAndInsertCommand(el, event);
		var caminho = findPositionAndPathToElementTarget(el, event);

		console.log("soltou sobre o seguinte elemento: ");

		console.log(caminho);

		console.log("soltou sobre o seguinte DOM: ");

		console.log(el);

		// se for 1, então está no nível do corpo da função:
		if (caminho.length == 1) {

			console.log("o caminho é de tamanho 1 e o objeto é o seguinte: " + caminho[0]);
			console.log(programa.funcoes[function_to_add].comandos[caminho[0]]);

			// se for do tipo true ou false, temos que determinar se soltou no if ou no else: 
			if (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.iftrue) {

				if ($(el).data('if')) {

					if ((programa.funcoes[function_to_add].comandos[caminho[0]].commands_block == null) 
						|| (programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.length == 0)) {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block = [];
						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					} else {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					}
				} else if ($(el).data('else')) {

					if ((programa.funcoes[function_to_add].comandos[caminho[0]].commands_else == null) 
						|| (programa.funcoes[function_to_add].comandos[caminho[0]].commands_else.length == 0)) {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_else = [];
						programa.funcoes[function_to_add].comandos[caminho[0]].commands_else.push(createElementGenericFunction());

					} else {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_else.push(createElementGenericFunction());

					}
				} else {
					console.log("soltou dentro do if, fora dos divs corretos... VERIFICAR QUAL ESTÁ MAIS PRÓXIMO... O IF OU O ELSE  --- NNN11");
					discoveryIfOrElse(el, event);
				}

			} else {

				if ((programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.repeatNtimes)
					|| (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.whiletrue) 
					|| (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.dowhiletrue) 
					|| (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.switch) ) {
					
					if ((programa.funcoes[function_to_add].comandos[caminho[0]].commands_block == null) 
						|| (programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.length == 0)) {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block = [];
						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					} else {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					}
				} else {
				
					var result = getBeforeOrAfterOrEndAllocate(el, event);
					if (result == true) {
						console.log("adicionando ANTES");
						programa.funcoes[function_to_add].comandos.splice(caminho[0], 0, createElementGenericFunction());
					} else {
						console.log("adicionando DEPOIS");
						programa.funcoes[function_to_add].comandos.splice(caminho[0] + 1, 0, createElementGenericFunction());
					}
				}
			}

			

		} else { // caso seja mais de um, o caminho, então, precisamos percorrer até achar: 

			// CONTINUAR DAQUI: 
			console.log("ACHO QUE É A SITUAÇÃO DE BLOCO INTERNO");
			console.log("SOLTOU NO ELEMENTO A SEGUIR: ");
			console.log(el.relatedObj);
			console.log("PAI DO ELEMENTO QUE ELA SOLTOU: ");
			console.log(el.parentNode.relatedObj);
			//

			if ((el.parentNode.relatedObj.tipo == tiposComandos.iftrue)) {

				if ($(el.parentNode).data('if') || $(el).data('if')) {

					if ((el.parentNode.relatedObj.commands_block == null) 
						|| (el.parentNode.relatedObj.commands_block.length == 0)) {

						el.parentNode.relatedObj.commands_block = [];
						el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());

					} else {

						if ($(el).data('if')) {
							// Descobrir qual o elemento mais próximo de onde ele soltou o comando recém criado:
							console.log("SITUAÇÃO TRATADA NO K1!");
							getNearbyIndexOfElementOnClick(el, event);

						} else {
							if (getBeforeOrAfterOrEndAllocate(el, event)) {
								console.log("K1 ANTECAO! SOLTOU ANTES DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index'), 0, createElementGenericFunction());
							} else {
								console.log("K1 ANTECAO! SOLTOU DEPOIS DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index') + 1, 0, createElementGenericFunction());
							}
						}

						//el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());

					}
				} else if ($(el.parentNode).data('else') || $(el).data('else')) {

					if ((el.parentNode.relatedObj.commands_else == null) 
						|| (el.parentNode.relatedObj.commands_else.length == 0)) {

						el.parentNode.relatedObj.commands_else = [];
						el.parentNode.relatedObj.commands_else.push(createElementGenericFunction());

					} else {

						if ($(el).data('else')) {
							// Descobrir qual o elemento mais próximo de onde ele soltou o comando recém criado:
							console.log("SITUAÇÃO TRATADA NO K2!");
							getNearbyIndexOfElementOnClick(el, event);

						} else {

							if (getBeforeOrAfterOrEndAllocate(el, event)) {
								console.log("K2 ANTECAO! SOLTOU ANTES DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_else.splice($(el).data('index'), 0, createElementGenericFunction());

							} else {
								console.log("K2 ANTECAO! SOLTOU DEPOIS DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_else.splice($(el).data('index') + 1, 0, createElementGenericFunction());
							}
						}

						//el.parentNode.relatedObj.commands_else.push(createElementGenericFunction());

					}
				} else {
					console.log("soltou dentro do if, fora dos divs corretos... VERIFICAR QUAL ESTÁ MAIS PRÓXIMO... O IF OU O ELSE  --- NNN22");
					discoveryIfOrElse(el, event);
				}

			} else {
				console.log("COMEÇAR A TRATAR!...");

				if ((el.parentNode.relatedObj.tipo == tiposComandos.repeatNtimes)
					|| (el.parentNode.relatedObj.tipo == tiposComandos.whiletrue) 
					|| (el.parentNode.relatedObj.tipo == tiposComandos.dowhiletrue) 
					|| (el.parentNode.relatedObj.tipo == tiposComandos.switch) 
					|| (el.parentNode.relatedObj.tipo == tiposComandos.iftrue)) {


					if ((el.parentNode.relatedObj.commands_block == null) 
						|| (el.parentNode.relatedObj.commands_block.length == 0)) {
					
						el.parentNode.relatedObj.commands_block = [];
						el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());

					} else {

						if (typeof $(el).data('subblock') !== 'undefined') {
							
							console.log("SITUAÇÃO TRATADA NO K3!");

							getNearbyIndexOfElementOnClick(el, event);

						} else {
							if (getBeforeOrAfterOrEndAllocate(el, event)) {
								console.log("K3 ANTECAO! SOLTOU ANTES DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index'), 0, createElementGenericFunction());
							} else {
								console.log("K3 ANTECAO! SOLTOU DEPOIS DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index') + 1, 0, createElementGenericFunction());
							}
						}

						//el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());
					}


				} else {
					console.log("AGORA SIM! SITUAÇÃO K4!");
					console.log("VOU ADICIONAR NO SEGINTE ELEMENTO: ");
					console.log(el.parentNode.parentNode.relatedObj);

					if (getBeforeOrAfterOrEndAllocate(el.parentNode, event)) {
						el.parentNode.parentNode.relatedObj.commands_block.splice($(el.parentNode).data('index'), 0, createElementGenericFunction());
					} else {
						el.parentNode.parentNode.relatedObj.commands_block.splice($(el.parentNode).data('index') + 1, 0, createElementGenericFunction());
					}
				}

			}
			

			/*console.log("elemento superior: ");
			console.log(programa.funcoes[function_to_add].comandos[caminho[0]]);
			console.log("elemento específico: 
			console.log(findElementByPath(caminho));*/

		}

	}


	//console.log("onde soltou:");
	//console.log(el);

	
	has_element_created_draged = false;
	which_element_is_draged = null;
	function_to_add = -1;

	renderAlgorithm();

}

function findNearbyCommandToAddInFunctionScope(el, event, node_list_commands, function_obj, command_type) {

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

	borda_inferior = elemento_menor_distancia.parentNode.getBoundingClientRect().top + elemento_menor_distancia.parentNode.getBoundingClientRect().height;
	
	// Está mais próximo da borda de baixo, ou seja.. inserir por último:
	if ((borda_inferior - event.clientY) < menor_distancia) {
		function_obj.commands.push(genericCreateCommand(command_type));
	} else {
		function_obj.commands.splice($(elemento_menor_distancia).data('index'), 0, genericCreateCommand(command_type));
	}
}

function findPositionAndPathToElementTarget(el, event) {

	var full_path = [];
	var m;

	if (typeof $(el).data('fullpath') !== 'undefined') {
		m = $(el).data('fullpath');
	} else {
		
		var hier = $(el).parentsUntil(".all_functions");
		for (var i = 0; i < hier.length; i++) {
			if (typeof $(hier[i]).data('fullpath') !== 'undefined') {
				m = $(hier[i]).data('fullpath');
				break;
			}
		}

	}

	if (isNaN(m)) {
		full_path = m.split(',');
		/*for (i = 0; i < full_path.length; i++) {
			full_path[i] = parseInt(full_path[i]);
		}*/
		return full_path;
	} else {
		return [m];
	}

}
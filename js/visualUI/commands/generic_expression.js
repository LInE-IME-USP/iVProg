import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as VariableValueMenuManagement from './variable_value_menu';
import { registerUserEvent, ActionTypes } from "../../services/userLog";
import WatchJS from 'melanke-watchjs';

window.timer = false;

export function renderExpression (command, function_obj, div_to_render, expression_array) {

	div_to_render.empty();

	WatchJS.unwatch(command, "expression");
	WatchJS.watch(command, "expression", function(){
		if (window.timer)
			return;
		var m = div_to_render.find('.single_element_expression').not('.mouse_distance').not('.add_parentheses');
		var s = "";
		m.each(function(e){
		   if ($(this).hasClass('parentheses_in_expression')) {
		   		s += ($(this).text()) + " ";
		   } else {
		        s += ($(this).find('.text').text());
		        
		        s += ($(this).find('.var_name').text());

		        s += ($(this).find('.parameters_function_called').text());

		        s += ($(this).find('.value_rendered').text());

		        s += " ";
		   }
		});
		if (s) {
			window.timer = true;
		} else {
			return;
		}
		registerUserEvent(function_obj.name, ActionTypes.CHANGE_COMMAND_EXP, command.type, '/', s);
		setTimeout(function() {
			window.timer = false;
		}, 200);
	}, 20, true);

	if (command.type === Models.COMMAND_TYPES.attribution) {

		WatchJS.unwatch(command.variable);
		WatchJS.watch(command.variable, function(){
	      renderExpression(command, function_obj, div_to_render, expression_array);
	    }, 0);

		if (command.variable.content) {

			var types_included = [];

			if (command.variable.content.type == Types.INTEGER || command.variable.content.type == Types.REAL) {
	    		types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		    } else if (command.variable.content.type == Types.BOOLEAN) {
		    	types_included.push(Models.EXPRESSION_TYPES.exp_conditional);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_logic);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		    } else if (command.variable.content.type == Types.TEXT) {
		    	types_included.push(Models.EXPRESSION_TYPES.exp_conditional);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_logic);
		    	types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		    } else {
		    	console.log('NÃO RECONHECI! VEJA: ', command.variable.content.type);
		    }

		    renderElements(command, function_obj, div_to_render, expression_array, types_included);
		} else {
			div_to_render.text(LocalizedStrings.getUI('var_menu_select_var').toLowerCase());
		}
	} else {
		var types_included = [];
		types_included.push(Models.EXPRESSION_TYPES.exp_conditional);
		types_included.push(Models.EXPRESSION_TYPES.exp_logic);
		types_included.push(Models.EXPRESSION_TYPES.exp_arithmetic);
		renderElements(command, function_obj, div_to_render, expression_array, types_included);
	}

	div_to_render.children('.mouse_distance').addClass('mouse_distance_hidden');
	div_to_render.children('.higher_element').on('mousemove', function(evt) {
		if (!window.open_or_close) {
			$(this).css('position', 'relative', '!important');
			$(this).children('.mouse_distance').css('opacity', '1');
		}
	});
	div_to_render.children('.higher_element').on('mouseout', function(evt) {
		if (!window.open_or_close) {
			$(this).css('position', 'absolute', '!important');
			$(this).children('.mouse_distance').css('opacity', '0');
		}
	});
	var lixeira = $('<div class="lixeira" draggable="true"></div>');

	div_to_render.find('.single_element_expression').on('mousedown', function (evt) {
		window.posX = evt.clientX;
		window.posY = evt.clientY;
	});
	
	Sortable.create(div_to_render[0], {
	    animation: 100,
	    ghostClass: 'ghost',
	    group: {
	        name: 'shared',
	        put: false // Do not allow items to be put into this list
	    },
	    draggable: '.single_element_expression',
	    sort: false,
	    filter: '.not_allowed',
	    
	    onStart: function() {
	    	$('body').append(lixeira);
	    	lixeira.css('display', 'block');
			lixeira.css('top', window.posY + 70, '!important');
			lixeira.css('left', window.posX - 20, '!important');
	    },
	    onMove: function() {
	    	lixeira.addClass('color_test');
	    },
	    onEnd: function() {
	    	lixeira.remove();
	    	div_to_render.find('.ghost').removeClass('ghost');
	    }
  	});
  	new Sortable(lixeira[0], {
	    group: 'shared',
	    animation: 150,
	    onAdd: function (evt) {
	       lixeira.css('display', 'none');
	       lixeira.find('.single_element_expression').remove();
	       lixeira.css('background-color', '');
	       lixeira.remove();
	       removeElement(evt, expression_array);
	       renderExpression(command, function_obj, div_to_render, expression_array);
	    }
	});
}

function removeElement (event, expression_array) {
	var indice = $(event.item).data('index');
	var first = expression_array[0];
	console.log('indice: ', indice);
	if (expression_array[indice].type) {
		// if is alone in expression:
		if (expression_array.length == 1) {
			//function_obj.commands.splice(function_obj.commands.indexOf(command), 1);
			expression_array.splice(0, 1);
		} else if (expression_array.length > 1) {
			if (indice > 0 && expression_array[indice - 1].type_op) {
				if (indice < (expression_array.length) 
					&& expression_array[indice - 2] == '('
					&& expression_array[indice + 1].type_op) {
					expression_array.splice(indice + 1, 1);
				}
				expression_array.splice(indice, 1);
				expression_array.splice(indice - 1, 1);
				if (indice - 2 < (expression_array.length) 
					&& expression_array[indice - 2] == '('
					&& expression_array[indice - 1] == ')') {
					expression_array.splice(indice - 1, 1);
					expression_array.splice(indice - 2, 1);
					if (indice - 3 >= 0 && indice - 3 < expression_array.length
						&& expression_array[indice - 3].type_op ) {
						expression_array.splice(indice - 3, 1);
					}
				}
			} else if (indice < (expression_array.length - 1) &&  expression_array[indice + 1].type_op) {
				expression_array.splice(indice + 1, 1);
				expression_array.splice(indice, 1);
			} else if (indice < (expression_array.length - 1) && indice > 0 
				&& expression_array[indice -1] == '(' && expression_array[indice +1] == ')') {
				if (indice > 1
					&& expression_array[indice - 2].type_op) {
					expression_array.splice(indice + 1, 1);
					expression_array.splice(indice, 1);
					expression_array.splice(indice - 1, 1);
					expression_array.splice(indice - 2, 1);

				} else if (indice < (expression_array.length - 2) 
					&& expression_array[indice + 2].type_op) {
					expression_array.splice(indice + 1, 1);
					expression_array.splice(indice, 1);
					expression_array.splice(indice - 1, 1);

				} else {
					expression_array.splice(indice + 1, 1);
					expression_array.splice(indice, 1);
					expression_array.splice(indice - 1, 1);
				}
			}
		}

	} else if (expression_array[indice].type_op) {
		// iVProg doesn't support operator remove
	} else {
		
		var opening = -1;
		var closing = -1;

		if (expression_array[indice] == '(') {
			opening = indice;
			for (var i = indice + 1; i < expression_array.length; i++) {
				if (expression_array[i] == ')') {
					closing = i;
					break;
				}
			}
		} else {
			closing = indice;
			for (var i = indice - 1; i >= 0; i--) {
				if (expression_array[i] == '(') {
					opening = i;
					break;
				}
			}
		}

		if (expression_array[opening + 1].type_op) {
			expression_array.splice(closing, 1);
			expression_array.splice(opening + 1, 1);
			expression_array.splice(opening, 1);
		} else {
			expression_array.splice(closing, 1);
			expression_array.splice(opening, 1);
		}
	}
	// if expression is empty, add a new var value:
	if (expression_array.length == 0) {
		expression_array.push(new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
	}
	if (first != expression_array[0] && expression_array[0].type_op) {
		expression_array.splice(0, 1);
	}
}

function renderElements (command, function_obj, div_to_render, expression_array, types_included) {

	/*if (expression_array.length > 0) {
		if (!expression_array[0].type_op) {
			renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, 0);
		}
	}*/

	var i = 0;
	for (i = 0; i < expression_array.length; i++) {
		if (expression_array[i].type == "var_value") {
			var div_temp = $('<div class="single_element_expression" data-index="'+i+'"></div>');
			if (i == 0) {
				if (expression_array.length > 0  && !expression_array[0].type_op) {

					renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, 0);
				}
			}
			VariableValueMenuManagement.renderMenu(command, expression_array[i], div_temp, function_obj);
			div_to_render.append(div_temp);
		} else if (expression_array[i] == '(' || expression_array[i] == ')') {
			if (expression_array[i] == ')') {
				renderFinalAddElements(div_to_render, types_included, expression_array, command, function_obj, i);
				renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
			} else if (expression_array[i] == '(' && !expression_array[i + 1].type_op) {
				renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
				renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, i + 1);
			} else {
				renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
			}

		} else {
			if (i == 0) {
				console.log("NEGAÇÃO NO PRIMEIRO ELEMENTO");
			} else if (expression_array[i - 1] == '(') {
				console.log("NEGAÇÃO APÓS O PARÊNTESES");
			}
			renderOperatorMenu(command, function_obj, div_to_render, expression_array[i], types_included, i, expression_array);
		}
	}

	renderFinalAddElements(div_to_render, types_included, expression_array, command, function_obj, i, true);

	renderAddParenthesis(command, function_obj, div_to_render, expression_array, types_included);

}

window.parentheses_activate = false;
window.open_or_close = null;
function renderAddParenthesis (command, function_obj, div_to_render, expression_array, types_included) {
	var addParentheses = $('<div class="single_element_expression add_parentheses not_allowed"><i class="icons"><b style="font-style: normal;">( )</b><i class="corner add icon blue" style="font-size: .6em;right: -3px;bottom: -2px;"></i></i></div>');
	div_to_render.append(addParentheses);
	addParentheses.popup({
	    content : "Adicionar parênteses",
	    delay: {
	      show: 750,
	      hide: 0
		}
	});

	addParentheses.on('click', function(mouse_event) {

		// verificar se já está ativado
		if (window.parentheses_activate) {
			return;
		}

		div_to_render.find('.usepointer').off('click');

		window.parentheses_activate = true;

		window.open_or_close = "open";

		div_to_render.find('.dropdown').addClass('disabled');

		div_to_render.find('.ghost_element').addClass('temp_class');
		div_to_render.find('.ghost_element').removeClass('ghost_element');

		var floatingObject = $('<div class="floating_parenthesis"> ( </div>');
		floatingObject.draggable().appendTo("body");
		floatingObject.css("position", "absolute");
		mouse_event.type = "mousedown.draggable";
		mouse_event.target = floatingObject[0];
		floatingObject.css("left", mouse_event.pageX + 10);
		floatingObject.css("top", mouse_event.pageY + 10);
		floatingObject.trigger(mouse_event);

		div_to_render.on('mousemove', function(evt) {
			var actual_target = null;
			if ($(evt.target).hasClass('single_element_expression')) {
				actual_target = $(evt.target);
			} else {
				actual_target = $(evt.target).closest('.single_element_expression');
			}

			if ($(evt.target).hasClass('temp_class') 
				|| actual_target.length < 1 
				|| actual_target.hasClass('add_parentheses')
				|| actual_target.hasClass('rendered_parentheses')
				|| $(evt.target).hasClass('expression_elements')) {
				return;
			}
			renderGhostParentheses(actual_target, command, function_obj, div_to_render, expression_array);
		});

		div_to_render.on('mouseleave', function() {
			/*window.open_parentheses.remove();
			window.close_parentheses.remove();*/
		});

		var floating;

		$('body').on('mouseup', function(evt) {

			if (window.open_or_close == "open") {

				window.open_or_close = "close";

				floatingObject.remove();

				var comando_que_esta = $(evt.target).closest('.command_container');
				var comando_certo = div_to_render.closest('.command_container');
				if (!comando_que_esta.is(comando_certo)) {

					window.parentheses_activate = false;
					div_to_render.find('.temp_class').addClass('ghost_element');
					div_to_render.find('.temp_class').removeClass('temp_class');
					div_to_render.off('mousemove');
					div_to_render.off('mouseleave');
					$('body').off('mouseup');
					window.open_parentheses.remove();
					window.close_parentheses.remove();
					window.inserir_open = -1;
					window.inserir_close = -1;
					window.open_or_close = null;
					renderExpression(command, function_obj, div_to_render, expression_array);

					return;
				}


				window.open_parentheses.addClass('parentheses_fixed');

				floating = $('<div class="floating_parenthesis"> ) </div>');
				floating.draggable().appendTo("body");
				floating.css("position", "absolute");
				floating.css("left", evt.pageX + 10);
				floating.css("top", evt.pageY + 10);

				$('body').on('mousemove', function(evts) {
					floating.css("left", evts.pageX + 10);
					floating.css("top", evts.pageY + 10);
				});				

			} else {

				floating.remove();

				window.open_parentheses.removeClass('parentheses_fixed');
				
				div_to_render.off('mousemove');
				div_to_render.off('mouseleave');
				$('body').off('mouseup');

				setTimeout(function(){
					window.parentheses_activate = false;
				}, 50);

				var comando_que_esta = $(evt.target).closest('.command_container');
				var comando_certo = div_to_render.closest('.command_container');
				var is_correct = false;
				if (comando_que_esta.is(comando_certo)) {
					is_correct = true;
				}

				if (is_correct) {
					expression_array.splice(window.inserir_open, 0, '(');
					expression_array.splice(window.inserir_close, 0, ')');
				}

				window.inserir_open = -1;
				window.inserir_close = -1;
				window.open_or_close = null;

				renderExpression(command, function_obj, div_to_render, expression_array);

			}

		});
	});
}

window.open_parentheses = $('<div class="parentheses_ghost">(</div>');
window.close_parentheses = $('<div class="parentheses_ghost">)</div>');
window.inserir_open = -1;
window.inserir_close = -1;
function renderGhostParentheses (actual_target, command, function_obj, div_to_render, expression_array) {

	/*window.open_parentheses.remove();
	window.close_parentheses.remove();*/

	var index_in_array = actual_target.data('index');

	if ((expression_array[index_in_array] == '(') || (expression_array[index_in_array] == ')')) {
		return;
	}

	if (window.open_or_close == "close") {
		if (index_in_array < window.inserir_open) {
			return;
		}
	}

	// Tratando a situação quando é na primeira posição:

	if (index_in_array == 0) {
		if (expression_array[index_in_array].type == "var_value") {

			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}

			/*if (expression_array.length == 1) {
				if (window.open_or_close == "close") {
					window.close_parentheses.insertAfter(actual_target);
					window.inserir_close = index_in_array + 2;
				}*/

			//} else {
				var count_opened = 0;
				var count_closed = 0;
				for (var i = 0; i < expression_array.length; i++) {
					if ((expression_array[i] == '(')) {
						count_opened ++;
					}
					if (expression_array[i] == ')') {
						count_closed ++;
					}
					if (count_opened != count_closed) {
					} else {
						if (count_opened > 0) {
							if (window.open_or_close == "close") {
								window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
								window.inserir_close = i + 2;
							}
							break;
						} else {
							if (expression_array[i].type == "var_value") {
								if (window.open_or_close == "close") {
									window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
									window.inserir_close = i + 2;
								}
								break;
							}
						}
					}

				}
			//}

		} else if (expression_array[index_in_array].type_op) {

			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}

			var count_opened = 0;
			var count_closed = 0;
			for (var i = 1; i < expression_array.length; i++) {
				// $('.slide-link[data-slide="0"]')
				if ((expression_array[i] == '(')) {
					count_opened ++;
				}
				if (expression_array[i] == ')') {
					count_closed ++;
				}
				if (count_opened != count_closed) {
				} else {
					if (count_opened > 0) {
						if (expression_array[i].type == "var_value") {
							window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
							window.inserir_close = i + 2;
						}

						break;
					} else {
						if (expression_array[i].type == "var_value") {
							if (expression_array[i].type == "var_value") {
								window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+i+'"]'));
								window.inserir_close = i + 2;
							}
							break;
						}
					}
				}

			}
		}
		return;
	}

	// Tratando quando não é no índice 0:
	if (expression_array[index_in_array].type == "var_value") {
		if (window.open_or_close == "open") {
			window.open_parentheses.insertBefore(actual_target);
			window.inserir_open = index_in_array;
		}
		if (window.open_or_close == "close") {
			window.close_parentheses.insertAfter(actual_target);
			window.inserir_close = index_in_array + 2;
		}
		return;
	}

	if (expression_array[index_in_array].type_op) {
		// buscar para a esquerda primeiro:
		if (expression_array[index_in_array - 1] == '(') {
			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}
		} else if (expression_array[index_in_array - 1] == ')') {
			// buscar a abertura
			var count_opened = 0;
			var count_closed = 0;
			for (var j = index_in_array - 1; j >= 0; j--) {
				if ((expression_array[j] == '(')) {
					count_opened ++;
				}
				if (expression_array[j] == ')') {
					count_closed ++;
				}

				if (count_opened != count_closed) {
				} else {
					if (count_closed > 0) {
						if (window.open_or_close == "open") {
							window.open_parentheses.insertBefore(div_to_render.find('.single_element_expression[data-index="'+j+'"]'));
							window.inserir_open = j;
						}
						break;
					}
				}

			}

		} else if (expression_array[index_in_array - 1].type == "var_value") {
			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(div_to_render.find('.single_element_expression[data-index="'+(index_in_array - 1)+'"]'));
				window.inserir_open = index_in_array - 1;
			}
		}

		// buscar para a direita agora:
		if (expression_array[index_in_array + 1] == '(') {
			// buscar o fechamento:

			var count_opened = 0;
			var count_closed = 0;
			for (var j = index_in_array + 1; j < expression_array.length; j ++) {
				if ((expression_array[j] == '(')) {
					count_opened ++;
				}
				if (expression_array[j] == ')') {
					count_closed ++;
				}

				if (count_opened != count_closed) {
				} else {
					if (count_opened > 0) {
						if (window.open_or_close == "close") {
							window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+j+'"]'));
							window.inserir_close = j + 2;
						}
						break;
					}
				}

			}


		} else if (expression_array[index_in_array + 1].type == "var_value") {
			if (window.open_or_close == "close") {
				window.close_parentheses.insertAfter(div_to_render.find('.single_element_expression[data-index="'+(index_in_array + 1)+'"]'));
				window.inserir_close = index_in_array + 3;
			}
		}
	}

}

function renderParenthesis (div_to_render, expression_content, command, function_obj, position, expression_array) {
	var ghost_parenthesis = $('<div class="single_element_expression parentheses_in_expression" data-index="'+position+'">'+expression_content+'</div>');
	div_to_render.append(ghost_parenthesis);
}

function renderStartAddOperator (div_to_render, types_included, expression_array, command, function_obj, position) {
	var menu_final = '<div class="ui dropdown disabled usepointer"><div class="text"> + </div><i class="dropdown icon"></i><div class="menu">';
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Aritméticos<div class="menu">';
			menu_final += getArithmeticOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getArithmeticOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Lógicos<div class="menu">';
			menu_final += getLogicOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getLogicOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Relacionais<div class="menu">';
			menu_final += getRelationalOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getRelationalOperators();
		}
	}
	menu_final += '</div></div>';

	menu_final = $(menu_final);
	var div_temp = $('<div class="single_element_expression ghost_element mouse_distance"></div>');
	div_temp.append(menu_final);
	var div_higher = $('<div class="higher_element"></div>');
	div_higher.append(div_temp);
	div_to_render.append(div_higher);
	menu_final.dropdown('set selected', Models.ARITHMETIC_TYPES.minus);

	div_temp.on('click', function() {
		if (!window.open_or_close) {
			var sera = position;

			if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
				console.log('p1');
				expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_arithmetic,Models.ARITHMETIC_TYPES.minus));
			} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
				console.log('p2');
				expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_logic,Models.LOGIC_COMPARISON.equals_to));
			} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
				console.log('p3');
				expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_conditional,Models.ARITHMETIC_COMPARISON.greater_than));
			}

			renderExpression(command, function_obj, div_to_render, expression_array);
		}
	});
}

function renderFinalAddElements (div_to_render, types_included, expression_array, command, function_obj, position, is_last = false) {

	var menu_final = '<div class="ui dropdown disabled usepointer"><div class="text"> + </div><i class="dropdown icon"></i><div class="menu">';
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Aritméticos<div class="menu">';
			menu_final += getArithmeticOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getArithmeticOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Lógicos<div class="menu">';
			menu_final += getLogicOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getLogicOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Relacionais<div class="menu">';
			menu_final += getRelationalOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getRelationalOperators();
		}
	}
	menu_final += '</div></div>';

	menu_final = $(menu_final);
	var div_temp = $('<div class="simple_add mouse_distance"></div>');
	var div_higher = $('<div class="higher_element"></div>');

	var button = $('<button class="ui button green add_expression"><i class="plus circle inverted icon"></i></button>');
	div_temp.append(button);

	if (!is_last) {
		div_higher.append(div_temp);
		div_to_render.append(div_higher);
		//div_temp.append(menu_final);
		div_temp.css('opacity', '0', '!important');
	} else {
		div_temp.removeClass('mouse_distance');
		div_temp.css('opacity', '1', '!important');
		//div_temp.append(menu_final);
		div_to_render.append(div_temp);
	}
	

	menu_final.dropdown('set selected', Models.ARITHMETIC_TYPES.plus);

	div_temp.on('click', function() {
		var sera = position;

		if (expression_array[sera] == ')' && expression_array[sera - 1] == '(') {
			expression_array.splice(sera, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
			renderExpression(command, function_obj, div_to_render, expression_array);
			return;
		}

		if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_arithmetic,Models.ARITHMETIC_TYPES.plus));
			expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_logic,Models.LOGIC_COMPARISON.equals_to));
			expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_conditional,Models.ARITHMETIC_COMPARISON.greater_than));
			expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
		}

		renderExpression(command, function_obj, div_to_render, expression_array);
	});
}

function renderOperatorMenu (command, function_obj, div_to_render, expression_element, types_included, position, expression_array) {

	var menu_final = '<div class="ui dropdown"><div class="text"> + </div><i class="dropdown icon"></i><div class="menu">';
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Aritméticos<div class="menu">';
			menu_final += getArithmeticOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getArithmeticOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Lógicos<div class="menu">';
			menu_final += getLogicOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getLogicOperators();
		}
	}
	if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
		if (types_included.length > 1) {
			menu_final += '<div class="item"><i class="dropdown icon"></i>Relacionais<div class="menu">';
			menu_final += getRelationalOperators();
			menu_final += '</div></div>';
		} else {
			menu_final += getRelationalOperators();
		}
	}
	menu_final += '</div></div>';

	menu_final = $(menu_final);
	var div_temp = $('<div class="single_element_expression not_allowed" data-index="'+position+'"></div>');
	div_temp.append(menu_final);
	div_to_render.append(div_temp);
	menu_final.dropdown({
		onChange: function(value, text, $selectedItem) {
      		expression_element.item = $selectedItem.data('value');
      		expression_element.type_op = $selectedItem.data('type');
    	}
	});

	menu_final.dropdown('set selected', expression_element.item);
}

function getArithmeticOperators () {
	var arithmetic_operators;
	arithmetic_operators = '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.plus+'">+</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.minus+'">-</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.multiplication+'">*</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.division+'">/</div>';
	arithmetic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_arithmetic+'" data-value="'+Models.ARITHMETIC_TYPES.module+'">%</div>';
	return arithmetic_operators;
}

function getLogicOperators () {
	var logic_operators;
	logic_operators = '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.equals_to+'">==</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.not_equals_to+'">!=</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.and+'">'+LocalizedStrings.getUI('and')+'</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.or+'">'+LocalizedStrings.getUI('or')+'</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.not+'">'+LocalizedStrings.getUI('not')+'</div>';
	return logic_operators;
}

function getRelationalOperators () {
	var relational_operators;
	relational_operators = '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.greater_than+'">></div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.less_than+'"><</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.equals_to+'">==</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.not_equals_to+'">!=</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.greater_than_or_equals_to+'">>=</div>';
	relational_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_conditional+'" data-value="'+Models.ARITHMETIC_COMPARISON.less_than_or_equals_to+'"><=</div>';
	return relational_operators;
}
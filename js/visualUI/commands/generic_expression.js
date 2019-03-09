import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenuManagement from './variable_value_menu';
import WatchJS from 'melanke-watchjs';

export function renderExpression (command, function_obj, div_to_render, expression_array) {

	div_to_render.empty();

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
			div_to_render.text('selecione uma variável');
		}
	}
}

function renderElements (command, function_obj, div_to_render, expression_array, types_included) {

	if (expression_array.length > 0) {
		if (!expression_array[0].type_op) {
			renderStartAddOperator(div_to_render, types_included, expression_array, command, function_obj, 0);
		}
	}

	var i = 0;
	for (i = 0; i < expression_array.length; i++) {
		if (expression_array[i].type == "var_value") {
			var div_temp = $('<div class="single_element_expression" data-index="'+i+'"></div>');
			VariableValueMenuManagement.renderMenu(command, expression_array[i], div_temp, function_obj);
			div_to_render.append(div_temp);
		} else if (expression_array[i] == '(' || expression_array[i] == ')') {
			renderParenthesis(div_to_render, expression_array[i], command, function_obj, i, expression_array);
		} else {
			renderOperatorMenu(command, function_obj, div_to_render, expression_array[i], types_included, i, expression_array);
		}
	}

	renderFinalAddElements(div_to_render, types_included, expression_array, command, function_obj, i);

	renderAddParenthesis(command, function_obj, div_to_render, expression_array, types_included);

}

window.parentheses_activate = false;
window.open_or_close = null;
function renderAddParenthesis (command, function_obj, div_to_render, expression_array, types_included) {
	var addParentheses = $('<div class="single_element_expression add_parentheses"><i class="icons"><b style="font-style: normal;">( )</b><i class="corner add icon blue" style="font-size: .6em;right: -3px;bottom: -2px;"></i></i></div>');
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
				|| $(evt.target).hasClass('parentheses_ghost')
				|| $(evt.target).hasClass('expression_elements')) {
				return;
			}
			renderGhostParentheses(actual_target, command, function_obj, div_to_render, expression_array);
		});

		div_to_render.on('mouseleave', function(evt) {
			/*window.open_parentheses.remove();
			window.close_parentheses.remove();*/
		});

		var floating;

		$('body').on('mouseup', function(evt) {

			if (window.open_or_close == "open") {

				window.open_or_close = "close";

				floatingObject.remove();

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
				
				div_to_render.find('.temp_class').addClass('ghost_element');
				div_to_render.find('.temp_class').removeClass('temp_class');
				
				div_to_render.off('mousemove');
				div_to_render.off('mouseleave');
				$('body').off('mouseup');

				setTimeout(function(){
					window.parentheses_activate = false;
				}, 50);

				window.open_parentheses.remove();
				window.close_parentheses.remove();

				var all_el = $(evt.target).parentsUntil('.command_container');

				var is_correct = false;
				
				for (var j = 0; j < all_el.length; j++) {
					if ($(all_el.get(j)).is(div_to_render)) {
						is_correct = true;
						break;
					}
				}

				if (is_correct) {
					expression_array.splice(window.inserir_open, 0, '(');
					expression_array.splice(window.inserir_close, 0, ')');
					renderExpression(command, function_obj, div_to_render, expression_array);
				}

				window.inserir_open = -1;
				window.inserir_close = -1;

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

	// Tratando a situação quando é na primeira posição:

	if (index_in_array == 0) {
		if (expression_array[index_in_array].type == "var_value") {

			if (window.open_or_close == "open") {
				window.open_parentheses.insertBefore(actual_target);
				window.inserir_open = index_in_array;
			}

			if (expression_array.length == 1) {
				if (window.open_or_close == "close") {
					window.close_parentheses.insertAfter(actual_target);
					window.inserir_close = index_in_array + 2;
				}

			} else {
				var count_opened = 0;
				var count_closed = 0;
				for (var i = 1; i < expression_array.length; i++) {
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
			}

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
	var ghost_parenthesis = $('<div class="single_element_expression" data-index="'+position+'">'+expression_content+'</div>');
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
	var div_temp = $('<div class="single_element_expression ghost_element"></div>');
	div_temp.append(menu_final);
	div_to_render.append(div_temp);
	menu_final.dropdown('set selected', Models.ARITHMETIC_TYPES.minus);

	div_temp.on('click', function() {
		var sera = position;

		if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_arithmetic,Models.ARITHMETIC_TYPES.minus));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_logic,Models.LOGIC_COMPARISON.equals_to));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_conditional,Models.ARITHMETIC_COMPARISON.greater_than));
		}

		renderExpression(command, function_obj, div_to_render, expression_array);
	});
}

function renderFinalAddElements (div_to_render, types_included, expression_array, command, function_obj, position) {

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
	var div_temp = $('<div class="single_element_expression ghost_element"></div>');
	div_temp.append(menu_final);
	div_to_render.append(div_temp);
	menu_final.dropdown('set selected', Models.ARITHMETIC_TYPES.plus);

	div_temp.on('click', function() {
		var sera = position;

		if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_arithmetic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_arithmetic,Models.ARITHMETIC_TYPES.plus));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_logic) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_logic,Models.LOGIC_COMPARISON.equals_to));
		} else if (types_included.indexOf(Models.EXPRESSION_TYPES.exp_conditional) >= 0) {
			expression_array.splice(sera, 0, new Models.ExpressionOperator(Models.EXPRESSION_TYPES.exp_conditional,Models.ARITHMETIC_COMPARISON.greater_than));
		}
		expression_array.splice(sera + 1, 0, new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));

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
	var div_temp = $('<div class="single_element_expression" data-index="'+position+'"></div>');
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
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.and+'">&&</div>';
	logic_operators += '<div class="item" data-type="'+Models.EXPRESSION_TYPES.exp_logic+'" data-value="'+Models.LOGIC_COMPARISON.or+'">||</div>';
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
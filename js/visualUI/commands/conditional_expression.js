import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as VariableValueMenuManagement from './variable_value_menu';


export function renderExpression (command, expression, function_obj, initial_el_to_render) {

	console.log("Rendered! :)");


	if (expression.expression == null || expression.expression.length < 1) {

		renderStartMenu(command, expression, function_obj, initial_el_to_render);

	} else {
		
		var main_div = $('<div class="expression_elements"></div>');

		switch (expression.expression.type) {
			case Models.EXPRESSION_TYPES.exp_logic:
				renderLogicExpression(command, expression, expression.expression, function_obj, main_div);
				break;
			case Models.EXPRESSION_TYPES.exp_arithmetic:
				renderArithmeticExpression(command, expression, expression.expression, function_obj, main_div);
				break;
		}

		initial_el_to_render.append(main_div);	
	}
}

function renderArithmeticOperator () {

}

function renderLogicOperator (command, all_expression, expression_logic, logic_operator, function_obj, element_to_append) {

//export const ARITHMETIC_COMPARISON = Object.freeze({greater_than:"greater_than", less_than:"less_than", equals_to:"equals_to", not_equals_to:"not_equals_to", greater_than_or_equals_to:"greater_than_or_equals_to", less_than_or_equals_to:"less_than_or_equals_to"});

	var menu_operator = $('<div class="ui dropdown"><div class="text"></div><i class="dropdown icon"></i></div>');
	menu_operator.dropdown({
	    values: [
	      {
	        name     : '==',
	        value    : Models.LOGIC_COMPARISON.equals_to,
	        selected : (logic_operator == Models.LOGIC_COMPARISON.equals_to)
	      },
	      {
	        name     : '!=',
	        value    : Models.LOGIC_COMPARISON.not_equals_to,
	        selected : (logic_operator == Models.LOGIC_COMPARISON.not_equals_to)
	      },
	      {
	        name     : '&&',
	        value    : Models.LOGIC_COMPARISON.and,
	        selected : (logic_operator == Models.LOGIC_COMPARISON.and)
	      },
	      {
	        name     : '||',
	        value    : Models.LOGIC_COMPARISON.or,
	        selected : (logic_operator == Models.LOGIC_COMPARISON.or)
	      }
	    ],
	    onChange: function(value, text, $selectedItem) {
	    	expression_logic.operator = value;
	    }
	  })
	;

	element_to_append.append(menu_operator);

}

function renderLogicExpression (command, all_expression, expression_logic, function_obj, element_to_append) {

	var exp_el_par_1 = $('<div class="expression_element"> ( </div>');
	var exp_el_expr_el_1 = $('<div class="expression_element"></div>');
	var exp_el_expr_operand = $('<div class="expression_element"></div>');
	var exp_el_expr_el_2 = $('<div class="expression_element"></div>');
	var exp_el_par_2 = $('<div class="expression_element"> ) </div>');

	if (expression_logic.first_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		renderLogicExpression(command, all_expression, expression_logic.first_operand, function_obj, exp_el_expr_el_1);
	} else if (expression_logic.first_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		renderArithmeticExpression(command, all_expression, expression_logic.first_operand, function_obj, exp_el_expr_el_1);
	} else { // var_value:
		VariableValueMenuManagement.renderMenu(command, expression_logic.first_operand, exp_el_expr_el_1, function_obj);
	}

	if (expression_logic.second_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		renderLogicExpression(command, all_expression, expression_logic.second_operand, function_obj, exp_el_expr_el_2);
	} else if (expression_logic.second_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		renderArithmeticExpression(command, all_expression, expression_logic.second_operand, function_obj, exp_el_expr_el_2);
	} else { // var_value: 
		VariableValueMenuManagement.renderMenu(command, expression_logic.second_operand, exp_el_expr_el_2, function_obj);
	}

	renderLogicOperator(command, all_expression, expression_logic, expression_logic.operator, function_obj, exp_el_expr_operand);

	element_to_append.append(exp_el_par_1);
	element_to_append.append(exp_el_expr_el_1);
	element_to_append.append(exp_el_expr_operand);
	element_to_append.append(exp_el_expr_el_2);
	element_to_append.append(exp_el_par_2);

}

function renderArithmeticExpression (command, all_expression, expression_arithmetic, function_obj, element_to_append) {}

function renderStartMenu (command, expression, function_obj, initial_el_to_render) {
	var start_menu = '';
	start_menu += '<div class="ui dropdown"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	start_menu += '<div class="item" data-exp="'+Models.EXPRESSION_TYPES.exp_logic+'">'+LocalizedStrings.getUI('text_logic_expression')+'</div>';
	start_menu += '<div class="item" data-exp="'+Models.EXPRESSION_TYPES.exp_arithmetic+'">'+LocalizedStrings.getUI('text_arithmetic_expression')+'</div>';
	start_menu += '</div></div>';
	start_menu = $(start_menu);

	start_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
			switch ($selectedItem.data('exp')) {
				case Models.EXPRESSION_TYPES.exp_logic:
					expression.expression = 
						new Models.LogicExpression(false, 
							new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), 
							new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), 
							Models.LOGIC_COMPARISON.equals_to);
					break;
				case Models.EXPRESSION_TYPES.exp_arithmetic:
					expression.expression = 
						new Models.ArithmeticExpression(
							new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), 
							new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true), 
							Models.ARITHMETIC_COMPARISON.less_than);
					break;
			}

			initial_el_to_render.html('');

			renderExpression(command, expression, function_obj, initial_el_to_render);

    	}
	});

	initial_el_to_render.append(start_menu);
}
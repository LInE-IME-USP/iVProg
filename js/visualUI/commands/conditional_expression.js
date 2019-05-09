import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as VariableValueMenuManagement from './variable_value_menu';


export function renderExpression (command, expression, function_obj, initial_el_to_render) {

	expression.dom_object = initial_el_to_render;

	if (expression.expression == null || expression.expression.length < 1) {

		renderStartMenu(command, expression, function_obj, initial_el_to_render);

	} else {
		
		var main_div = $('<div class="expression_elements"></div>');

		switch (expression.expression.type) {
			case Models.EXPRESSION_TYPES.exp_logic:
				renderLogicExpression(command, expression, expression.expression, function_obj, main_div, initial_el_to_render);
				break;
			case Models.EXPRESSION_TYPES.exp_arithmetic:
				renderArithmeticExpression(command, expression, expression.expression, function_obj, main_div);
				break;
		}

		initial_el_to_render.append(main_div);	
		var restartMenu = $('<div class="ui restart_expression"><i class="ui icon undo"></i></div>');
		initial_el_to_render.append(restartMenu);	
		restartMenu.on('click', function(e){
	    	expression.expression = null;
	    	initial_el_to_render.empty();
	    	renderExpression(command, expression, function_obj, initial_el_to_render);
	  	});
	}
}

function renderArithmeticOperator (command, all_expression, expression_arithmetic, arithmetic_operator, function_obj, element_to_append) {

	var menu_operator = $('<div class="ui dropdown"><div class="text"></div><i class="dropdown icon"></i></div>');
	menu_operator.dropdown({
	    values: [
	      {
	        name     : '>',
	        value    : Models.ARITHMETIC_COMPARISON.greater_than,
	        selected : (arithmetic_operator == Models.ARITHMETIC_COMPARISON.greater_than)
	      },
	      {
	        name     : '<',
	        value    : Models.ARITHMETIC_COMPARISON.less_than,
	        selected : (arithmetic_operator == Models.ARITHMETIC_COMPARISON.less_than)
	      },
	      {
	        name     : '==',
	        value    : Models.ARITHMETIC_COMPARISON.equals_to,
	        selected : (arithmetic_operator == Models.ARITHMETIC_COMPARISON.equals_to)
	      },
	      {
	        name     : '!=',
	        value    : Models.ARITHMETIC_COMPARISON.not_equals_to,
	        selected : (arithmetic_operator == Models.ARITHMETIC_COMPARISON.not_equals_to)
	      },
	      {
	        name     : '>=',
	        value    : Models.ARITHMETIC_COMPARISON.greater_than_or_equals_to,
	        selected : (arithmetic_operator == Models.ARITHMETIC_COMPARISON.greater_than_or_equals_to)
	      },
	      {
	        name     : '<=',
	        value    : Models.ARITHMETIC_COMPARISON.less_than_or_equals_to,
	        selected : (arithmetic_operator == Models.ARITHMETIC_COMPARISON.less_than_or_equals_to)
	      }
	    ],
	    onChange: function(value, text, $selectedItem) {
	    	expression_arithmetic.operator = value;
	    }
	  })
	;

	element_to_append.append(menu_operator);
}

function renderLogicOperator (command, all_expression, expression_logic, logic_operator, function_obj, element_to_append, initial_el_to_render) {

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
	    	if ($selectedItem) {
		    	expression_logic.operator = value;
		    	if (expression_logic.second_operand == null) {
		    		expression_logic.second_operand = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);
		    		initial_el_to_render.empty();
		    		renderExpression(command, all_expression, function_obj, initial_el_to_render);
		    	}
	    	}
	    }
	  });

	element_to_append.append(menu_operator);

}


function renderLogicExpression (command, all_expression, expression_logic, function_obj, element_to_append, initial_el_to_render) {

	var exp_el_par_1 = $(' <span class="span_command_spec"> </span> ');
	var exp_el_expr_el_1 = $('<div class="expression_element"></div>');
	var exp_el_expr_operand = $('<div class="expression_element"></div>');
	var exp_el_expr_el_2 = $('<div class="expression_element"></div>');
	var exp_el_par_2 = $(' <span class="span_command_spec"> </span> ');

	if (expression_logic.first_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		renderLogicExpression(command, all_expression, expression_logic.first_operand, function_obj, exp_el_expr_el_1);
	} else if (expression_logic.first_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		renderArithmeticExpression(command, all_expression, expression_logic.first_operand, function_obj, exp_el_expr_el_1);
	} else {
		VariableValueMenuManagement.renderMenu(command, expression_logic.first_operand, exp_el_expr_el_1, function_obj);
	}

	element_to_append.append(exp_el_par_1);
	element_to_append.append(exp_el_expr_el_1);

	renderLogicOperator(command, all_expression, expression_logic, expression_logic.operator, function_obj, exp_el_expr_operand, initial_el_to_render);

	element_to_append.append(exp_el_expr_operand);

	if (expression_logic.second_operand) {
		if (expression_logic.second_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
			renderLogicExpression(command, all_expression, expression_logic.second_operand, function_obj, exp_el_expr_el_2);
		} else if (expression_logic.second_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
			renderArithmeticExpression(command, all_expression, expression_logic.second_operand, function_obj, exp_el_expr_el_2);
		} else {
			VariableValueMenuManagement.renderMenu(command, expression_logic.second_operand, exp_el_expr_el_2, function_obj);
		}

		element_to_append.append(exp_el_expr_el_2);
	}

	element_to_append.append(exp_el_par_2);

}

function renderArithmeticExpression (command, all_expression, expression_arithmetic, function_obj, element_to_append) {

	var exp_el_par_1 = $(' <span class="span_command_spec"> </span> ');
	var exp_el_expr_el_1 = $('<div class="expression_element"></div>');
	var exp_el_expr_operand = $('<div class="expression_element"></div>');
	var exp_el_expr_el_2 = $('<div class="expression_element"></div>');
	var exp_el_par_2 = $(' <span class="span_command_spec"> </span> ');


	if (expression_arithmetic.first_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		renderLogicExpression(command, all_expression, expression_arithmetic.first_operand, function_obj, exp_el_expr_el_1);
	} else if (expression_arithmetic.first_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		renderArithmeticExpression(command, all_expression, expression_arithmetic.first_operand, function_obj, exp_el_expr_el_1);
	} else {
		VariableValueMenuManagement.renderMenu(command, expression_arithmetic.first_operand, exp_el_expr_el_1, function_obj);
	}

	if (expression_arithmetic.second_operand.type == Models.EXPRESSION_TYPES.exp_logic) {
		renderLogicExpression(command, all_expression, expression_arithmetic.second_operand, function_obj, exp_el_expr_el_2);
	} else if (expression_arithmetic.second_operand.type == Models.EXPRESSION_TYPES.exp_arithmetic) {
		renderArithmeticExpression(command, all_expression, expression_arithmetic.second_operand, function_obj, exp_el_expr_el_2);
	} else {
		VariableValueMenuManagement.renderMenu(command, expression_arithmetic.second_operand, exp_el_expr_el_2, function_obj);
	}

	renderArithmeticOperator(command, all_expression, expression_arithmetic, expression_arithmetic.operator, function_obj, exp_el_expr_operand);

	element_to_append.append(exp_el_par_1);
	element_to_append.append(exp_el_expr_el_1);
	element_to_append.append(exp_el_expr_operand);
	element_to_append.append(exp_el_expr_el_2);
	element_to_append.append(exp_el_par_2);
}

function renderStartMenu (command, expression, function_obj, initial_el_to_render) {
	var start_menu = '';
	start_menu += '<div class="ui dropdown menu_start_rendered"><div class="text"><i>'+LocalizedStrings.getUI('expression_menu_select')+'</i></div><i class="dropdown icon"></i><div class="menu">';
	start_menu += '<div class="item" data-exp="'+Models.EXPRESSION_TYPES.exp_logic+'">'+LocalizedStrings.getUI('text_logic_expression')+' (EL == EL and EL)</div>';
	start_menu += '<div class="item" data-exp="'+Models.EXPRESSION_TYPES.exp_arithmetic+'">'+LocalizedStrings.getUI('text_arithmetic_expression')+' (EA < EA)</div>';
	start_menu += '</div></div>';
	start_menu = $(start_menu);

	start_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
			switch ($selectedItem.data('exp')) {
				case Models.EXPRESSION_TYPES.exp_logic:
					expression.expression = 
						new Models.LogicExpression(false, 
							new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true));
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
    	},
    	selectOnKeydown: false
	});

	initial_el_to_render.append(' <span class="span_command_spec"> </span> ');
	
	initial_el_to_render.append(start_menu);

	initial_el_to_render.append(' <span class="span_command_spec"> </span> ');
}
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';
import * as Utils from './utils';
import { registerUserEvent, registerSystemEvent, ActionTypes } from "./../services/userLog";
import { isValidIdentifier } from "./../util/utils";

var counter_new_variables = 0;

export function addVariable (function_obj, function_container, is_in_click = false) {
	var new_var = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI('new_variable') + '_' + counter_new_variables, 1);
	if (function_obj.variables_list == null) {
		function_obj.variables_list = [];
	}
	function_obj.variables_list.push(new_var);

	counter_new_variables ++;

	registerUserEvent(function_obj.name, ActionTypes.INSERT_FUNCTION_VAR, new_var.name, Types.INTEGER, 0);
	var newe = renderVariable(function_container, new_var, function_obj);

	if (is_in_click) {
		newe.css('display', 'none');
		newe.fadeIn();
	}
}

function updateName (variable_obj, new_name, variable_obj_dom, function_obj) {

	if (variable_obj.name == new_name) {
		return;
	}

	if (isValidIdentifier(new_name)) {
		if (variableNameAlreadyExists(new_name, function_obj)) {
			Utils.renderErrorMessage(variable_obj_dom.find('.editing_name_var'), LocalizedStrings.getUI('inform_valid_variable_duplicated'));
		} else {
			registerUserEvent(function_obj.name, ActionTypes.RENAME_FUNCTION_VAR, variable_obj.name, new_name);
			variable_obj.name = new_name;
		}
	} else {
		Utils.renderErrorMessage(variable_obj_dom.find('.editing_name_var'), LocalizedStrings.getUI('inform_valid_name'));
	}
}

function variableNameAlreadyExists (name_var, function_obj) {

	if (function_obj.parameters_list) {
		for (var i = 0; i < function_obj.parameters_list.length; i++) {
			if (function_obj.parameters_list[i].name == name_var) {
				return true;
			}
		}
	}

	if (function_obj.variables_list) {
		for (var i = 0; i < function_obj.variables_list.length; i++) {
			if (function_obj.variables_list[i].name == name_var) {
				return true;
			}
		}
	}

	return false;
}

function removeVariable (variable_obj, variable_container, function_name) {
	var function_associated = variable_container.data('associatedFunction');
	registerUserEvent(function_name, ActionTypes.REMOVE_FUNCTION_VAR, variable_obj.name);

	var index = function_associated.variables_list.indexOf(variable_obj);
	if (index > -1) {
	  window.insertContext = true;
	  delete function_associated.variables_list[index];
	  function_associated.variables_list.splice(index, 1);
	}
	variable_container.children().off();
	variable_container.off();
	variable_container.fadeOut();
}

function updateType (variable_obj, new_type, function_name, new_dimensions = 0) {
	variable_obj.type = new_type;		
	variable_obj.dimensions = new_dimensions;

	if (new_dimensions > 0) {
		variable_obj.rows = new_dimensions;
		variable_obj.columns = 2;
	}
	registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, variable_obj.name,
		new_type, new_dimensions, variable_obj.rows, variable_obj.columns);

	updateInitialValues(variable_obj, function_name);
}

function addHandlers (variable_obj, variable_container, function_obj) {

	// Manage variable name: 
	variable_container.find( ".enable_edit_name_variable" ).on('click', function(e){
		registerUserEvent(function_obj.name, ActionTypes.ENTER_CHANGE_VAR_NAME, variable_obj.name);
		enableNameUpdate(variable_obj, variable_container, function_obj);
	});

	// Menu to change type:
	variable_container.find('.ui.dropdown.variable_type').dropdown({
	    onChange: function(_, __, $selectedItem) {
	    	if ($selectedItem.data('dimensions')) {
	    		updateType(variable_obj, Types[$selectedItem.data('type')], function_obj.name, $selectedItem.data('dimensions'));
	    	} else {
	    		updateType(variable_obj, Types[$selectedItem.data('type')], function_obj.name);
	    	}

	    	renderValues(variable_obj, variable_container, function_obj.name);
	    },
	    selectOnKeydown: false
	});

	// Remove variable: 
	variable_container.find( ".remove_variable" ).on('click', function(e){
		removeVariable(variable_obj, variable_container, function_obj.name);
	});

}


export function renderVariable (function_container, new_var, function_obj) {

	var element = '<div class="ui label variable_container pink"><i class="ui icon ellipsis vertical inverted"></i>';

	element += '<div class="ui dropdown variable_type">';

  	if (new_var.dimensions > 0) {
  		element += '<div class="text">'+ LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(`type_${new_var.type.toLowerCase()}`);
  		for (var i = 0; i < new_var.dimensions; i ++) {
  			element += ' [ ] ';
  		}
  		element += '</div>';
  	} else {
  		element += '<div class="text">' + LocalizedStrings.getUI(`type_${new_var.type.toLowerCase()}`) + '</div>';
  	}
	element += '<div class="menu">';

	for (var tm in Types) {
  		if (tm == Types.VOID.toUpperCase()) {
  			continue;
  		}
  		element += '<div class="item ' + (new_var.type == tm.toLowerCase() ? ' selected ' : '') + '" data-type="'+tm+'" >'+LocalizedStrings.getUI(`type_${tm.toLowerCase()}`)+'</div>';
	}

  	for (var tm in Types) {
  		if (tm == Types.VOID.toUpperCase()) {
  			continue;
  		}
  		element += '<div class="item">'
  			+ '<i class="dropdown icon"></i>'
	    	+  LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(`type_${tm.toLowerCase()}`)
	      	+  '<div class="menu">'
		        + '<div class="item" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(`type_${tm.toLowerCase()}`)+' [ ] " data-type="'+tm+'" data-dimensions="1">[ ]</div>'
		        + '<div class="item" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(`type_${tm.toLowerCase()}`)+' [ ] [ ] " data-type="'+tm+'" data-dimensions="2">[ ] [ ] </div>'
	      	+  '</div>'
	    	+ '</div>';	
  	}

    element += '</div></div> ';

    element += '<div class="editing_name_var"><span class="span_name_variable enable_edit_name_variable">'+new_var.name+'</span> </div>';

	element += ' <span class="character_equals"> = </span> <div class="ui div_valor_var">'+new_var.value+'</div>';    

	element += ' <i class="yellow inverted icon times remove_variable"></i></div>';

	element = $(element);

	element.data('associatedFunction', function_obj);

	function_container.find('.variables_list_div').append(element);

	addHandlers(new_var, element, function_obj);

	renderValues(new_var, element, function_obj.name);

	return element;
}

function updateColumnsAndRowsText (variable_container, variable_var) {
	var prev = variable_container.find('.text').text().split('[');
	if (prev.length == 2) {
		var ff = prev[0] + '[ ' + variable_var.columns + ' ] ';
		variable_container.find('.text').empty();
		variable_container.find('.text').text(ff);
	}
	if (prev.length == 3) {
		var ff = prev[0] + '[ ' + variable_var.columns + ' ] [ ' + variable_var.rows + ' ] ';
		variable_container.find('.text').empty();
		variable_container.find('.text').text(ff);
	}
}

function renderValues (new_var, variable_container, function_name) {

	var ret = "";
	var j = 0;
		
	if (new_var.dimensions == 0) {
		if (new_var.type == Types.REAL) {
			ret += '<div class="created_div_valor_var"><span class="span_value_variable simple_var">'+new_var.value.toFixed(1)+'</span> </div> ';
		} else {
			if (new_var.type == Types.BOOLEAN) {
				ret += '<div class="created_div_valor_var"><span class="span_value_variable boolean_simple_type">'+LocalizedStrings.getUI(`logic_value_${new_var.value}`)+'</span> </div> ';
			} else {
				ret += '<div class="created_div_valor_var"><span class="span_value_variable simple_var">'+new_var.value+'</span> </div> ';
			}
		}
	} else {
		ret += '<table class="tabela_var">';

		if (new_var.dimensions == 1) {
			ret += '<tr>';
			if (new_var.type == Types.REAL) {
				for (var k = 0; k < new_var.columns; k++) {
					ret += '<td><span class="span_value_variable vector_var" data-index="'+k+'">'+new_var.value[k].toFixed(1)+'</span></td>';
				}
			} else {
				for (var k = 0; k < new_var.columns; k++) {
					if (new_var.type == Types.BOOLEAN) {
						ret += '<td><span class="span_value_variable boolean_vector_var" data-index="'+k+'">'+LocalizedStrings.getUI(`logic_value_${new_var.value[k]}`)+'</span></td>';
					} else {
						ret += '<td><span class="span_value_variable vector_var" data-index="'+k+'">'+new_var.value[k]+'</span>'+'</td>';
					}
				}
			}
			
			ret += '</tr>';
			ret += '</table>';

			ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline remove_global_vector_column"></i>'
		    	+ ' <i class="ui icon plus square outline add_global_vector_column"></i></div>';
		}

		if (new_var.dimensions == 2) {
			if (new_var.type == Types.REAL) {
				for (var l = 0; l < new_var.rows; l++) {
    				ret += '<tr>';
    				for (var k = 0; k < new_var.columns; k++) {
    					ret += '<td><span class="span_value_variable matrix_var" data-index="'+k+'" data-row="'+l+'">'+new_var.value[l][k].toFixed(1)+'</span>'+'</td>';
    				} 
    				ret += '</tr>';
				}
			} else {
				for (var l = 0; l < new_var.rows; l++) {
    				ret += '<tr>';
    				for (var k = 0; k < new_var.columns; k++) {
    					if (new_var.type == Types.BOOLEAN) { 
    						ret += '<td><span class="span_value_variable boolean_matrix_var" data-index="'+k+'" data-row="'+l+'">'+LocalizedStrings.getUI(`logic_value_${new_var.value[l][k]}`)+'</span></td>';
    					} else {
    						ret += '<td><span class="span_value_variable matrix_var" data-index="'+k+'" data-row="'+l+'">'+new_var.value[l][k]+'</span></td>';
    					}
    				} 
    				ret += '</tr>';
				}
			}
			if (new_var.rows == 0) {
				ret += '<tr><td></td></tr>';
			}
			ret += '<tr><td colspan="'+new_var.columns+'" class="tr_manage_lines"><i class="ui icon minus square outline remove_global_matrix_line"></i>'
		    	+ ' <i class="ui icon plus square outline add_global_matrix_line"></i></td></tr>';
			ret += '</table>';

			ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline remove_global_matrix_column"></i>'
		    	+ ' <i class="ui icon plus square outline add_global_matrix_column"></i></div>';
		}
		
	}

	$( variable_container ).find( ".div_valor_var" ).html('');

	ret = $(ret);

	$(ret).find('.span_value_variable').		data('associatedOject', new_var);

	$( ret ).find( ".boolean_simple_type" ).on('click', function(e){
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name, new_var.value);
		alternateBooleanValue(new_var, this.parentNode);
	});
	$( ret ).find( ".simple_var" ).on('click', function(e){
		registerUserEvent(function_name, ActionTypes.ENTER_CHANGE_VAR_VALUE, new_var.name);
		enableValueUpdate(new_var, this.parentNode, function_name);
	});

	$( ret ).find( ".boolean_vector_var" ).on('click', function(e){
		alternateBooleanVectorValue(new_var, $(this).data('index'), this.parentNode);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
	});
	$( ret ).find( ".vector_var" ).on('click', function(e){
		enableVectorValueUpdate(new_var, $(this).data('index'), this.parentNode, function_name);
	});
	$( ret ).find( ".remove_global_vector_column" ).on('click', function(e){
		removeColumnVector(new_var);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, new_var.name,
			new_var.type, new_var.dimensions, new_var.rows, new_var.columns);
		registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container, function_name);
	});
	$( ret ).find( ".add_global_vector_column" ).on('click', function(e){
		addColumnVector(new_var);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, new_var.name,
			new_var.type, new_var.dimensions, new_var.rows, new_var.columns);
		registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container, function_name);
	});
	$( ret ).find( ".remove_global_matrix_column" ).on('click', function(e){
		removeColumnMatrix(new_var);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, new_var.name,
			new_var.type, new_var.dimensions, new_var.rows, new_var.columns);
		registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container, function_name);
	});
	$( ret ).find( ".add_global_matrix_column" ).on('click', function(e){
		addColumnMatrix(new_var);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, new_var.name,
			new_var.type, new_var.dimensions, new_var.rows, new_var.columns);
		registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container, function_name);
	});
	$( ret ).find( ".remove_global_matrix_line" ).on('click', function(e){
		removeLineMatrix(new_var);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, new_var.name,
			new_var.type, new_var.dimensions, new_var.rows, new_var.columns);
		registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container, function_name);
	});
	$( ret ).find( ".add_global_matrix_line" ).on('click', function(e){
		addLineMatrix(new_var);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_TYPE, new_var.name,
			new_var.type, new_var.dimensions, new_var.rows, new_var.columns);
		registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container, function_name);
	});
	$( ret ).find( ".boolean_matrix_var" ).on('click', function(e){
		alternateBooleanMatrixValue(new_var, $(this).data('row'), $(this).data('index'), this.parentNode);
		registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, new_var.name,
			new_var.value);
	});
	$( ret ).find( ".matrix_var" ).on('click', function(e){
		registerUserEvent(function_name, ActionTypes.ENTER_CHANGE_VAR_VALUE, new_var.name);
		enableMatrixValueUpdate(new_var, $(this).data('row'), $(this).data('index'), this.parentNode, function_name);
	});
	$( variable_container ).find( ".div_valor_var" ).append(ret);

	updateColumnsAndRowsText(variable_container, new_var);

}

function alternateBooleanMatrixValue (var_obj, row, index, value_container) {
	var_obj.value[row][index] = !var_obj.value[row][index];
	$(value_container).find('.span_value_variable').text(LocalizedStrings.getUI(`logic_value_${var_obj.value[row][index]}`));
}

function addLineMatrix (var_obj) {
	var_obj.rows ++;

	if (var_obj.type == Types.INTEGER) {
		var n_l = [];
		for (var i = 0; i < var_obj.columns; i++) {
			n_l.push(1);
		}
		var_obj.value.push(n_l);
	}
	if (var_obj.type == Types.REAL) {
		var n_l = [];
		for (i = 0; i < var_obj.columns; i++) {
			n_l.push(1.0);
		}
		var_obj.value.push(n_l);
	}

	if (var_obj.type == Types.TEXT) {
		var n_l = [];
		for (i = 0; i < var_obj.columns; i++) {
			n_l.push(LocalizedStrings.getUI('textvar_default_value'));
		}
		var_obj.value.push(n_l);
	}

	if (var_obj.type == Types.BOOLEAN) {
		var n_l = [];
		for (i = 0; i < var_obj.columns; i++) {
			n_l.push(true);
		}
		var_obj.value.push(n_l);
	}
}

function removeLineMatrix (var_obj) {
	if (var_obj.rows == 0) {
		return;
	}

	var_obj.rows --;
	var_obj.value.splice(var_obj.value.length - 1, 1);
}

function addColumnMatrix (var_obj) {
	var_obj.columns ++;

	if (var_obj.type == Types.INTEGER) {
		for (var i = 0; i < var_obj.rows; i++) {
			var_obj.value[i].push(1);
		}
	}
	if (var_obj.type == Types.REAL) {
		for (var i = 0; i < var_obj.rows; i++) {
			var_obj.value[i].push(1.0);
		}
	}
	if (var_obj.type == Types.TEXT) {
		for (var i = 0; i < var_obj.rows; i++) {
			var_obj.value[i].push(LocalizedStrings.getUI('textvar_default_value'));
		}
	}
	if (var_obj.type == Types.BOOLEAN) {
		for (var i = 0; i < var_obj.rows; i++) {
			var_obj.value[i].push(true);
		}
	}
}

function removeColumnMatrix (var_obj) {
	if (var_obj.columns == 0) {
		return;
	}

	var_obj.columns --;

	for (var i = 0; i < var_obj.rows; i++) {
		var_obj.value[i].splice(var_obj.value[i].length - 1, 1);
	}
}

function addColumnVector (var_obj) {
	var_obj.columns ++;

	if (var_obj.type == Types.INTEGER) {
		var_obj.value.push(1);
	}
	if (var_obj.type == Types.REAL) {
		var_obj.value.push(1.0);
	}
	if (var_obj.type == Types.TEXT) {
		var_obj.value.push(LocalizedStrings.getUI('textvar_default_value'));
	}
	if (var_obj.type == Types.BOOLEAN) {
		var_obj.value.push(true);
	}
}

function removeColumnVector (var_obj) {
	if (var_obj.columns == 0) {
		return;
	}

	var_obj.columns --;
	var_obj.value.splice(var_obj.value.length - 1, 1);
}

function alternateBooleanValue (var_obj, value_container) {
	var_obj.value = !var_obj.value;
	$(value_container).find('.span_value_variable').text(LocalizedStrings.getUI(`logic_value_${var_obj.value}`));
}

function alternateBooleanVectorValue (var_obj, index, value_container) {
	var_obj.value[index] = !var_obj.value[index];
	$(value_container).find('.span_value_variable').text(LocalizedStrings.getUI(`logic_value_${var_obj.value[index]}`));
}

function updateInitialValues (variable_obj, function_name) {
	if (variable_obj.type == Types.INTEGER) {
		if (variable_obj.dimensions == 0) {
			variable_obj.value = 1;
		}
		if (variable_obj.dimensions == 1) {
			variable_obj.value = [1, 1];
		}
		if (variable_obj.dimensions == 2) {
			variable_obj.value = [[1, 1], [1, 1]];
		}
	}

	if (variable_obj.type == Types.REAL) {
		if (variable_obj.dimensions == 0) {
			variable_obj.value = 1.0;
		}
		if (variable_obj.dimensions == 1) {
			variable_obj.value = [1.0, 1.0];
		}
		if (variable_obj.dimensions == 2) {
			variable_obj.value = [[1.0, 1.0], [1.0, 1.0]];
		}
	}

	if (variable_obj.type == Types.TEXT) {
		if (variable_obj.dimensions == 0) {
			variable_obj.value = LocalizedStrings.getUI('textvar_default_value');
		}
		if (variable_obj.dimensions == 1) {
			variable_obj.value = [LocalizedStrings.getUI('textvar_default_value'), LocalizedStrings.getUI('textvar_default_value')];
		}
		if (variable_obj.dimensions == 2) {
			variable_obj.value = [[LocalizedStrings.getUI('textvar_default_value'), LocalizedStrings.getUI('textvar_default_value')], 
									[LocalizedStrings.getUI('textvar_default_value'), LocalizedStrings.getUI('textvar_default_value')]];
		}
	}

	if (variable_obj.type == Types.BOOLEAN) {
		if (variable_obj.dimensions == 0) {
			variable_obj.value = true;
		}
		if (variable_obj.dimensions == 1) {
			variable_obj.value = [true, true];
		}
		if (variable_obj.dimensions == 2) {
			variable_obj.value = [[true, true], [true, true]];
		}
	}
	registerSystemEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, variable_obj.name, variable_obj.value);
}

var opened_name_value_vector_global_ = false;
var opened_input_value_vector_global_ = null;
function enableVectorValueUpdate (var_obj, index, parent_node, function_name) {
	if (opened_name_value_vector_global_) {
		opened_input_value_vector_global_.focus();
		return;
	}
	parent_node = $(parent_node);
	opened_name_value_vector_global_ = true;

	parent_node.find('.span_value_variable').text('');

	var input_field;

	if (var_obj.type == Types.REAL) {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[index].toFixed(1) + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	} else {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[index] + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	}

	input_field.on('input', function() {
	    var inputWidth = input_field.textWidth()+10;
	    opened_input_value_vector_global_ = input_field;
	    input_field.focus();

	    var tmpStr = input_field.val();
		input_field.val('');
		input_field.val(tmpStr);

	    input_field.css({
	        width: inputWidth
	    })
	}).trigger('input');

	input_field.focusout(function() {
		let changed = false;
		/// update array:
		if (input_field.val().trim()) {
			if (var_obj.type == Types.REAL) {
				var_obj.value[index] = parseFloat(input_field.val().trim());

				parent_node.find('.span_value_variable').text(var_obj.value[index].toFixed(1));
			} else {

				if (var_obj.type == Types.INTEGER) {
					var_obj.value[index] = parseInt(input_field.val().trim());
				} else {
					var_obj.value[index] = input_field.val().trim();
				}

				parent_node.find('.span_value_variable').text(var_obj.value[index]);

			}
			changed = true;
		} else {
			if (var_obj.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(var_obj.value[index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(var_obj.value[index]);
			}
		}
		if (var_obj.type == Types.TEXT) {
			var_obj.value[index] = input_field.val();
			changed = true;
			parent_node.find('.span_value_variable').text(var_obj.value[index]);
		}
		if (changed) {
			registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, var_obj.name,
				var_obj.value);
		}
		input_field.off();
		input_field.remove();

		/// update elements:
		opened_name_value_vector_global_ = false;
		opened_input_value_vector_global_ = false;
	});

	input_field.on('keydown', function(e) {
		const code = e.keyCode || e.which;
		let changed = false;
		if(code == 13) {
			if (input_field.val().trim()) {
				if (var_obj.type == Types.REAL) {
					var_obj.value[index] = parseFloat(input_field.val().trim());

					parent_node.find('.span_value_variable').text(var_obj.value[index].toFixed(1));
				} else {

					if (var_obj.type == Types.INTEGER) {
						var_obj.value[index] = parseInt(input_field.val().trim());
					} else {
						var_obj.value[index] = input_field.val().trim();
					}

					parent_node.find('.span_value_variable').text(var_obj.value[index]);

				}
				changed = true;
			} else {
				if (var_obj.type == Types.REAL) {
					parent_node.find('.span_value_variable').text(var_obj.value[index].toFixed(1));
				} else {
					parent_node.find('.span_value_variable').text(var_obj.value[index]);
				}
			}
			if (var_obj.type == Types.TEXT) {
				var_obj.value[index] = input_field.val();
				changed = true;
				parent_node.find('.span_value_variable').text(var_obj.value[index]);
			}
			if (changed) {
				registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, var_obj.name,
					var_obj.value);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;
		}
		if(code == 27) {
			if (var_obj.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(var_obj.value[index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(var_obj.value[index]);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;
		}
	});

	input_field.select();
}

var opened_name_value_global_var = false;
var opened_input_value_global_ar = null;
function enableValueUpdate (var_obj, parent_node, function_name) {
	if (opened_name_value_global_var) {
		opened_input_value_global_ar.focus();
		return;
	}
	parent_node = $(parent_node);
	opened_name_value_global_var = true;

	var input_field;

	parent_node.find('.span_value_variable').text('');
	if (var_obj.type == Types.REAL) {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value.toFixed(1) + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	} else {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	}

	input_field.on('input', function() {
	    var inputWidth = input_field.textWidth()+10;
	    opened_input_value_global_ar = input_field;
	    input_field.focus();

	    var tmpStr = input_field.val();
		input_field.val('');
		input_field.val(tmpStr);

	    input_field.css({
	        width: inputWidth
	    })
	}).trigger('input');

	input_field.focusout(function() {
		/// update array:
		let changed = false;
		if (input_field.val().trim()) {
			if (var_obj.type == Types.REAL) {
				var_obj.value = parseFloat(input_field.val().trim());
				parent_node.find('.span_value_variable').text(var_obj.value.toFixed(1));
			} else{
				if (var_obj.type == Types.INTEGER) {
					var_obj.value = parseInt(input_field.val().trim());
				} else {
					var_obj.value = input_field.val().trim();
				}
				parent_node.find('.span_value_variable').text(var_obj.value);
				
			}
			changed = true;
		} else {
			if (var_obj.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(var_obj.value.toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(var_obj.value);
			}
		}
		if (var_obj.type == Types.TEXT) {
			var_obj.value = input_field.val();
			changed = true;
			parent_node.find('.span_value_variable').text(var_obj.value);
		}
		if (changed) {
			registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, var_obj.name,
				var_obj.value);
		}
		input_field.off();
		input_field.remove();

		/// update elements:
		opened_name_value_global_var = false;
		opened_input_value_global_ar = false;

	});

	input_field.on('keydown', function(e) {
		const code = e.keyCode || e.which;
		let changed = false;
		if(code == 13) {
			if (input_field.val().trim()) {
				if (var_obj.type == Types.REAL) {
					var_obj.value = parseFloat(input_field.val().trim());
					parent_node.find('.span_value_variable').text(var_obj.value.toFixed(1));
				} else{
					if (var_obj.type == Types.INTEGER) {
						var_obj.value = parseInt(input_field.val().trim());
					} else {
						var_obj.value = input_field.val().trim();
					}
					parent_node.find('.span_value_variable').text(var_obj.value);
				}
				changed = true;
			} else {
				if (var_obj.type == Types.REAL) {
					parent_node.find('.span_value_variable').text(var_obj.value.toFixed(1));
				} else {
					parent_node.find('.span_value_variable').text(var_obj.value);
				}
			}
			if (var_obj.type == Types.TEXT) {
				var_obj.value = input_field.val();
				changed = true;
				parent_node.find('.span_value_variable').text(var_obj.value);
			}
			if (changed) {
				registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, var_obj.name,
					var_obj.value);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;

		}
		if(code == 27) {
			if (var_obj.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(var_obj.value.toFixed(1));
			} else{
				parent_node.find('.span_value_variable').text(var_obj.value);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;
		}
	});

	input_field.select();
}

var opened_name_global = false;
var opened_input_global = null;
function enableNameUpdate (variable_obj, variable_container, function_obj) {

	if (opened_name_global) {
		opened_input_global.focus();
		return;
	}
	opened_name_global = true;

	variable_container.find('.span_name_variable').text('');

	var input_name;

	input_name = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+variable_obj.name+"' />" );
	input_name.insertBefore(variable_container.find('.span_name_variable'));

	input_name.on('input', function() {
	    var inputWidth = input_name.textWidth()+10;
	    opened_input_global = input_name;
	    input_name.focus();					

	    var tmpStr = input_name.val();
		input_name.val('');
		input_name.val(tmpStr);

	    input_name.css({
	        width: inputWidth
	    })
	}).trigger('input');

	input_name.focusout(function() {
		/// update array:
		if (input_name.val().trim().length > 0) {
			updateName(variable_obj, input_name.val().trim(), variable_container, function_obj);
			variable_container.find('.span_name_variable').text(variable_obj.name);
		} else {
			variable_container.find('.span_name_variable').text(variable_obj.name);
		}
		input_name.off();
		input_name.remove();

		/// update elements:
		opened_name_global = false;
		opened_input_global = false;
	});

	input_name.on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if (input_name.val().trim().length > 0) {
				updateName(variable_obj, input_name.val().trim(), variable_container, function_obj);
				variable_container.find('.span_name_variable').text(variable_obj.name);
			} else {
				variable_container.find('.span_name_variable').text(variable_obj.name);
			}
			input_name.off();
			input_name.remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
		if(code == 27) {

			variable_container.find('.span_name_variable').text(variable_obj.name);
			input_name.off();
			input_name.remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
	});

	input_name.select();
	
}


var opened_name_value_matrix_global_v = false;
var opened_input_value_matrix_global_v = null;
function enableMatrixValueUpdate (var_obj, row, index, parent_node, function_name) {
	if (opened_name_value_matrix_global_v) {
		opened_input_value_matrix_global_v.focus();
		return;
	}
	parent_node = $(parent_node);
	opened_name_value_matrix_global_v = true;

	parent_node.find('.span_value_variable').text('');

	var input_field;

	if (var_obj.type == Types.REAL) {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[row][index].toFixed(1) + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	} else {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[row][index] + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	}

	input_field.on('input', function() {
	    var inputWidth = input_field.textWidth()+10;
	    opened_input_value_matrix_global_v = input_field;
	    input_field.focus();

	    var tmpStr = input_field.val();
		input_field.val('');
		input_field.val(tmpStr);

	    input_field.css({
	        width: inputWidth
	    })
	}).trigger('input');

	input_field.focusout(function() {
		let changed = false;
		/// update array:
		if (input_field.val().trim()) {
			if (var_obj.type == Types.REAL) {
				var_obj.value[row][index] = parseFloat(input_field.val().trim());

				parent_node.find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
			} else {					
				if (var_obj.type == Types.INTEGER) {
					var_obj.value[row][index] = parseInt(input_field.val().trim());
				} else {
					var_obj.value[row][index] = input_field.val().trim();
				}
				parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
			}
			changed = true;
		} else {
			if (var_obj.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
			}
		}
		if (var_obj.type == Types.TEXT) {
			var_obj.value[row][index] = input_field.val();
			changed = true;
			parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
		}
		if (changed) {
			registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, var_obj.name,
				var_obj.value);
		}
		input_field.off();
		input_field.remove();

		/// update elements:
		opened_name_value_matrix_global_v = false;
		opened_input_value_matrix_global_v = false;
	});

	input_field.on('keydown', function(e) {
		const code = e.keyCode || e.which;
		let changed = false;
		if(code == 13) {
			if (input_field.val().trim()) {
				if (var_obj.type == Types.REAL) {
					var_obj.value[row][index] = parseFloat(input_field.val().trim());

					parent_node.find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
				} else {
					if (var_obj.type == Types.INTEGER) {
						var_obj.value[row][index] = parseInt(input_field.val().trim());
					} else {
						var_obj.value[row][index] = input_field.val().trim();
					}
					parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
				}
				changed = true;
			} else {
				if (var_obj.type == Types.REAL) {
					parent_node.find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
				} else {
					parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
				}
			}
			if (var_obj.type == Types.TEXT) {
				var_obj.value[row][index] = input_field.val();
				changed = true;
				parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
			}
			if (changed) {
				registerUserEvent(function_name, ActionTypes.CHANGE_VAR_VALUE, var_obj.name,
					var_obj.value);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;
		}
		if(code == 27) {
			if (var_obj.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(var_obj.value[row][index]);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;
		}
	});

	input_field.select();
}

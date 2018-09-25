import $ from 'jquery';
import jQuery from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';


window.jQuery = jQuery;

import '../semantic/semantic.min.js';

var counter_new_variables = 0;

export function addVariable (function_obj, function_container) {
	var new_var = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI('new_variable') + '_' + counter_new_variables, 1);
	if (function_obj.variables_list == null) {
		function_obj.variables_list = [];
	}
	function_obj.variables_list.push(new_var);

	counter_new_variables ++;

	renderVariable(function_container, new_var, function_obj);
}

function updateName (variable_obj, new_name) {
	variable_obj.name = new_name;
}

function removeVariable (variable_obj, variable_container) {
	var function_associated = $(variable_container).data('associatedFunction');

	var index = function_associated.variables_list.indexOf(variable_obj);
	if (index > -1) {
	  function_associated.variables_list.splice(index, 1);
	}
	$(variable_container).remove();
}

function updateType (variable_obj, new_type, new_dimensions = 0) {
	variable_obj.type = new_type;
	variable_obj.dimensions = new_dimensions;

	if (new_dimensions > 0) {
		variable_obj.rows = new_dimensions;
		variable_obj.columns = 2;
	}

	updateInitialValues(variable_obj);
}

function addHandlers (variable_obj, variable_container) {

	// Manage variable name: 
	$( variable_container ).find( ".enable_edit_name_variable" ).on('click', function(e){
		enableNameUpdate(variable_obj, variable_container);
	});

	// Menu to change type:
	$( variable_container ).find('.ui.dropdown.variable_type').dropdown({
	    onChange: function(value, text, $selectedItem) {
	    	if ($($selectedItem).data('dimensions')) {
	    		updateType(variable_obj, Types[$($selectedItem).data('type')], $($selectedItem).data('dimensions'));
	    	} else {
	    		updateType(variable_obj, Types[$($selectedItem).data('type')]);
	    	}

	    	renderValues(variable_obj, variable_container);
	    }
	});

	// Remove global: 
	$( variable_container ).find( ".remove_variable" ).on('click', function(e){
		removeVariable(variable_obj, variable_container);
	});

}


export function renderVariable(function_container, new_var, function_obj) {

	var element = '<div class="ui label variable_container">';

	element += '<span class="span_name_variable enable_edit_name_variable">'+new_var.name+'</span> <i class="icon small pencil alternate enable_edit_name_variable"></i>';
 	
 	element += '<div class="ui dropdown variable_type">';

  	if (new_var.dimensions > 0) {
  		element += '<div class="text">'+ LocalizedStrings.getUI('vector') + ':' + LocalizedStrings.getUI(new_var.type.toLowerCase());
  		for (var i = 0; i < new_var.dimensions; i ++) {
  			element += ' [ ] ';
  		}
  		element += '</div>';
  	} else {
  		element += '<div class="text">' + LocalizedStrings.getUI(new_var.type.toLowerCase()) + '</div>';
  	}
	element += '<i class="dropdown icon"></i><div class="menu">';

	for (var tm in Types) {
  		if (tm == Types.VOID.toUpperCase()) {
  			continue;
  		}
  		element += '<div class="item ' + (new_var.type == tm.toLowerCase() ? ' selected ' : '') + '" data-type="'+tm+'" >'+LocalizedStrings.getUI(tm.toLowerCase())+'</div>';
	}

  	for (var tm in Types) {
  		if (tm == Types.VOID.toUpperCase()) {
  			continue;
  		}
  		element += '<div class="item">'
	    	+ '<i class="dropdown icon"></i>'
	    	+  LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())
	      	+  '<div class="menu">'
		        + '<div class="item" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())+' [ ] " data-type="'+tm+'" data-dimensions="1">[ ]</div>'
		        + '<div class="item" data-text="'+ LocalizedStrings.getUI('vector')+':'+LocalizedStrings.getUI(tm.toLowerCase())+' [ ] [ ] " data-type="'+tm+'" data-dimensions="2">[ ] [ ] </div>'
	      	+  '</div>'
	    	+ '</div>';	
  	}

    element += '</div></div>  = ';

	element += '<div class="ui div_valor_var">'+new_var.value+'</div>';    

	element += ' <i class="red icon times remove_variable"></i></div>';

	element = $(element);

	$(element).data('associatedFunction', function_obj);

	$(function_container).find('.variables_list_div').append(element);

	addHandlers(new_var, element);

	renderValues(new_var, element);
}

function renderValues (new_var, variable_container) {

	var ret = "";
	var j = 0;

	if (new_var.dimensions == 0) {
		if (new_var.type == Types.REAL) {
			ret += '<div class="created_div_valor_var"><span class="span_value_variable simple_var">'+new_var.value.toFixed(1)+'</span> <i class="icon small pencil alternate enable_edit_name_function simple_var"></i></div> ';
		} else {
			if (new_var.type == Types.BOOLEAN) {
				ret += '<div class="created_div_valor_var"><span class="span_value_variable boolean_simple_type">'+new_var.value+'</span> <i class="icon small pencil alternate enable_edit_name_function boolean_simple_type"></i></div> ';
			} else {
				ret += '<div class="created_div_valor_var"><span class="span_value_variable simple_var">'+new_var.value+'</span> <i class="icon small pencil alternate enable_edit_name_function simple_var"></i></div> ';
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
						ret += '<td><span class="span_value_variable boolean_vector_var" data-index="'+k+'">'+new_var.value[k]+'</span></td>';
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
    						ret += '<td><span class="span_value_variable boolean_matrix_var" data-index="'+k+'" data-row="'+l+'">'+new_var.value[l][k]+'</span></td>';
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

	$(ret).find('.span_value_variable').data('associatedOject', new_var);

	$( ret ).find( ".boolean_simple_type" ).on('click', function(e){
		alternateBooleanValue(new_var, this.parentNode);
	});
	$( ret ).find( ".simple_var" ).on('click', function(e){
		enableValueUpdate(new_var, this.parentNode);
	});

	$( ret ).find( ".boolean_vector_var" ).on('click', function(e){
		alternateBooleanVectorValue(new_var, $(this).data('index'), this.parentNode);
	});
	$( ret ).find( ".vector_var" ).on('click', function(e){
		enableVectorValueUpdate(new_var, $(this).data('index'), this.parentNode);
	});
	$( ret ).find( ".remove_global_vector_column" ).on('click', function(e){
		removeColumnVector(new_var);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container);
	});
	$( ret ).find( ".add_global_vector_column" ).on('click', function(e){
		addColumnVector(new_var);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container);
	});
	$( ret ).find( ".remove_global_matrix_column" ).on('click', function(e){
		removeColumnMatrix(new_var);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container);
	});
	$( ret ).find( ".add_global_matrix_column" ).on('click', function(e){
		addColumnMatrix(new_var);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container);
	});
	$( ret ).find( ".remove_global_matrix_line" ).on('click', function(e){
		removeLineMatrix(new_var);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container);
	});
	$( ret ).find( ".add_global_matrix_line" ).on('click', function(e){
		addLineMatrix(new_var);
		$( variable_container ).find( ".div_valor_var" ).html('');
		renderValues(new_var, variable_container);
	});
	$( ret ).find( ".boolean_matrix_var" ).on('click', function(e){
		alternateBooleanMatrixValue(new_var, $(this).data('row'), $(this).data('index'), this.parentNode);
	});
	$( ret ).find( ".matrix_var" ).on('click', function(e){
		enableMatrixValueUpdate(new_var, $(this).data('row'), $(this).data('index'), this.parentNode);
	});
	$( variable_container ).find( ".div_valor_var" ).append(ret);

}

function alternateBooleanMatrixValue (var_obj, row, index, value_container) {
	var_obj.value[row][index] = !var_obj.value[row][index];
	$(value_container).find('.span_value_variable').text(var_obj.value[row][index]);
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
			n_l.push(LocalizedStrings.getUI('text_start'));
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
			var_obj.value[i].push(LocalizedStrings.getUI('text_start'));
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
		var_obj.value.push(LocalizedStrings.getUI('text_start'));
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
	$(value_container).find('.span_value_variable').text(var_obj.value);
}

function alternateBooleanVectorValue (var_obj, index, value_container) {
	var_obj.value[index] = !var_obj.value[index];
	$(value_container).find('.span_value_variable').text(var_obj.value[index]);
}

function updateInitialValues (variable_obj) {
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
			variable_obj.value = LocalizedStrings.getUI('text_start');
		}
		if (variable_obj.dimensions == 1) {
			variable_obj.value = [LocalizedStrings.getUI('text_start'), LocalizedStrings.getUI('text_start')];
		}
		if (variable_obj.dimensions == 2) {
			variable_obj.value = [[LocalizedStrings.getUI('text_start'), LocalizedStrings.getUI('text_start')], 
									[LocalizedStrings.getUI('text_start'), LocalizedStrings.getUI('text_start')]];
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
}

var opened_name_value_vector_global_ = false;
var opened_input_value_vector_global_ = null;
function enableVectorValueUpdate (var_obj, index, parent_node) {
	if (opened_name_value_vector_global_) {
		$(opened_input_value_vector_global_).focus();
		return;
	}
	opened_name_value_vector_global_ = true;

	$(parent_node).find('.span_value_variable').text('');

	if (var_obj.type == Types.REAL) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[index].toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[index] + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	}

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_value_vector_global_ = this;
	    $(this).focus();

	    var tmpStr = $(this).val();
		$(this).val('');
		$(this).val(tmpStr);

	    $(this).css({
	        width: inputWidth
	    })
	}).trigger('input');

	$('.width-dynamic').focusout(function() {
		/// update array:
		if ($(this).val().trim()) {
			if (var_obj.type == Types.REAL) {
				var_obj.value[index] = parseFloat($(this).val().trim());

				$(parent_node).find('.span_value_variable').text(var_obj.value[index].toFixed(1));
			} else {

				if (var_obj.type == Types.INTEGER) {
					var_obj.value[index] = parseInt($(this).val().trim());
				} else {
					var_obj.value[index] = $(this).val().trim();
				}

				$(parent_node).find('.span_value_variable').text(var_obj.value[index]);

			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_vector_global_ = false;
		opened_input_value_vector_global_ = false;
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (var_obj.type == Types.REAL) {
					var_obj.value[index] = parseFloat($(this).val().trim());

					$(parent_node).find('.span_value_variable').text(var_obj.value[index].toFixed(1));
				} else {

					if (var_obj.type == Types.INTEGER) {
						var_obj.value[index] = parseInt($(this).val().trim());
					} else {
						var_obj.value[index] = $(this).val().trim();
					}

					$(parent_node).find('.span_value_variable').text(var_obj.value[index]);

				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;
		}
		if(code == 27) {
			if (global_var.type == Types.REAL) {
				$(parent_node).find('.span_value_variable').text(var_obj.value[index].toFixed(1));
			} else {
				$(parent_node).find('.span_value_variable').text(var_obj.value[index]);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;
		}
	});
}

var opened_name_value_global_var = false;
var opened_input_value_global_ar = null;
function enableValueUpdate (var_obj, parent_node) {
	if (opened_name_value_global_var) {
		$(opened_input_value_global_ar).focus();
		return;
	}
	opened_name_value_global_var = true;

	$(parent_node).find('.span_value_variable').text('');
	if (var_obj.type == Types.REAL) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value.toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	}

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_value_global_ar = this;
	    $(this).focus();

	    var tmpStr = $(this).val();
		$(this).val('');
		$(this).val(tmpStr);

	    $(this).css({
	        width: inputWidth
	    })
	}).trigger('input');

	$('.width-dynamic').focusout(function() {
		/// update array:
		if ($(this).val().trim()) {
			if (var_obj.type == Types.REAL) {
				var_obj.value = parseFloat($(this).val().trim());
				$(parent_node).find('.span_value_variable').text(var_obj.value.toFixed(1));
			} else{
				if (var_obj.type == Types.INTEGER) {
					var_obj.value = parseInt($(this).val().trim());
				} else {
					var_obj.value = $(this).val().trim();
				}
				$(parent_node).find('.span_value_variable').text(var_obj.value);
				
			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_global_var = false;
		opened_input_value_global_ar = false;

	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (var_obj.type == Types.REAL) {
					var_obj.value = parseFloat($(this).val().trim());
					$(parent_node).find('.span_value_variable').text(var_obj.value.toFixed(1));
				} else{
					if (var_obj.type == Types.INTEGER) {
						var_obj.value = parseInt($(this).val().trim());
					} else {
						var_obj.value = $(this).val().trim();
					}
					$(parent_node).find('.span_value_variable').text(var_obj.value);
				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;

		}
		if(code == 27) {
			if (var_obj.type == Types.REAL) {
				$(parent_node).find('.span_value_variable').text(var_obj.value.toFixed(1));
			} else{
				$(parent_node).find('.span_value_variable').text(var_obj.value);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;
		}
	});
}

var opened_name_global = false;
var opened_input_global = null;
function enableNameUpdate (variable_obj, variable_container) {

	if (opened_name_global) {
		$(opened_input_global).focus();
		return;
	}
	opened_name_global = true;

	$( variable_container ).find('.span_name_variable').text('');
	$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+variable_obj.name+"' />" ).insertBefore($(variable_container).find('.span_name_variable'));

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_global = this;
	    $(this).focus();

	    var tmpStr = $(this).val();
		$(this).val('');
		$(this).val(tmpStr);

	    $(this).css({
	        width: inputWidth
	    })
	}).trigger('input');

	$('.width-dynamic').focusout(function() {
		/// update array:
		if ($(this).val().trim()) {
			updateName(variable_obj, $(this).val().trim());
			$(variable_container).find('.span_name_variable').text(variable_obj.name);
		}
		$(this).remove();

		/// update elements:
		opened_name_global = false;
		opened_input_global = false;
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				updateName(variable_obj, $(this).val().trim());
				$(variable_container).find('.span_name_variable').text(variable_obj.name);
			}
			$(this).remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
		if(code == 27) {

			$(variable_container).find('.span_name_variable').text(variable_obj.name);

			$(this).remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
	});
	
}


var opened_name_value_matrix_global_v = false;
var opened_input_value_matrix_global_v = null;
function enableMatrixValueUpdate (var_obj, row, index, parent_node) {
	if (opened_name_value_matrix_global_v) {
		$(opened_input_value_matrix_global_v).focus();
		return;
	}
	opened_name_value_matrix_global_v = true;

	$(parent_node).find('.span_value_variable').text('');

	if (var_obj.type == Types.REAL) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[row][index].toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ var_obj.value[row][index] + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	}

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_value_matrix_global_v = this;
	    $(this).focus();

	    var tmpStr = $(this).val();
		$(this).val('');
		$(this).val(tmpStr);

	    $(this).css({
	        width: inputWidth
	    })
	}).trigger('input');

	$('.width-dynamic').focusout(function() {
		/// update array:
		if ($(this).val().trim()) {
			if (var_obj.type == Types.REAL) {
				var_obj.value[row][index] = parseFloat($(this).val().trim());

				$(parent_node).find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
			} else {
				if (var_obj.type == Types.INTEGER) {
					var_obj.value[row][index] = parseInt($(this).val().trim());
				} else {
					var_obj.value[row][index] = $(this).val().trim();
				}
				$(parent_node).find('.span_value_variable').text(var_obj.value[row][index]);
			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_matrix_global_v = false;
		opened_input_value_matrix_global_v = false;
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (var_obj.type == Types.REAL) {
					var_obj.value[row][index] = parseFloat($(this).val().trim());

					$(parent_node).find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
				} else {
					if (var_obj.type == Types.INTEGER) {
						var_obj.value[row][index] = parseInt($(this).val().trim());
					} else {
						var_obj.value[row][index] = $(this).val().trim();
					}
					$(parent_node).find('.span_value_variable').text(var_obj.value[row][index]);
				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;
		}
		if(code == 27) {
			if (var_obj.type == Types.REAL) {
				$(parent_node).find('.span_value_variable').text(var_obj.value[row][index].toFixed(1));
			} else {
				$(parent_node).find('.span_value_variable').text(var_obj.value[row][index]);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;
		}
	});
}

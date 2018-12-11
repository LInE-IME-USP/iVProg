import $ from 'jquery';
import jQuery from 'jquery';
import { Types } from './types';
import * as Models from './ivprog_elements';
import { LocalizedStrings } from './../services/localizedStringsService';


window.jQuery = jQuery;

import '../semantic/semantic.min.js';

var counter_new_globals = 0;

export function addGlobal (program, is_from_click = false) {

	var new_global = new Models.Variable(Types.INTEGER, LocalizedStrings.getUI('new_global') + '_' + counter_new_globals, 1);
	counter_new_globals ++;

	program.addGlobal(new_global);

	var newe = renderGlobal(new_global);

	if (is_from_click) {
		newe.css('display', 'none');
		newe.fadeIn();
	}

}

function toggleConstant (global_var) {
	global_var.is_constant = !global_var.is_constant;
}

function updateName (global_var, new_name) {
	global_var.name = new_name;
}

function updateType (global_var, new_type, new_dimensions = 0) {
	global_var.type = new_type;
	global_var.dimensions = new_dimensions;

	if (new_dimensions > 0) {
		global_var.rows = new_dimensions;
		global_var.columns = 2;
	}

	updateInitialValues(global_var);
}

function removeGlobal (global_var, global_container) {
	var index = window.program_obj.globals.indexOf(global_var);
	if (index > -1) {
	  window.program_obj.globals.splice(index, 1);
	}
	global_container.children().off();
	global_container.off();
	global_container.remove();
}

function updateInitialValues (global_var) {
	if (global_var.type == Types.INTEGER) {
		if (global_var.dimensions == 0) {
			global_var.value = 1;
		}
		if (global_var.dimensions == 1) {
			global_var.value = [1, 1];
		}
		if (global_var.dimensions == 2) {
			global_var.value = [[1, 1], [1, 1]];
		}
	}

	if (global_var.type == Types.REAL) {
		if (global_var.dimensions == 0) {
			global_var.value = 1.0;
		}
		if (global_var.dimensions == 1) {
			global_var.value = [1.0, 1.0];
		}
		if (global_var.dimensions == 2) {
			global_var.value = [[1.0, 1.0], [1.0, 1.0]];
		}
	}

	if (global_var.type == Types.TEXT) {
		if (global_var.dimensions == 0) {
			global_var.value = LocalizedStrings.getUI('text_start');
		}
		if (global_var.dimensions == 1) {
			global_var.value = [LocalizedStrings.getUI('text_start'), LocalizedStrings.getUI('text_start')];
		}
		if (global_var.dimensions == 2) {
			global_var.value = [[LocalizedStrings.getUI('text_start'), LocalizedStrings.getUI('text_start')], 
									[LocalizedStrings.getUI('text_start'), LocalizedStrings.getUI('text_start')]];
		}
	}

	if (global_var.type == Types.BOOLEAN) {
		if (global_var.dimensions == 0) {
			global_var.value = true;
		}
		if (global_var.dimensions == 1) {
			global_var.value = [true, true];
		}
		if (global_var.dimensions == 2) {
			global_var.value = [[true, true], [true, true]];
		}
	}
}

function alternateBooleanGlobalValue (global_var, value_container) {
	global_var.value = !global_var.value;
	$(value_container).find('.span_value_variable').text(LocalizedStrings.getUI(global_var.value));
}

function alternateBooleanGlobalVectorValue (global_var, index, value_container) {
	global_var.value[index] = !global_var.value[index];
	$(value_container).find('.span_value_variable').text(LocalizedStrings.getUI(global_var.value[index]));
}

function removeGlobalColumnVector (global_var) {
	if (global_var.columns == 0) {
		return;
	}

	global_var.columns --;
	global_var.value.splice(global_var.value.length - 1, 1);
}

function addGlobalColumnVector (global_var) {
	global_var.columns ++;

	if (global_var.type == Types.INTEGER) {
		global_var.value.push(1);
	}
	if (global_var.type == Types.REAL) {
		global_var.value.push(1.0);
	}
	if (global_var.type == Types.TEXT) {
		global_var.value.push(LocalizedStrings.getUI('text_start'));
	}
	if (global_var.type == Types.BOOLEAN) {
		global_var.value.push(true);
	}
}

function removeColumnGlobalMatrix (global_var) {
	if (global_var.columns == 0) {
		return;
	}

	global_var.columns --;

	for (var i = 0; i < global_var.rows; i++) {
		global_var.value[i].splice(global_var.value[i].length - 1, 1);
	}
}

function addColumnGlobalMatrix (global_var) {
	global_var.columns ++;

	if (global_var.type == Types.INTEGER) {
		for (var i = 0; i < global_var.rows; i++) {
			global_var.value[i].push(1);
		}
	}
	if (global_var.type == Types.REAL) {
		for (var i = 0; i < global_var.rows; i++) {
			global_var.value[i].push(1.0);
		}
	}
	if (global_var.type == Types.TEXT) {
		for (var i = 0; i < global_var.rows; i++) {
			global_var.value[i].push(LocalizedStrings.getUI('text_start'));
		}
	}
	if (global_var.type == Types.BOOLEAN) {
		for (var i = 0; i < global_var.rows; i++) {
			global_var.value[i].push(true);
		}
	}
}

function removeLineGlobalMatrix (global_var) {
	if (global_var.rows == 0) {
		return;
	}

	global_var.rows --;
	global_var.value.splice(global_var.value.length - 1, 1);
}

function addLineGlobalMatrix (global_var) {
	global_var.rows ++;

	if (global_var.type == Types.INTEGER) {
		var n_l = [];
		for (var i = 0; i < global_var.columns; i++) {
			n_l.push(1);
		}
		global_var.value.push(n_l);
	}
	if (global_var.type == Types.REAL) {
		var n_l = [];
		for (i = 0; i < global_var.columns; i++) {
			n_l.push(1.0);
		}
		global_var.value.push(n_l);
	}

	if (global_var.type == Types.TEXT) {
		var n_l = [];
		for (i = 0; i < global_var.columns; i++) {
			n_l.push(LocalizedStrings.getUI('text_start'));
		}
		global_var.value.push(n_l);
	}

	if (global_var.type == Types.BOOLEAN) {
		var n_l = [];
		for (i = 0; i < global_var.columns; i++) {
			n_l.push(true);
		}
		global_var.value.push(n_l);
	}
}

function alternateBooleanGlobalMatrixValue (global_var, row, index, value_container) {
	global_var.value[row][index] = !global_var.value[row][index];
	$(value_container).find('.span_value_variable').text(LocalizedStrings.getUI(global_var.value[row][index]));
}

function renderValues (global_var, global_container) {

	var ret = "";
	var j = 0;

	if (global_var.dimensions == 0) {
		if (global_var.type == Types.REAL) {
			ret += '<div class="created_div_valor_var"><span class="span_value_variable simple_var">'+global_var.value.toFixed(1)+'</span>  </div> ';
		} else {
			if (global_var.type == Types.BOOLEAN) {
				ret += '<div class="created_div_valor_var"><span class="span_value_variable boolean_simple_type">'+LocalizedStrings.getUI(global_var.value)+'</span>  </div> ';
			} else {
				ret += '<div class="created_div_valor_var"><span class="span_value_variable simple_var">'+global_var.value+'</span>  </div> ';
			}
		}
	} else {
		ret += '<table class="tabela_var">';

		if (global_var.dimensions == 1) {
			ret += '<tr>';
			if (global_var.type == Types.REAL) {
				for (var k = 0; k < global_var.columns; k++) {
					ret += '<td><span class="span_value_variable vector_var" data-index="'+k+'">'+global_var.value[k].toFixed(1)+'</span></td>';
				}
			} else {
				for (var k = 0; k < global_var.columns; k++) {
					if (global_var.type == Types.BOOLEAN) {
						ret += '<td><span class="span_value_variable boolean_vector_var" data-index="'+k+'">'+LocalizedStrings.getUI(global_var.value[k])+'</span></td>';
					} else {
						ret += '<td><span class="span_value_variable vector_var" data-index="'+k+'">'+global_var.value[k]+'</span>'+'</td>';
					}
				}
			}
			
			ret += '</tr>';
			ret += '</table>';

			ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline remove_global_vector_column"></i>'
		    	+ ' <i class="ui icon plus square outline add_global_vector_column"></i></div>';
		}

		if (global_var.dimensions == 2) {
			if (global_var.type == Types.REAL) {
				for (var l = 0; l < global_var.rows; l++) {
    				ret += '<tr>';
    				for (var k = 0; k < global_var.columns; k++) {
    					ret += '<td><span class="span_value_variable matrix_var" data-index="'+k+'" data-row="'+l+'">'+global_var.value[l][k].toFixed(1)+'</span>'+'</td>';
    				} 
    				ret += '</tr>';
				}
			} else {
				for (var l = 0; l < global_var.rows; l++) {
    				ret += '<tr>';
    				for (var k = 0; k < global_var.columns; k++) {
    					if (global_var.type == Types.BOOLEAN) { 
    						ret += '<td><span class="span_value_variable boolean_matrix_var" data-index="'+k+'" data-row="'+l+'">'+LocalizedStrings.getUI(global_var.value[l][k])+'</span></td>';
    					} else {
    						ret += '<td><span class="span_value_variable matrix_var" data-index="'+k+'" data-row="'+l+'">'+global_var.value[l][k]+'</span></td>';
    					}
    				} 
    				ret += '</tr>';
				}
			}
			if (global_var.rows == 0) {
				ret += '<tr><td></td></tr>';
			}
			ret += '<tr><td colspan="'+global_var.columns+'" class="tr_manage_lines"><i class="ui icon minus square outline remove_global_matrix_line"></i>'
		    	+ ' <i class="ui icon plus square outline add_global_matrix_line"></i></td></tr>';
			ret += '</table>';

			ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline remove_global_matrix_column"></i>'
		    	+ ' <i class="ui icon plus square outline add_global_matrix_column"></i></div>';
		}
		
	}

	global_container.find( ".div_valor_var" ).html('');

	ret = $(ret);

	ret.find('.span_value_variable').data('associatedOject', global_var);

	ret.find( ".boolean_simple_type" ).on('click', function(e){
		alternateBooleanGlobalValue(global_var, this.parentNode);
	});
	ret.find( ".simple_var" ).on('click', function(e){
		enableGlobalValueUpdate(global_var, this.parentNode);
	});

	ret.find( ".boolean_vector_var" ).on('click', function(e){
		alternateBooleanGlobalVectorValue(global_var, $(this).data('index'), this.parentNode);
	});
	ret.find( ".vector_var" ).on('click', function(e){
		enableGlobalVectorValueUpdate(global_var, $(this).data('index'), this.parentNode);
	});
	ret.find( ".remove_global_vector_column" ).on('click', function(e){
		removeGlobalColumnVector(global_var);
		global_container.find( ".div_valor_var" ).html('');
		renderValues(global_var, global_container);
	});
	ret.find( ".add_global_vector_column" ).on('click', function(e){
		addGlobalColumnVector(global_var);
		global_container.find( ".div_valor_var" ).html('');
		renderValues(global_var, global_container);
	});
	ret.find( ".remove_global_matrix_column" ).on('click', function(e){
		removeColumnGlobalMatrix(global_var);
		global_container.find( ".div_valor_var" ).html('');
		renderValues(global_var, global_container);
	});
	ret.find( ".add_global_matrix_column" ).on('click', function(e){
		addColumnGlobalMatrix(global_var);
		global_container.find( ".div_valor_var" ).html('');
		renderValues(global_var, global_container);
	});
	ret.find( ".remove_global_matrix_line" ).on('click', function(e){
		removeLineGlobalMatrix(global_var);
		global_container.find( ".div_valor_var" ).html('');
		renderValues(global_var, global_container);
	});
	ret.find( ".add_global_matrix_line" ).on('click', function(e){
		addLineGlobalMatrix(global_var);
		global_container.find( ".div_valor_var" ).html('');
		renderValues(global_var, global_container);
	});
	ret.find( ".boolean_matrix_var" ).on('click', function(e){
		alternateBooleanGlobalMatrixValue(global_var, $(this).data('row'), $(this).data('index'), this.parentNode);
	});
	ret.find( ".matrix_var" ).on('click', function(e){
		enableGlobalMatrixValueUpdate(global_var, $(this).data('row'), $(this).data('index'), this.parentNode);
	});
	global_container.find( ".div_valor_var" ).append(ret);


	updateColumnsAndRowsText(global_container, global_var);

}

function addHandlers (global_container) {
	var global_var = global_container.data('associatedOject'); 
	// Manage constant option:
	global_container.find( ".alternate_constant" ).on('click', function(e){
		toggleConstant(global_var);

		$( this ).removeClass( "on off" );
		if (global_var.is_constant) {
			$( this ).addClass( "on" );
		} else {
			$( this ).addClass( "off" );
		}
	});

	// Manage global name: 
	global_container.find( ".enable_edit_name_parameter" ).on('click', function(e){
		enableNameUpdate(global_container);
	});

	// Menu to change type:
	global_container.find('.ui.dropdown.global_type').dropdown({
	    onChange: function(value, text, $selectedItem) {
	    	if ($selectedItem.data('dimensions')) {
	    		updateType(global_var, Types[$selectedItem.data('type')], $selectedItem.data('dimensions'));
	    	} else {
	    		updateType(global_var, Types[$selectedItem.data('type')]);
	    	}

	    	renderValues(global_var, global_container);

	    }
	});

	// Remove global: 
	global_container.find( ".remove_global" ).on('click', function(e){
		removeGlobal(global_var, global_container);
	});

}

function updateColumnsAndRowsText (global_container, global_var) {
	var prev = global_container.find('.text').text().split('[');
	if (prev.length == 2) {
		var ff = prev[0] + '[ ' + global_var.columns + ' ] ';
		global_container.find('.text').empty();
		global_container.find('.text').text(ff);
	}
	if (prev.length == 3) {
		var ff = prev[0] + '[ ' + global_var.columns + ' ] [ ' + global_var.rows + ' ] ';
		global_container.find('.text').empty();
		global_container.find('.text').text(ff);
	}
}

export function renderGlobal (global_var) {

	var element = '<div class="ui label global_container pink"><div class="global_const">const: ';

	element += '<i class="ui icon toggle '+(global_var.is_constant?"on":"off")+' alternate_constant"></i></div>';
 	
 	element += '<div class="ui dropdown global_type">';

  	if (global_var.dimensions > 0) {
  		element += '<div class="text">'+ LocalizedStrings.getUI('vector')+ ':' + LocalizedStrings.getUI(global_var.type);
  		for (var i = 0; i < global_var.dimensions; i ++) {
  			element += ' [ <span class="dimensions_'+i+'"></span> ] ';
  		}
  		element += '</div>';
  	} else {
  		element += '<div class="text">' + LocalizedStrings.getUI(global_var.type.toLowerCase()) + '</div>';
  	}
	element += '<div class="menu">';

	for (var tm in Types) {
  		if (tm == Types.VOID.toUpperCase()) {
  			continue;
  		}
  		element += '<div class="item ' + (global_var.type == tm.toLowerCase() ? ' selected ' : '') + '" data-type="'+tm+'" >'+LocalizedStrings.getUI(tm.toLowerCase())+'</div>';
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

    element += '</div></div> <div class="editing_name_var"> <span class="span_name_variable enable_edit_name_parameter">'+global_var.name+'</span> </div> <span class="character_equals"> = </span> ';

	element += '<div class="ui div_valor_var">'+global_var.value+'</div>';    

	element += ' <i class="yellow inverted icon times remove_global"></i></div>';

	var complete_element = $(element);

	complete_element.data('associatedOject', global_var);

	$('.list_globals').append(complete_element);

	addHandlers(complete_element);

	renderValues(global_var, complete_element);

	if (global_var.dimensions == 1) {
		complete_element.find('.dimensions_0').text(global_var.columns);
	}
	if (global_var.dimensions == 2) {
		complete_element.find('.dimensions_0').text(global_var.columns);
		complete_element.find('.dimensions_1').text(global_var.rows);
	}

	return complete_element;
}

var opened_name_value_matrix_global_v = false;
var opened_input_value_matrix_global_v = null;
function enableGlobalMatrixValueUpdate (global_var, row, index, parent_node) {
	if (opened_name_value_matrix_global_v) {
		opened_input_value_matrix_global_v.focus();
		return;
	}
	parent_node = $(parent_node);
	opened_name_value_matrix_global_v = true;

	parent_node.find('.span_value_variable').text('');

	var input_field;

	if (global_var.type == Types.REAL) {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ global_var.value[row][index].toFixed(1) + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	} else {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ global_var.value[row][index] + "' />" );
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
		/// update array:
		if (input_field.val().trim()) {
			if (global_var.type == Types.REAL) {
				global_var.value[row][index] = parseFloat(input_field.val().trim());

				parent_node.find('.span_value_variable').text(global_var.value[row][index].toFixed(1));
			} else {
				if (global_var.type == Types.INTEGER) {
					global_var.value[row][index] = parseInt(input_field.val().trim());
				} else {
					global_var.value[row][index] = input_field.val().trim();
				}
				parent_node.find('.span_value_variable').text(global_var.value[row][index]);
			}
		} else {
			if (global_var.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(global_var.value[row][index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(global_var.value[row][index]);
			}
		}
		if (global_var.type == Types.TEXT) {
			global_var.value[row][index] = input_field.val();
			parent_node.find('.span_value_variable').text(global_var.value[row][index]);
		}
		input_field.off();
		input_field.remove();

		/// update elements:
		opened_name_value_matrix_global_v = false;
		opened_input_value_matrix_global_v = false;
	});

	input_field.on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if (input_field.val().trim()) {
				if (global_var.type == Types.REAL) {
					global_var.value[row][index] = parseFloat(input_field.val().trim());

					parent_node.find('.span_value_variable').text(global_var.value[row][index].toFixed(1));
				} else {
					if (global_var.type == Types.INTEGER) {
						global_var.value[row][index] = parseInt(input_field.val().trim());
					} else {
						global_var.value[row][index] = input_field.val().trim();
					}
					parent_node.find('.span_value_variable').text(global_var.value[row][index]);
				}
			} else {
				if (global_var.type == Types.REAL) {
					parent_node.find('.span_value_variable').text(global_var.value[row][index].toFixed(1));
				} else {
					parent_node.find('.span_value_variable').text(global_var.value[row][index]);
				}
			}
			if (global_var.type == Types.TEXT) {
				global_var.value[row][index] = input_field.val();
				parent_node.find('.span_value_variable').text(global_var.value[row][index]);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;
		}
		if(code == 27) {
			if (global_var.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(global_var.value[row][index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(global_var.value[row][index]);
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

var opened_name_value_global_var = false;
var opened_input_value_global_ar = null;
function enableGlobalValueUpdate (global_var, parent_node) {
	if (opened_name_value_global_var) {
		opened_input_value_global_ar.focus();
		return;
	}
	parent_node = $(parent_node);
	opened_name_value_global_var = true;

	parent_node.find('.span_value_variable').text('');

	var input_field;

	if (global_var.type == Types.REAL) {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ global_var.value.toFixed(1) + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	} else {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ global_var.value + "' />" );
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
		if (input_field.val().trim()) {
			if (global_var.type == Types.REAL) {
				global_var.value = parseFloat(input_field.val().trim());
				parent_node.find('.span_value_variable').text(global_var.value.toFixed(1));
			} else{
				if (global_var.type == Types.INTEGER) {
					global_var.value = parseInt(input_field.val().trim());
				} else {
					global_var.value = input_field.val().trim();
				}
				parent_node.find('.span_value_variable').text(global_var.value);
				
			}
		} else {
			if (global_var.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(global_var.value.toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(global_var.value);
			}
		}
		if (global_var.type == Types.TEXT) {
			global_var.value = input_field.val();
			parent_node.find('.span_value_variable').text(global_var.value);
		}
		input_field.off();
		input_field.remove();

		/// update elements:
		opened_name_value_global_var = false;
		opened_input_value_global_ar = false;

	});

	input_field.on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if (input_field.val().trim()) {
				if (global_var.type == Types.REAL) {
					global_var.value = parseFloat(input_field.val().trim());
					parent_node.find('.span_value_variable').text(global_var.value.toFixed(1));
				} else {
					if (global_var.type == Types.INTEGER) {
						global_var.value = parseInt(input_field.val().trim());
					} else {
						global_var.value = input_field.val().trim();
					}
					parent_node.find('.span_value_variable').text(global_var.value);
				}
			} else {
				if (global_var.type == Types.REAL) {
					parent_node.find('.span_value_variable').text(global_var.value.toFixed(1));
				} else {
					parent_node.find('.span_value_variable').text(global_var.value);
				}
			}
			if (global_var.type == Types.TEXT) {
				global_var.value = input_field.val();
				parent_node.find('.span_value_variable').text(global_var.value);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;

		}
		if(code == 27) {
			if (global_var.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(global_var.value.toFixed(1));
			} else{
				parent_node.find('.span_value_variable').text(global_var.value);
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
function enableNameUpdate (global_container) {

	var global_var = global_container.data('associatedOject'); 

	if (opened_name_global) {
		opened_input_global.focus();
		return;
	}
	opened_name_global = true;

	global_container.find('.span_name_variable').text('');
	var input_name = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+global_var.name+"' />" );
	input_name.insertBefore(global_container.find('.span_name_variable'));

	input_name.on('input', function() {
	    var inputWidth = input_name.textWidth()+10;
	    opened_input_global = input_name;
	    opened_input_global.focus();

	    opened_input_global.css({
	        width: inputWidth
	    })
	}).trigger('input');

	input_name.focusout(function() {
		/// update array:
		if (input_name.val().trim().length > 0) {
			updateName(global_var, input_name.val().trim());
			global_container.find('.span_name_variable').text(global_var.name);
		} else {
			global_container.find('.span_name_variable').text(global_var.name);
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
			if (input_name.val().trim()) {
				updateName(global_var, input_name.val().trim());
				global_container.find('.span_name_variable').text(global_var.name);
			} else {
				global_container.find('.span_name_variable').text(global_var.name);
			}
			input_name.off();
			input_name.remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
		if(code == 27) {

			global_container.find('.span_name_variable').text(global_var.name);
			input_name.off();
			input_name.remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
	});

	input_name.select();
	
}


var opened_name_value_vector_global_ = false;
var opened_input_value_vector_global_ = null;
function enableGlobalVectorValueUpdate (global_var, index, parent_node) {
	if (opened_name_value_vector_global_) {
		opened_input_value_vector_global_.focus();
		return;
	}
	parent_node = $(parent_node);
	opened_name_value_vector_global_ = true;

	parent_node.find('.span_value_variable').text('');

	var input_field;

	if (global_var.type == Types.REAL) {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ global_var.value[index].toFixed(1) + "' />" );
		input_field.insertBefore(parent_node.find('.span_value_variable'));
	} else {
		input_field = $( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ global_var.value[index] + "' />" );
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
		/// update array:
		if (input_field.val().trim()) {
			if (global_var.type == Types.REAL) {
				global_var.value[index] = parseFloat(input_field.val().trim());

				parent_node.find('.span_value_variable').text(global_var.value[index].toFixed(1));
			} else {

				if (global_var.type == Types.INTEGER) {
					global_var.value[index] = parseInt(input_field.val().trim());
				} else {
					global_var.value[index] = input_field.val().trim();
				}

				parent_node.find('.span_value_variable').text(global_var.value[index]);

			}
		} else {
			if (global_var.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(global_var.value[index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(global_var.value[index]);
			}
		}
		if (global_var.type == Types.TEXT) {
			global_var.value[index] = input_field.val();
			parent_node.find('.span_value_variable').text(global_var.value[index]);
		}
		input_field.off();
		input_field.remove();

		/// update elements:
		opened_name_value_vector_global_ = false;
		opened_input_value_vector_global_ = false;
	});

	input_field.on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if (input_field.val().trim()) {
				if (global_var.type == Types.REAL) {
					global_var.value[index] = parseFloat(input_field.val().trim());

					parent_node.find('.span_value_variable').text(global_var.value[index].toFixed(1));
				} else {

					if (global_var.type == Types.INTEGER) {
						global_var.value[index] = parseInt(input_field.val().trim());
					} else {
						global_var.value[index] = input_field.val().trim();
					}

					parent_node.find('.span_value_variable').text(global_var.value[index]);

				}
			} else {
				if (global_var.type == Types.REAL) {
					parent_node.find('.span_value_variable').text(global_var.value[index].toFixed(1));
				} else {
					parent_node.find('.span_value_variable').text(global_var.value[index]);
				}
			}
			if (global_var.type == Types.TEXT) {
				global_var.value[index] = input_field.val();
				parent_node.find('.span_value_variable').text(global_var.value[index]);
			}
			input_field.off();
			input_field.remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;
		}
		if(code == 27) {
			if (global_var.type == Types.REAL) {
				parent_node.find('.span_value_variable').text(global_var.value[index].toFixed(1));
			} else {
				parent_node.find('.span_value_variable').text(global_var.value[index]);
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


$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};
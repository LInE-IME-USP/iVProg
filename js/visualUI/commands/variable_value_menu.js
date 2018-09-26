import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as AttribuitionsManagement from './attribution';
import * as WritersManagement from './writer';

export const VAR_OR_VALUE_TYPES = Object.freeze({only_variable: 1, only_value: 2, only_function: 3, variable_and_function: 4, variable_and_value_opt: 5,
	value_and_function: 6, all: 7});

export function renderMenu (command, ref_object, dom_object, function_obj, size_field = 2) {
	var menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';

	if (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_function) {

		menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom"><div class="text"></div><i class="dropdown icon"></i><div class="menu menu_only_functions">';
		menu_var_or_value += '</div>';
	} 

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_function) 
		|| (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.value_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {
		
		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_function+'"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('btn_function');
		menu_var_or_value += '<div class="menu menu_only_functions">';
		menu_var_or_value += '</div></div>';
	}
	
	if (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_variable) {

		menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom"><div class="text"></div><i class="dropdown icon"></i><div class="menu menu_only_vars">';
		menu_var_or_value += '</div>';
	} 

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_value_opt) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {
		
		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('variable');
		menu_var_or_value += '<div class="menu menu_only_vars">';
		menu_var_or_value += '</div></div>';
	}

	if (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_value) {
		menu_var_or_value = '<input type="text" class="width-dynamic" size="'+size_field+'" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" />';
	}

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_value_opt) 
		|| (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.value_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {

		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_value+'">'+LocalizedStrings.getUI('text_value')+'</div>';
	}

    menu_var_or_value += '</div></div>';

    menu_var_or_value = $(menu_var_or_value);

    dom_object.append(menu_var_or_value);

    addHandlers(command, ref_object, dom_object, menu_var_or_value, function_obj);

    addVariablesToMenu(function_obj, menu_var_or_value, ref_object);

    addFunctionsToMenu(function_obj, menu_var_or_value, ref_object);

    if (ref_object.content || ref_object.function_called) {
    	renderPreviousContent(function_obj, menu_var_or_value, ref_object, dom_object, command);
    }
}

function renderPreviousContent (function_obj, menu_var_or_value, ref_object, dom_object, command) {

	
	if (ref_object.function_called) {

		console.log("P1 chamando para: ");
		console.log(ref_object);

		menu_var_or_value.remove();
		variableValueMenuCode(command, ref_object, dom_object, function_obj, menu_var_or_value);

	} else if (ref_object.content.type) { 
		
		console.log("P2 chamando para: ");
		console.log(ref_object);

		menu_var_or_value.remove();
		variableValueMenuCode(command, ref_object, dom_object, function_obj, menu_var_or_value);

	} else {

		console.log("P3 chamando para: ");
		console.log(ref_object);

		menu_var_or_value.remove();
		variableValueMenuCode(command, ref_object, dom_object, function_obj, menu_var_or_value);

	}
}

function variableValueMenuCode (command, variable_obj, dom_object, function_obj, menu_var_or_value) {


	var ret = '';
	if (variable_obj.function_called) {

		if (variable_obj.function_called.parameters_list == null || variable_obj.function_called.length == 0) {

			menu_var_or_value.find('.text').text(' ');
			dom_object.find('.menu_var_or_value_dom').remove();

			var parameters_menu = '<div class="parameters_function_called"> '+variable_obj.function_called.name+' <span> ( </span>';
			
			parameters_menu += '<span> ) </span></div>';

			parameters_menu = $(parameters_menu);

			dom_object.append(parameters_menu);

			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
			context_menu += '</div></div>';

			context_menu = $(context_menu);

			context_menu.insertAfter( dom_object.find('.parameters_function_called') );

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
			     if ($selectedItem.data('clear')) {
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
			     	delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj);
			     }
		      }
			});
		} else {

			console.log("sei que preciso colcoar parametros");
			menu_var_or_value.find('.text').text(' ');
			dom_object.find('.menu_var_or_value_dom').remove();

			var parameters_menu = '<div class="parameters_function_called"> '+variable_obj.function_called.name+' <span> ( </span>';
			
			parameters_menu += '<span> ) </span></div>';

			parameters_menu = $(parameters_menu);

			dom_object.append(parameters_menu);

			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
			context_menu += '</div></div>';

			context_menu = $(context_menu);

			context_menu.insertAfter( dom_object.find('.parameters_function_called') );

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
			     if ($selectedItem.data('clear')) {
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
			     	delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj);
			     }
		      }
			});
		}


	} else if (variable_obj.content.type) {

		var variable_render = "";

		if (variable_obj.content.dimensions == 1) {

			variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content.name+'</span>';

			variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ]</span>';
			
			variable_render += '</div>';

			variable_render = $(variable_render);

			dom_object.append(variable_render);


			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
			context_menu += '</div></div>';

			context_menu = $(context_menu);

			//context_menu.insertAfter( dom_object.find('.variable_rendered') );

			variable_render.append(context_menu);

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
			     if ($selectedItem.data('clear')) {
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;
			     	delete variable_obj.function_called;
		     		delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj);
			     }
		      }
			});

			variableValueMenuCode(command, variable_obj.column, $(variable_render.find('.column_container')), function_obj, menu_var_or_value);

		} else if (variable_obj.content.dimensions == 2) {
			/*ret += ' [ ' + variableValueMenuCode(command, variable_obj.row, dom_object, function_obj) + ' ] ';
			ret += ' [ ' + variableValueMenuCode(command, variable_obj.column, dom_object, function_obj) + ' ] ';*/
		} else {

			variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content.name+'</span>';

			variable_render += '</div>';

			variable_render = $(variable_render);

			dom_object.append(variable_render);


			var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
			context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
			context_menu += '</div></div>';

			context_menu = $(context_menu);

			context_menu.insertAfter( variable_render );

			context_menu.dropdown({
				onChange: function(value, text, $selectedItem) {
			     if ($selectedItem.data('clear')) {
			     	dom_object.text('');

			     	variable_obj.content = null;
			     	variable_obj.row = null;
			     	variable_obj.column = null;

			     	delete variable_obj.function_called;
		     		delete variable_obj.parameters_list;

			     	renderMenu(command, variable_obj, dom_object, function_obj);
			     }
		      }
			});

		}

	} else {

		var variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_obj.content+'</span>';
		variable_render += '</div>';

		variable_render = $(variable_render);

		dom_object.append(variable_render);

		var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
		context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
		context_menu += '</div></div>';

		context_menu = $(context_menu);

		context_menu.insertAfter( variable_render );

		context_menu.dropdown({
			onChange: function(value, text, $selectedItem) {
		     if ($selectedItem.data('clear')) {
		     	dom_object.text('');

   				variable_obj.content = null;
			    variable_obj.row = null;
			    variable_obj.column = null;

		     	delete variable_obj.function_called;
		     	delete variable_obj.parameters_list;

		     	dom_object.find('.value_rendered').remove();
				dom_object.find('.context_menu_clear').remove();
				dom_object.find('.width-dynamic-minus').remove();

		     	renderMenu(command, variable_obj, dom_object, function_obj);
		     }
	      }
		});

		variable_render.on('click', function(e) {
			variable_render.remove();
			variable_render.empty();
			variable_render.remove();
			dom_object.empty();
			dom_object.append('<span class="menu_var_or_value_dom"> </span>');
			
			openInputToValue(command, variable_obj, dom_object, menu_var_or_value, function_obj);
		});
	}

	

}

function addFunctionsToMenu (function_obj, menu_var_or_value, ref_object) {
	var sub_menu = menu_var_or_value.find('.menu_only_functions');
	sub_menu.text('');

	for (var i = 0; i < window.program_obj.functions.length; i++) {
		var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_function+'">' + window.program_obj.functions[i].name + ' </div>');
		temp.data('function_reference', window.program_obj.functions[i]);
		sub_menu.append(temp);
	}
}

function addVariablesToMenu (function_obj, menu_var_or_value, ref_object) {

	var sub_menu = menu_var_or_value.find('.menu_only_vars');
	sub_menu.text('');

	if (window.program_obj.globals) {

		if (ref_object.include_constant) {
			for (var i = 0; i < window.program_obj.globals.length; i++) {
				var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + window.program_obj.globals[i].name + ' </div>');
				temp.data('variable_reference', window.program_obj.globals[i]);
				sub_menu.append(temp);
			}
		} else {
			for (var i = 0; i < window.program_obj.globals.length; i++) {
				if (!window.program_obj.globals[i].is_constant) {
					var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + window.program_obj.globals[i].name + ' </div>');
					temp.data('variable_reference', window.program_obj.globals[i]);
					sub_menu.append(temp);
				}
			}
		}
	}

	if (function_obj.parameters_list) {
		for (var i = 0; i < function_obj.parameters_list.length; i++) {
			var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + function_obj.parameters_list[i].name + ' </div>');
			temp.data('variable_reference', function_obj.parameters_list[i]);
			sub_menu.append(temp);
		}
	}

	if (function_obj.variables_list) {
		for (var i = 0; i < function_obj.variables_list.length; i++) {
			var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + function_obj.variables_list[i].name + ' </div>');
			temp.data('variable_reference', function_obj.variables_list[i]);
			sub_menu.append(temp);
		}
	}

}

function addHandlers (command, ref_object, dom_object, menu_var_or_value, function_obj) {

	if (ref_object.variable_and_value != VAR_OR_VALUE_TYPES.only_value) {
		menu_var_or_value.dropdown({
		  onChange: function(value, text, $selectedItem) {
		  	dom_object.find('.var_name').remove();

		     switch ($selectedItem.data('option')) {
		     	case VAR_OR_VALUE_TYPES.only_function:
		     		openInputToFunction(command, ref_object, dom_object, menu_var_or_value, function_obj, $($selectedItem).data('function_reference'));
		     		break;

		     	case VAR_OR_VALUE_TYPES.only_value:
		     		openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj);
		     		break;

		     	case VAR_OR_VALUE_TYPES.only_variable:
		     		openInputToVariable(command, ref_object, dom_object, menu_var_or_value, function_obj, $($selectedItem).data('variable_reference'));
		     		break;
		     }
	      }
	    });
	}

	dom_object.find('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    $(this).focus();

	    var tmpStr = $(this).val();
		$(this).val('');
		$(this).val(tmpStr);

	    $(this).css({
	        width: inputWidth
	    })
	}).trigger('input');
	
}

function openInputToFunction (command, ref_object, dom_object, menu_var_or_value, function_obj, function_selected) {
	
	ref_object.function_called = function_selected;
	ref_object.parameters_list = [];


	if (function_selected.parameters_list != null && function_selected.parameters_list.length > 0) {

		menu_var_or_value.find('.text').text(' ');
		dom_object.find('.menu_var_or_value_dom').remove();

		var parameters_menu = '<div class="parameters_function_called"> '+function_selected.name+' <span> ( </span>';
		for (var j = 0; j < function_selected.parameters_list.length; j++) {
			parameters_menu += '<div class="parameter_'+j+'"></div>';
			if ((j + 1) != function_selected.parameters_list.length) {
				parameters_menu += ' , ';
			}
		}
		parameters_menu += '<span> ) </span></div>';

		parameters_menu = $(parameters_menu);

		dom_object.append(parameters_menu);

		for (var j = 0; j < function_selected.parameters_list.length; j++) {
			var temp = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
			ref_object.parameters_list.push(temp);
			renderMenu(command, temp, parameters_menu.find('.parameter_'+j), function_obj);
		}


		var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
		context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
		context_menu += '</div></div>';

		context_menu = $(context_menu);

		context_menu.insertAfter( dom_object.find('.parameters_function_called') );

		context_menu.dropdown({
			onChange: function(value, text, $selectedItem) {
		     if ($selectedItem.data('clear')) {
		     	dom_object.text('');

		     	ref_object.content = null;
		     	ref_object.row = null;
		     	ref_object.column = null;
		     	delete ref_object.function_called;
		     	delete ref_object.parameters_list;

		     	renderMenu(command, ref_object, dom_object, function_obj);
		     }
	      }
		});

	} else {
		menu_var_or_value.find('.text').text(' ');
		dom_object.find('.menu_var_or_value_dom').remove();

		var parameters_menu = '<div class="parameters_function_called"> '+function_selected.name+' <span> ( </span>';
		
		parameters_menu += '<span> ) </span></div>';

		parameters_menu = $(parameters_menu);

		dom_object.append(parameters_menu);

		var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
		context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
		context_menu += '</div></div>';

		context_menu = $(context_menu);

		context_menu.insertAfter( dom_object.find('.parameters_function_called') );

		context_menu.dropdown({
			onChange: function(value, text, $selectedItem) {
		     if ($selectedItem.data('clear')) {
		     	dom_object.text('');

		     	ref_object.content = null;
		     	ref_object.row = null;
		     	ref_object.column = null;
		     	delete ref_object.function_called;
		     	delete ref_object.parameters_list;

		     	renderMenu(command, ref_object, dom_object, function_obj);
		     }
	      }
		});
	}

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj);
	}

	if (command.type == Models.COMMAND_TYPES.writer) {
		WritersManagement.addContent(command, ref_object, dom_object, menu_var_or_value, function_obj, ref_object.content);
	}
}

function openInputToVariable (command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected) {
	
	ref_object.content = variable_selected;

	menu_var_or_value.find('.text').text(' ');
	dom_object.find('.menu_var_or_value_dom').remove();

	var variable_render = '<div class="variable_rendered"> <span class="var_name">'+variable_selected.name+'</span>';

	if (variable_selected.dimensions == 1) {
		variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ]</span>';
	}
	if (variable_selected.dimensions == 2) {
		variable_render += ' <span>[ </span> <div class="row_container"></div> <span> ]</span> ';
		variable_render += ' <span>[ </span> <div class="column_container"></div> <span> ]</span>';
	}


	variable_render += '</div>';

	variable_render = $(variable_render);

	dom_object.append(variable_render);

	if (variable_selected.dimensions == 1) {
		ref_object.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.column, variable_render.find('.column_container'), function_obj);
	}
	if (variable_selected.dimensions == 2) {
		ref_object.row = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.row, variable_render.find('.row_container'), function_obj);

		ref_object.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.column, variable_render.find('.column_container'), function_obj);
	}

	var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	context_menu += '</div></div>';

	context_menu = $(context_menu);

	context_menu.insertAfter( dom_object.find('.variable_rendered') );

	context_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
	     if ($selectedItem.data('clear')) {
	     	dom_object.text('');

	     	ref_object.content = null;
	     	ref_object.row = null;
	     	ref_object.column = null;

	     	delete ref_object.function_called;
		    delete ref_object.parameters_list;

	     	renderMenu(command, ref_object, dom_object, function_obj);
	     }
      }
	});

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected);
	}

	if (command.type == Models.COMMAND_TYPES.writer) {
		WritersManagement.addContent(command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected);
	}

	
}


function openInputToValue (command, ref_object, dom_object, menu_var_or_value, function_obj) {

	if (ref_object.content == null) {
		ref_object.content = "";
	}

	menu_var_or_value.find('.text').text(' ');
	var field = $('<input type="text" size="2" class="width-dynamic-minus" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" />');
	field.insertBefore(dom_object.find('.menu_var_or_value_dom'));
	var rendered = $('<div class="value_rendered"></div>');
	rendered.insertBefore(field);

	field.focus();
	field.val(ref_object.content);

	dom_object.find('.menu_var_or_value_dom').remove();

	var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	context_menu += '</div></div>';

	context_menu = $(context_menu);

	context_menu.insertAfter( field );

	context_menu.dropdown({
		onChange: function(value, text, $selectedItem) {
	     if ($selectedItem.data('clear')) {
	     	dom_object.text('');

	     	dom_object.find('.value_rendered').remove();
			dom_object.find('.context_menu_clear').remove();
			dom_object.find('.width-dynamic-minus').remove();

			ref_object.content = null;
		    ref_object.row = null;
		    ref_object.column = null;

			delete ref_object.function_called;
		    delete ref_object.parameters_list;

	     	renderMenu(command, ref_object, dom_object, function_obj);
	     }
      }
	});

	dom_object.find('.width-dynamic-minus').focusout(function() {
		if ($(this).val().trim()) {
			ref_object.content = $(this).val().trim();
		}

		rendered.text(ref_object.content);
		$(this).remove();

	});

	dom_object.find('.width-dynamic-minus').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				ref_object.content = $(this).val().trim();
			}
			rendered.text(ref_object.content);

			$(this).remove();
		}
		if(code == 27) {
			rendered.text(ref_object.content);

			$(this).remove();
		}
	});

	rendered.on('click', function(e) {
		rendered.remove();
		rendered.empty();
		rendered.remove();
		dom_object.empty();
		dom_object.append('<span class="menu_var_or_value_dom"> </span>');
		
		openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj)
	});

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj);
	}

	if (command.type == Models.COMMAND_TYPES.writer) {
		WritersManagement.addContent(command, ref_object, dom_object, menu_var_or_value, function_obj, ref_object.content);
	}
}


$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};

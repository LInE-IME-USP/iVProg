import $ from 'jquery';
import { Types } from '../types';
import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as GlobalsManagement from '../globals';
import * as VariablesManagement from '../variables';
import * as AttribuitionsManagement from './attribution';
import WatchJS from 'melanke-watchjs';


var watch = WatchJS.watch;


export const VAR_OR_VALUE_TYPES = Object.freeze({only_variable: 1, only_value: 2, only_function: 3, variable_and_function: 4, variable_and_value_opt: 5,
	value_and_function: 6, all: 7});

export function renderMenu (command, ref_object, dom_object, function_obj, size_field = 2) {

	var menu_var_or_value = '<div class="ui dropdown menu_var_or_value_dom"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';

	if ((ref_object.variable_and_value == VAR_OR_VALUE_TYPES.only_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.variable_and_function) 
		|| (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.value_and_function) || (ref_object.variable_and_value == VAR_OR_VALUE_TYPES.all)) {
		
		menu_var_or_value += '<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_function+'"><i class="dropdown icon"></i>'+LocalizedStrings.getUI('btn_function')+'</div>';
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

    $(dom_object).append(menu_var_or_value);

    addHandlers(command, ref_object, dom_object, menu_var_or_value, function_obj);

    addVariablesToMenu(function_obj, menu_var_or_value, ref_object);
}

function addVariablesToMenu (function_obj, menu_var_or_value, ref_object) {

	watch(window.program_obj.globals, function(){
    	addVariablesToMenu(function_obj, menu_var_or_value, ref_object);
	}, 1);

	watch(function_obj.parameters_list, function(){
    	addVariablesToMenu(function_obj, menu_var_or_value, ref_object);
	}, 1);

	watch(function_obj.variables_list, function(){
    	addVariablesToMenu(function_obj, menu_var_or_value, ref_object);
	}, 1);

	var sub_menu = $(menu_var_or_value).find('.menu_only_vars');
	$(sub_menu).text('');

	if (window.program_obj.globals) {

		if (ref_object.include_constant) {
			for (var i = 0; i < window.program_obj.globals.length; i++) {
				var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + window.program_obj.globals[i].name + ' </div>');
				$(temp).data('variable_reference', window.program_obj.globals[i]);
				$(sub_menu).append(temp);
			}
		} else {
			for (var i = 0; i < window.program_obj.globals.length; i++) {
				if (!window.program_obj.globals[i].is_constant) {
					var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + window.program_obj.globals[i].name + ' </div>');
					$(temp).data('variable_reference', window.program_obj.globals[i]);
					$(sub_menu).append(temp);
				}
			}
		}

		
	}

	if (function_obj.parameters_list) {
		for (var i = 0; i < function_obj.parameters_list.length; i++) {
			var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + function_obj.parameters_list[i].name + ' </div>');
			$(temp).data('variable_reference', function_obj.parameters_list[i]);
			$(sub_menu).append(temp);
		}
	}

	if (function_obj.variables_list) {
		for (var i = 0; i < function_obj.variables_list.length; i++) {
			var temp = $('<div class="item" data-option="'+VAR_OR_VALUE_TYPES.only_variable+'">' + function_obj.variables_list[i].name + ' </div>');
			$(temp).data('variable_reference', function_obj.variables_list[i]);
			$(sub_menu).append(temp);
		}
	}

}

function addHandlers (command, ref_object, dom_object, menu_var_or_value, function_obj) {

	if (ref_object.variable_and_value != VAR_OR_VALUE_TYPES.only_value) {
		$(menu_var_or_value).dropdown({
		  onChange: function(value, text, $selectedItem) {
		  	$(dom_object).find('.var_name').remove();

		     switch ($($selectedItem).data('option')) {
		     	case VAR_OR_VALUE_TYPES.only_function:
		     		console.log("foi função");
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

	$(dom_object).find('.width-dynamic').on('input', function() {
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

function openInputToVariable (command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected) {


	ref_object.content = variable_selected;

	$(menu_var_or_value).find('.text').text(' ');
	$(dom_object).find('.menu_var_or_value_dom').remove();

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

	$(dom_object).append(variable_render);


	if (variable_selected.dimensions == 1) {
		ref_object.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.column, $(variable_render).find('.column_container'), function_obj);
	}
	if (variable_selected.dimensions == 2) {
		ref_object.row = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.row, $(variable_render).find('.row_container'), function_obj);

		ref_object.column = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
		renderMenu(command, ref_object.column, $(variable_render).find('.column_container'), function_obj);
	}

	var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	context_menu += '</div></div>';

	context_menu = $(context_menu);

	$( context_menu ).insertAfter( $(dom_object).find('.variable_rendered') );

	$(context_menu).dropdown({
		onChange: function(value, text, $selectedItem) {
	     if ($($selectedItem).data('clear')) {
	     	$(dom_object).text('');
	     	ref_object = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);
	     	renderMenu(command, ref_object, dom_object, function_obj);
	     }
      }
	});

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj, variable_selected);
	}
}


function openInputToValue (command, ref_object, dom_object, menu_var_or_value, function_obj) {

	if (ref_object.content == null) {
		ref_object.content = "";
	}

	$(menu_var_or_value).find('.text').text(' ');
	var field = $('<input type="text" size="2" class="width-dynamic-minus" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false" />');
	$( field ).insertBefore($(dom_object).find('.menu_var_or_value_dom'));
	var rendered = $('<div class="value_rendered"></div>');
	$( rendered ).insertBefore(field);

	field.focus();
	$(field).val(ref_object.content);

	$(dom_object).find('.menu_var_or_value_dom').remove();

	var context_menu = '<div class="ui dropdown context_menu_clear"><div class="text"></div><i class="dropdown icon"></i><div class="menu">';
	context_menu += '<div class="item" data-clear="true">'+LocalizedStrings.getUI('btn_clear')+'</div>';
	context_menu += '</div></div>';

	context_menu = $(context_menu);

	$( context_menu ).insertAfter( field );

	$(context_menu).dropdown({
		onChange: function(value, text, $selectedItem) {
	     if ($($selectedItem).data('clear')) {
	     	$(dom_object).text('');

	     	ref_object = new Models.VariableValueMenu(VAR_OR_VALUE_TYPES.all, null, null, null, true);

	     	$(dom_object).find('.value_rendered').remove();
			$(dom_object).find('.context_menu_clear').remove();
			$(dom_object).find('.width-dynamic-minus').remove();

	     	renderMenu(command, ref_object, dom_object, function_obj);
	     }
      }
	});

	$(dom_object).find('.width-dynamic-minus').focusout(function() {
		if ($(this).val().trim()) {
			ref_object.content = $(this).val().trim();
		}

		$(rendered).text(ref_object.content);
		$(this).remove();

	});

	$(dom_object).find('.width-dynamic-minus').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				ref_object.content = $(this).val().trim();
			}
			$(rendered).text(ref_object.content);

			$(this).remove();
		}
		if(code == 27) {
			$(rendered).text(ref_object.content);

			$(this).remove();
		}
	});

	$(rendered).on('click', function(e) {
		console.log('no clique, vou passar o seguinte: ');
		console.log(dom_object);
		rendered.remove();
		rendered.empty();
		$(rendered).remove();
		$(dom_object).empty();
		$(dom_object).append('<span class="menu_var_or_value_dom"> </span>');
		//$('<span class="menu_var_or_value_dom"> </span>').insertBefore($(dom_object).find('.value_rendered'));
		//$(dom_object).find('.value_rendered').remove();
		//$(dom_object).find('.context_menu_clear').remove();
		//$(dom_object).find('.width-dynamic-minus').remove();

		openInputToValue(command, ref_object, dom_object, menu_var_or_value, function_obj)
	});

	if (command.type == Models.COMMAND_TYPES.attribution) {
		AttribuitionsManagement.renderMenuOperations(command, ref_object, dom_object, menu_var_or_value, function_obj);
	}
}


$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};

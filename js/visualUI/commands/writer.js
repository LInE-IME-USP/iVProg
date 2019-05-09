import * as Models from '../ivprog_elements';
import { LocalizedStrings } from '../../services/localizedStringsService';
import * as VariableValueMenu from './variable_value_menu';
import * as VariableValueMenuManagement from './variable_value_menu';
import * as CommandsManagement from '../commands';
import * as GenericExpressionManagement from './generic_expression';

export function createFloatingCommand () {
	return $('<div class="ui writer created_element"> <i class="ui icon small upload"></i> <span> '+LocalizedStrings.getUI('text_command_write')+' var </span></div>');
}

export function renderCommand (command, function_obj) {
	var ret = '';
	ret += '<div class="ui writer command_container"> <i class="ui icon small upload command_drag"></i> <i class="ui icon times red button_remove_command"></i> <span class="span_command_spec">'+LocalizedStrings.getUI('text_command_write')+' ( </span><div class="all_elements_write"></div> <span class="close_parentheses span_command_spec">)</span> </div>';

	var el = $(ret);
	el.data('command', command);

	//renderExpression (command, function_obj, div_to_render, expression_array)

	GenericExpressionManagement.renderExpression(command, function_obj, el.find('.all_elements_write'), command.content);

	/*for (var i = 0; i < command.content.length; i ++) {
		var new_div_item = $( '<div class="var_value_menu_div"></div>' );
		var div_parent_with_handler = $( '<div class="div_parent_handler"></div>' );
		div_parent_with_handler.append($('<i class="ui icon ellipsis vertical inverted handler"></i>'));
		div_parent_with_handler.append(new_div_item);
		div_parent_with_handler.append($('<i class="white inverted icon times handler"></i>'));

		el.find('.all_elements_write').append(div_parent_with_handler);
		VariableValueMenu.renderMenu(command, command.content[i], new_div_item, function_obj);

		addHandlerIconAdd(el.find('.all_elements_write'), command, function_obj);

		addHandlersManager(command, function_obj, el, div_parent_with_handler, command.content[i]);
	}

	if (command.content.length == 0) {
		addHandlerIconAdd(el.find('.all_elements_write'), command, function_obj);
	}*/

	addHandlers(command, function_obj, el);
	return el;
}

function addHandlersManager (command, function_obj, writer_dom, item_div, content_element) {

	item_div.find('.times').on('click', function() {
		for (var i = 0; i < command.content.length; i++) {
			if (command.content[i] == content_element) {
				delete command.content[i];
	  			command.content.splice(i, 1);
				
				item_div.children().off();
				item_div.off();
				item_div.fadeOut();

				if (command.content.length > 0) {
					item_div.next('.icon_add_item_to_writer').fadeOut();
				}
				break;
			}
		}
	});
}

function addHandlers (command, function_obj, writer_dom) {

	writer_dom.find('.button_remove_command').on('click', function() {
		if (CommandsManagement.removeCommand(command, function_obj, writer_dom)) {
			writer_dom.fadeOut(400, function() {
				writer_dom.remove();
			});
		}
	});

	Sortable.create(writer_dom.find(".all_elements_write")[0], {
	    handle: '.ellipsis',
	    animation: 100,
	    ghostClass: 'ghost',
	    group: 'writer_' + Math.floor(Math.random() * 10000000),
	    draggable: '.div_parent_handler',
	    onEnd: function (evt) {
	    	
	    	command.content.splice(evt.newIndex, 0, command.content.splice(evt.oldIndex, 1)[0]);

	    	writer_dom.empty();
	    	writer_dom.replaceWith(renderCommand(command, function_obj));
	    }
	  });


}

function addHandlerIconAdd (dom_object, command, function_obj, insert_after = false, after_which = null) {
	var icon_add_item = $( '<i class="ui icon plus square outline icon_add_item_to_writer"></i> ' );
	if (!insert_after) {
		dom_object.append(icon_add_item);
	} else {
		icon_add_item.insertAfter(after_which);
	}
	
	icon_add_item.on('click', function(e) {
		var div_parent_with_handler = $( '<div class="div_parent_handler" style="display:none;"></div>' );
		var new_div_item = $( '<div class="var_value_menu_div"></div>' );
		div_parent_with_handler.append($('<i class="ui icon ellipsis vertical inverted handler"></i>'));
		div_parent_with_handler.append(new_div_item);
		div_parent_with_handler.append($('<i class="white inverted icon times handler"></i>'));
		div_parent_with_handler.insertAfter(icon_add_item);

		var new_related_menu = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);

		VariableValueMenu.renderMenu(command, new_related_menu, new_div_item, function_obj);

		addHandlerIconAdd(dom_object, command, function_obj, true, div_parent_with_handler);

		addHandlersManager(command, function_obj, dom_object, div_parent_with_handler, new_related_menu);
		var pos = 1;
		dom_object.find('.icon_add_item_to_writer').each(function() {
			if ($(this).get(0) === icon_add_item.get(0)) {
				command.content.splice(pos, 0, new_related_menu);
			} else {
				pos ++;
			}
		});
		if (command.content.length == 1) {
			icon_add_item.remove();
		}
		div_parent_with_handler.fadeIn();
	});
}

export function addContent (command, ref_object, dom_object, menu_var_or_value, function_obj, ref_object_content) {
	
	if (dom_object.hasClass('var_value_menu_div')) {
		var icon_add_item = $( '<i class="ui icon plus square outline icon_add_item_to_writer"></i> ' );
		icon_add_item.insertAfter(dom_object);

		icon_add_item.on('click', function(e) {
			var div_parent_with_handler = $( '<div class="div_parent_handler"></div>' );
			div_parent_with_handler.append($('<i class="ui icon ellipsis vertical inverted handler"></i>'));
			div_parent_with_handler.append(new_div_item);
			div_parent_with_handler.append($('<i class="white inverted icon times handler"></i>'));

			div_parent_with_handler.insertAfter(icon_add_item);
			var new_related_menu = new Models.VariableValueMenu(VariableValueMenuManagement.VAR_OR_VALUE_TYPES.all, null, null, null, true);

			VariableValueMenu.renderMenu(command, new_related_menu, new_div_item, function_obj);

			addHandlersManager(command, function_obj, dom_object, div_parent_with_handler, new_related_menu);

			command.content.push(new_related_menu);

			if (command.content.length == 1) {
				icon_add_item.remove();
			}
		});
	}
}

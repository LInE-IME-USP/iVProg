var counter_new_functions = 0;
var counter_new_parameters = 0;

function addFunctionHandler() {
	new_function = new Funcao("new_function_" + counter_new_functions);
	adicionarFuncao(new_function);

	counter_new_functions ++;
	renderAlgorithm();
}

function updateSequenceFunctionHandler(index_from, index_to) {
	programa.funcoes.splice(index_to, 0, programa.funcoes.splice(index_from, 1)[0]);
	renderAlgorithm();
}

function removeFunctionHandler(div_function, sequence) {
	programa.funcoes.splice(sequence, 1);

	$(div_function).slideUp(400, function(){
	    renderAlgorithm();
	});
    
}

function minimizeFunctionHandler(div_function, sequence) {
	$(div_function).find(".function_area").toggle();
	programa.funcoes[sequence].esta_oculta = !programa.funcoes[sequence].esta_oculta;
}

function renderAlgorithm() {
	$('.all_functions').empty();
	for (i = 0; i < programa.funcoes.length; i++) {
		appendFunction(programa.funcoes[i], i);
	}
	$('.data_types_dropdown').dropdown();
	
	$('.parameter_data_types_dropdown').dropdown();
	addHandlers();
}

function addHandlers() {

}

function addParameter(sequence) {
	if (programa.funcoes[sequence].lista_parametros == null) {
		programa.funcoes[sequence].lista_parametros = new Array();
	}
	programa.funcoes[sequence].lista_parametros.push(new Variavel(tiposDados.integer, "new_parameter_" + counter_new_parameters));
	counter_new_parameters ++;

	renderAlgorithm();
}

function updateFunctionReturn(sequence, new_value) {
	programa.funcoes[sequence].tipo_retorno = new_value;
}

function updateParameterType(wich_function, wich_parameter, new_value) {
	programa.funcoes[wich_function].lista_parametros[wich_parameter].tipo = new_value;
}



var opened_name_function = false;
var opened_input = null;
var sequence_name_opened;
function enableNameFunctionUpdate(div_el, sequence) {
	if (opened_name_function) {
		$(opened_input).focus();
		return;
	}
	opened_name_function = true;
	sequence_name_opened = sequence;

	$(div_el).find('.span_name_function').text('');
	$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+programa.funcoes[sequence].nome+"' />" ).insertBefore($(div_el).find('.span_name_function'));

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input = this;
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
			programa.funcoes[sequence_name_opened].nome = $(this).val().trim();
		}
		$(this).remove();

		/// update elements:
		opened_name_function = false;
		opened_input = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				programa.funcoes[sequence_name_opened].nome = $(this).val().trim();
			}
			$(this).remove();

			/// update elements:
			opened_name_function = false;
			opened_input = false;

			renderAlgorithm();
		}
		if(code == 27) {
			$(div_el).find('.span_name_function').text(programa.funcoes[sequence_name_opened].nome);

			$(this).remove();

			/// update elements:
			opened_name_function = false;
			opened_input = false;
		}
	});
	
}


var opened_name_parameter = false;
var opened_input_parameter = null;
var sequence_name_opened_parameter;
var sequence_function_opened_parameter;
function enableNameParameterUpdate(parent_node, which_function, which_parameter) {
	if (opened_name_parameter) {
		$(opened_input_parameter).focus();
		return;
	}
	opened_name_parameter = true;
	sequence_name_opened_parameter = which_parameter;
	sequence_function_opened_parameter = which_function;

	$(parent_node).find('.span_name_parameter').text('');
	$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+programa.funcoes[which_function].lista_parametros[which_parameter].nome+"' />" ).insertBefore($(parent_node).find('.span_name_parameter'));

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_parameter = this;
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
			programa.funcoes[which_function].lista_parametros[which_parameter].nome = $(this).val().trim();
		}
		$(this).remove();

		/// update elements:
		opened_name_parameter = false;
		opened_input_parameter = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				programa.funcoes[which_function].lista_parametros[which_parameter].nome = $(this).val().trim();
			}
			$(this).remove();

			/// update elements:
			opened_name_parameter = false;
			opened_input_parameter = false;

			renderAlgorithm();
		}
		if(code == 27) {
			$(parent_node).find('.span_name_parameter').text(programa.funcoes[which_function].lista_parametros[which_parameter].nome);

			$(this).remove();

			/// update elements:
			opened_name_parameter = false;
			opened_input_parameter = false;
		}
	});

}

function appendFunction(function_obj, sequence) {
	var appender = '<div class="ui secondary segment function_div list-group-item">'
		+ '<span class="glyphicon glyphicon-move" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>'
		
		+ (!function_obj.eh_principal ? '<button class="ui icon button large remove_function_button" onclick="removeFunctionHandler(this.parentNode, '+sequence+')"><i class="red icon times"></i></button>' : '')
		+ '<button class="ui icon button tiny minimize_function_button" onclick="minimizeFunctionHandler(this.parentNode, '+sequence+')"><i class="icon window minimize"></i></button>'

		+ '<div class="function_signature_div">function '

		+ '<select class="ui fluid dropdown data_types_dropdown" onchange="updateFunctionReturn('+sequence+', this.value)">'
        + '<option value="'+tiposDados.void+'" '+(function_obj.tipo_retorno == tiposDados.void ? 'selected' : '')+'>'+tiposDados.void+'</option>'
        + '<option value="'+tiposDados.integer+'" '+(function_obj.tipo_retorno == tiposDados.integer ? 'selected' : '')+'>'+tiposDados.integer+'</option>'
        + '<option value="'+tiposDados.real+'" '+(function_obj.tipo_retorno == tiposDados.real ? 'selected' : '')+'>'+tiposDados.real+'</option>'
        + '<option value="'+tiposDados.text+'" '+(function_obj.tipo_retorno == tiposDados.text ? 'selected' : '')+'>'+tiposDados.text+'</option>'
        + '<option value="'+tiposDados.boolean+'" '+(function_obj.tipo_retorno == tiposDados.boolean ? 'selected' : '')+'>'+tiposDados.boolean+'</option>'
        + '</select>'

		+ '<div class="function_name_div"><span class="span_name_function" ondblclick="enableNameFunctionUpdate(this.parentNode, '+sequence+')" >'+function_obj.nome+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableNameFunctionUpdate(this.parentNode, '+sequence+')"></i></div> ' 

		+ '( <i class="ui icon plus square outline add_parameter" onclick="addParameter('+sequence+')"></i> <div class="ui large labels parameters_list">';


		if (function_obj.lista_parametros != null) {
			for (var j = 0; j < function_obj.lista_parametros.length; j++) {
				var par_temp = function_obj.lista_parametros[j];

				appender += '<div class="ui label function_name_parameter"><span class="span_name_parameter" ondblclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')"></i>';

				appender += '<select class="ui fluid dropdown parameter_data_types_dropdown" onchange="updateParameterType('+sequence+', '+j+', this.value)">'
        		+ '<option value="'+tiposDados.integer+'" '+(par_temp.tipo == tiposDados.integer ? 'selected' : '')+'>'+tiposDados.integer+'</option>'
        		+ '<option value="'+tiposDados.real+'" '+(par_temp.tipo == tiposDados.real ? 'selected' : '')+'>'+tiposDados.real+'</option>'
        		+ '<option value="'+tiposDados.text+'" '+(par_temp.tipo == tiposDados.text ? 'selected' : '')+'>'+tiposDados.text+'</option>'
        		+ '<option value="'+tiposDados.boolean+'" '+(par_temp.tipo == tiposDados.boolean ? 'selected' : '')+'>'+tiposDados.boolean+'</option>'
        		+ '</select>';

				appender += "</div>";
			}
		}

		appender += '</div> ) {</div>'
		
		+ (function_obj.esta_oculta ? ' <div class="function_area" style="display: none;"> ' : ' <div class="function_area"> ')

		+ '<div class="ui top attached segment variables_list_div"><div class="ui teal small labeled icon button add_variable_button">Variable<i class="add icon"></i></div></div>'
		+ '<div class="ui bottom attached segment commands_list_div"><div class="ui teal small labeled icon button add_command_button">Command<i class="add icon"></i></div></div>'		

		+ '<div class="function_close_div">}</div>'
		+ '</div>'
		+ '</div>';

	$('.all_functions').append(appender); 
}










$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};

var counter_new_functions = 0;

function addFunctionHandler() {
	new_function = new Funcao("new_function_" + counter_new_functions);
	programa.adicionarFuncao(new_function);

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
}

function updateFunctionReturn(sequence, new_value) {
	programa.funcoes[sequence].tipo_retorno = new_value;
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
	});
	
}

function appendFunction(function_obj, sequence) {
	$('.all_functions').append('<div class="ui secondary segment function_div list-group-item">'
		+ '<span class="glyphicon glyphicon-move" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>'
		
		+ (!function_obj.eh_principal ? '<button class="ui icon button large remove_function_button" onclick="removeFunctionHandler(this.parentNode, '+sequence+')"><i class="red icon times outline"></i></button>' : '')
		+ '<button class="ui icon button tiny minimize_function_button" onclick="minimizeFunctionHandler(this.parentNode, '+sequence+')"><i class="icon window minimize"></i></button>'

		+ '<div class="function_signature_div">function '

		+ '<select class="ui fluid dropdown data_types_dropdown" onchange="updateFunctionReturn('+sequence+', this.value)">'
        + '<option value="'+tiposDados.void+'" '+(function_obj.tipo_retorno == tiposDados.void ? 'selected' : '')+'>'+tiposDados.void+'</option>'
        + '<option value="'+tiposDados.integer+'" '+(function_obj.tipo_retorno == tiposDados.integer ? 'selected' : '')+'>'+tiposDados.integer+'</option>'
        + '<option value="'+tiposDados.real+'" '+(function_obj.tipo_retorno == tiposDados.real ? 'selected' : '')+'>'+tiposDados.real+'</option>'
        + '<option value="'+tiposDados.text+'" '+(function_obj.tipo_retorno == tiposDados.text ? 'selected' : '')+'>'+tiposDados.text+'</option>'
        + '<option value="'+tiposDados.boolean+'" '+(function_obj.tipo_retorno == tiposDados.boolean ? 'selected' : '')+'>'+tiposDados.boolean+'</option>'
        + '</select>'

		+ '<div class="function_name_div"><span class="span_name_function">'+function_obj.nome+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableNameFunctionUpdate(this.parentNode, '+sequence+')"></i></div> ( ) {</div>'
		
		+ (function_obj.esta_oculta ? '<div class="function_area" style="display: none;">' : '<div class="function_area">')

		+ '<div class="ui top attached segment variables_list_div"><div class="ui teal small labeled icon button add_variable_button">Variable<i class="add icon"></i></div></div>'
		+ '<div class="ui bottom attached segment commands_list_div"><div class="ui teal small labeled icon button add_command_button">Command<i class="add icon"></i></div></div>'		

		+ '<div class="function_close_div">}</div>'
		+ '</div>'
		+ '</div>'); 
}










$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};

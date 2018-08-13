var counter_new_functions = 0;
var counter_new_parameters = 0;

function addFunctionHandler() {
	new_function = new Funcao(i18n("new_function") + "_" + counter_new_functions);
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
	$('.ui.dropdown.function_return')
    	.dropdown({
		    onChange: function(value, text, $selectedItem) {

		    	classList = $selectedItem.attr('class').split(/\s+/);
				$.each(classList, function(index, item) {
				    if (item.indexOf("seq_") > -1) {
				        seq = item.split("seq_")[1];


						for (tm in tiposDados) {
							if ($selectedItem.hasClass(tm)) {
								programa.funcoes[seq].tipo_retorno = tm;
							} 
						}

				        updateFunctionReturn(seq, value);
				    }
				});

		    }
		})
  	;

  	$('.ui.dropdown.parameter_type').dropdown({
		    onChange: function(value, text, $selectedItem) {

		    	classList = $selectedItem.attr('class').split(/\s+/);
		    	var fun;
				var seq;
				$.each(classList, function(index, item) {

					if (item.indexOf("fun_") > -1) {
						fun = item.split("fun_")[1];
					}
				    if (item.indexOf("seq_") > -1) {
				        seq = item.split("seq_")[1];
				    }
				});
				var dim = 0;
				if (value.indexOf(i18n(tiposDados.vector)) > -1) {
					dim = 1;
				}
				for (tm in tiposDados) {
					if ($selectedItem.hasClass(tm)) {
						console.log("possui: " + tm);
						updateParameterType(fun, seq, tm, dim);
						break;
					} 
				}

		    }
		});
}

function addParameter(sequence) {
	if (programa.funcoes[sequence].lista_parametros == null) {
		programa.funcoes[sequence].lista_parametros = new Array();
	}
	programa.funcoes[sequence].lista_parametros.push(new Variavel(tiposDados.integer, i18n("new_parameter") + "_" + counter_new_parameters));
	counter_new_parameters ++;

	renderAlgorithm();
}

function updateFunctionReturn(sequence, new_value) {
	if (new_value.indexOf(i18n(tiposDados.vector)) > -1) {

		programa.funcoes[sequence].dimensoes_retorno = 1;
	} else {

		programa.funcoes[sequence].dimensoes_retorno = 0;
	}
	
}

function updateParameterType(wich_function, wich_parameter, new_value, new_dimensions) {
	programa.funcoes[wich_function].lista_parametros[wich_parameter].tipo = new_value;
	programa.funcoes[wich_function].lista_parametros[wich_parameter].dimensoes = new_dimensions;

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

function removeParameter(parent_node, which_function, which_parameter) {
	programa.funcoes[which_function].lista_parametros.splice(which_parameter, 1);
	renderAlgorithm();
}

function appendFunction(function_obj, sequence) {
	var appender = '<div class="ui secondary segment function_div list-group-item">'
		+ '<span class="glyphicon glyphicon-move" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>'
		
		+ (!function_obj.eh_principal ? '<button class="ui icon button large remove_function_button" onclick="removeFunctionHandler(this.parentNode, '+sequence+')"><i class="red icon times"></i></button>' : '')
		+ '<button class="ui icon button tiny minimize_function_button" onclick="minimizeFunctionHandler(this.parentNode, '+sequence+')"><i class="icon window minimize"></i></button>'

		+ '<div class="function_signature_div">'+i18n('function')+' ';


    if (function_obj.eh_principal) {
    	appender += '<div class="function_name_div">  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ' + i18n('void') + ' &nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <span class="span_name_function" >'+function_obj.nome+'</span> </div> '
			+ '( <div class="ui large labels parameters_list">';
    } else {
    	appender += renderFunctionReturn(function_obj, sequence);

	    appender += '<div class="function_name_div"><span class="span_name_function" ondblclick="enableNameFunctionUpdate(this.parentNode, '+sequence+')" >'+function_obj.nome+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableNameFunctionUpdate(this.parentNode, '+sequence+')"></i></div> ' 
    		+ '( <i class="ui icon plus square outline add_parameter" onclick="addParameter('+sequence+')"></i> <div class="ui large labels parameters_list">';
    }

    appender += renderFunctionParameters(function_obj, sequence);
		
	appender += '</div> ) {</div>'
		+ (function_obj.esta_oculta ? ' <div class="function_area" style="display: none;"> ' : ' <div class="function_area"> ')

		+ '<div class="ui top attached segment variables_list_div"><div class="ui teal small labeled icon button add_variable_button">'+i18n('Variable')+'<i class="add icon"></i></div></div>'
		+ '<div class="ui bottom attached segment commands_list_div"><div class="ui teal small labeled icon button add_command_button">'+i18n('Command')+'<i class="add icon"></i></div></div>'		

		+ '<div class="function_close_div">}</div>'
		+ '</div>'
		+ '</div>';

	$('.all_functions').append(appender); 
}

// Essa função imprime os parâmetros e cria os elementos para a sua manipulação
function renderFunctionParameters(function_obj, sequence) {
	var ret = "";
	if (function_obj.lista_parametros != null) {

		for (var j = 0; j < function_obj.lista_parametros.length; j++) {

			var par_temp = function_obj.lista_parametros[j];

			ret += '<div class="ui label function_name_parameter"><span class="span_name_parameter" ondblclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')"></i>';

			ret += '<div class="ui dropdown parameter_type seq_'+j+' fun_'+sequence+'">';
  	
		  	if (par_temp.dimensoes > 0) {
		  		ret += '<div class="text seq_'+j+' fun_'+sequence+'">'+ i18n(tiposDados.vector)+':'+i18n(par_temp.tipo);
		  		ret += '</div>';
		  	} else {
		  		ret += '<div class="text seq_'+j+' fun_'+sequence+'">'+i18n(par_temp.tipo)+'</div>';
		  	}

		  	ret += '<i class="dropdown icon"></i>'
		  		+ '<div class="menu seq_'+j+' fun_'+sequence+'">';

		  	var i = 0;
		  	for (tm in tiposDados) {
		  		i ++;
		  		if (i == 1) { continue; }
		  		if (i == (Object.keys(tiposDados).length - 1)) { break; }

		  		ret += '<div class="item ' + ((par_temp.tipo == tm && par_temp.dimensoes < 1) ? ' selected ' : '') + ' seq_'+j+' fun_'+sequence+' '+tm+'" >'+i18n(tm)+'</div>';

		  	}

		  	i = 0;
		  	for (tm in tiposDados) {
		  		i ++;
		  		if (i == 1) { continue; }
		  		if (i == (Object.keys(tiposDados).length)) { break; }

		  		ret += '<div class="item seq_'+j+' '+tm+' fun_'+sequence+' ' + ((par_temp.tipo == tm && par_temp.dimensoes > 0) ? ' selected ' : '') + ' ">'
			    	+  i18n(tiposDados.vector)+':'+i18n(tm)
			    	+ '</div>';	
		  	}

    		ret += '</div></div>';




			ret += ' <i class="red icon times remove_parameter" onclick="removeParameter(this.parentNode, '+sequence+', '+j+')"></i></div>';

		}
	}
	return ret;
}


function renderVariables(function_obj, sequence) {
	var ret = "";
	if (function_obj.lista_parametros != null) {

		for (var j = 0; j < function_obj.lista_parametros.length; j++) {

			var par_temp = function_obj.lista_parametros[j];

			/*ret += '<div class="ui label function_name_parameter"><span class="span_name_parameter" ondblclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')"></i>';

			ret += '<select class="ui fluid dropdown parameter_data_types_dropdown" onchange="updateParameterType('+sequence+', '+j+', this.value)">'
    		+ '<option value="'+tiposDados.integer+'" '+(par_temp.tipo == tiposDados.integer ? 'selected' : '')+'>'+i18n(tiposDados.integer)+'</option>'
    		+ '<option value="'+tiposDados.real+'" '+(par_temp.tipo == tiposDados.real ? 'selected' : '')+'>'+i18n(tiposDados.real)+'</option>'
    		+ '<option value="'+tiposDados.text+'" '+(par_temp.tipo == tiposDados.text ? 'selected' : '')+'>'+i18n(tiposDados.text)+'</option>'
    		+ '<option value="'+tiposDados.boolean+'" '+(par_temp.tipo == tiposDados.boolean ? 'selected' : '')+'>'+i18n(tiposDados.boolean)+'</option>'
    		+ '</select>';

			ret += ' <i class="red icon times remove_parameter" onclick="removeParameter(this.parentNode, '+sequence+', '+j+')"></i></div>';*/

			ret += '<div class="ui label function_name_parameter"><span class="span_name_parameter" ondblclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameParameterUpdate(this.parentNode, '+sequence+', '+j+')"></i>';

			ret += '<div class="ui dropdown parameter_type seq_'+j+' fun_'+sequence+'">';
  	
		  	if (par_temp.dimensoes > 0) {
		  		ret += '<div class="text seq_'+j+' fun_'+sequence+'">'+ i18n(tiposDados.vector)+':'+i18n(par_temp.tipo);
		  		for (i = 0; i < par_temp.dimensoes; i ++) {
		  			ret += ' [ ] ';
		  		}

		  		ret += '</div>';
		  	} else {
		  		ret += '<div class="text seq_'+j+' fun_'+sequence+'">'+i18n(par_temp.tipo)+'</div>';
		  	}

		  	ret += '<i class="dropdown icon"></i>'
		  		+ '<div class="menu seq_'+j+' fun_'+sequence+'">';

		  	var i = 0;
		  	for (tm in tiposDados) {
		  		i ++;
		  		if (i == 1) { continue; }
		  		if (i == (Object.keys(tiposDados).length - 1)) { break; }

		  		ret += '<div class="item ' + (par_temp.tipo == tm ? ' selected ' : '') + ' seq_'+j+' fun_'+sequence+' '+tm+'" >'+i18n(tm)+'</div>';

		  	}

		  	i = 0;
		  	for (tm in tiposDados) {
		  		i ++;
		  		if (i == 1) { continue; }
		  		if (i == (Object.keys(tiposDados).length)) { break; }

		  		ret += '<div class="item seq_'+j+' fun_'+sequence+'">'
			    	+ '<i class="dropdown icon"></i>'
			    	+  i18n(tiposDados.vector)+':'+i18n(tm)
			      	+  '<div class="menu seq_'+j+' fun_'+sequence+'">'
				        + '<div class="item seq_'+j+' fun_'+sequence+' '+tm+'" data-text="'+ i18n(tiposDados.vector)+':'+i18n(tm)+' [ ] ">[ ]</div>'
				        + '<div class="item seq_'+j+' fun_'+sequence+' '+tm+'" data-text="'+ i18n(tiposDados.vector)+':'+i18n(tm)+' [ ] [ ] ">[ ] [ ] </div>'
				        + '<div class="item seq_'+j+' fun_'+sequence+' '+tm+'" data-text="'+ i18n(tiposDados.vector)+':'+i18n(tm)+' [ ] [ ] [ ]">[ ] [ ] [ ] </div>'
			      	+  '</div>'
			    	+ '</div>';	
		  	}

    		ret += '</div></div>';




			ret += ' <i class="red icon times remove_parameter" onclick="removeParameter(this.parentNode, '+sequence+', '+j+')"></i></div>';

		}
	}
	return ret;
}

// Essa função imprime o tipo de retorno da função e cria o menu do tipo 'select' para alteração
function renderFunctionReturn(function_obj, sequence) {

	var ret = '<div class="ui dropdown function_return seq_'+sequence+'">';
  	
  	if (function_obj.dimensoes_retorno > 0) {
  		ret += '<div class="text seq_'+sequence+'"">'+ i18n(tiposDados.vector)+':'+i18n(function_obj.tipo_retorno);
  		ret += '</div>';
  	} else {
  		ret += '<div class="text seq_'+sequence+'"">'+i18n(function_obj.tipo_retorno)+'</div>';
  	}

  	ret += '<i class="dropdown icon"></i>'
  		+ '<div class="menu seq_'+sequence+'"">';

  	var i = 0;
  	for (tm in tiposDados) {
  		if (i == (Object.keys(tiposDados).length - 1)) { break; }

  		ret += '<div class="item ' + ((function_obj.tipo_retorno == tm && function_obj.dimensoes_retorno < 1) ? ' selected ' : '') + ' seq_'+sequence+' '+tm+'" >'+i18n(tm)+'</div>';

  		i ++;
  	}

  	i = 0;
  	for (tm in tiposDados) {
  		i ++;
  		if (i == 1) { continue; }
  		if (i == (Object.keys(tiposDados).length)) { break; }

  		ret += '<div class="item seq_'+sequence+' '+tm+' '+ ((function_obj.tipo_retorno == tm && function_obj.dimensoes_retorno > 0) ? ' selected ' : '') +'" data-text="'+i18n(tiposDados.vector)+':'+i18n(tm)+' ">'
	    	+  i18n(tiposDados.vector)+':'+i18n(tm)
	    	+ '</div>';	
  	}

    ret += '</div></div>';

    return ret;
}










$.fn.textWidth = function(text, font) {
    
    if (!$.fn.textWidth.fakeEl) $.fn.textWidth.fakeEl = $('<span>').hide().appendTo(document.body);
    
    $.fn.textWidth.fakeEl.text(text || this.val() || this.text() || this.attr('placeholder')).css('font', font || this.css('font'));
    
    return $.fn.textWidth.fakeEl.width();
};

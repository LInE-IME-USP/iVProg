var counter_new_functions = 0;
var counter_new_parameters = 0;
var counter_new_variables = 0;
var counter_new_globals = 0;

function addFunctionHandler() {
	new_function = new Funcao(i18n("new_function") + "_" + counter_new_functions, tiposDados.void, 0, new Array(), false, false, null, new Comentario(i18n('text_comment_start')));
	adicionarFuncao(new_function);

	counter_new_functions ++;
	renderAlgorithm();
}

function addGlobalVar() {
	var v = new Variavel(tiposDados.integer, i18n('new_global') + '_' + counter_new_globals, 1);
	counter_new_globals ++;

	programa.globais.push(v);
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

	if (programa.globais.length > 0) {
		renderGlobals();
	}

	$('.data_types_dropdown').dropdown();
	
	$('.parameter_data_types_dropdown').dropdown();
	addHandlers();

}

function renderGlobals() {
	var ret = "";
	if (programa.globais.length > 0) {

		$('.list_globals').empty();

		for (var j = 0; j < programa.globais.length; j++) {

			var par_temp = programa.globais[j];

			ret += '<div class="ui label name_variable"><span class="span_name_variable" ondblclick="enableNameGlobalUpdate(this.parentNode, '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameGlobalUpdate(this.parentNode, '+j+')"></i>';

			ret += '<div class="ui dropdown global_type seq_'+j+'">';
  	
		  	if (par_temp.dimensoes > 0) {
		  		ret += '<div class="text seq_'+j+'">'+ i18n(tiposDados.vector)+':'+i18n(par_temp.tipo);
		  		for (i = 0; i < par_temp.dimensoes; i ++) {
		  			ret += ' [ ] ';
		  		}

		  		ret += '</div>';
		  	} else {
		  		ret += '<div class="text seq_'+j+'">'+i18n(par_temp.tipo)+'</div>';
		  	}

		  	ret += '<i class="dropdown icon"></i>'
		  		+ '<div class="menu seq_'+j+'">';

		  	var i = 0;
		  	for (tm in tiposDados) {
		  		i ++;
		  		if (i == 1) { continue; }
		  		if (i == (Object.keys(tiposDados).length)) { break; }

		  		ret += '<div class="item ' + (par_temp.tipo == tm ? ' selected ' : '') + ' seq_'+j+' '+tm+'" >'+i18n(tm)+'</div>';

		  	}

		  	i = 0;
		  	for (tm in tiposDados) {
		  		i ++;
		  		if (i == 1) { continue; }
		  		if (i == (Object.keys(tiposDados).length)) { break; }

		  		ret += '<div class="item seq_'+j+' ">'
			    	+ '<i class="dropdown icon"></i>'
			    	+  i18n(tiposDados.vector)+':'+i18n(tm)
			      	+  '<div class="menu seq_'+j+' ">'
				        + '<div class="item seq_'+j+' '+tm+'" data-text="'+ i18n(tiposDados.vector)+':'+i18n(tm)+' [ ] ">[ ]</div>'
				        + '<div class="item seq_'+j+' '+tm+'" data-text="'+ i18n(tiposDados.vector)+':'+i18n(tm)+' [ ] [ ] ">[ ] [ ] </div>'
			      	+  '</div>'
			    	+ '</div>';	
		  	}

    		ret += '</div></div>  = ';

    		if (par_temp.dimensoes == 0) {
    			if (par_temp.tipo == tiposDados.real) {
    				ret += '<div class="div_valor_var"><span class="span_value_variable" ondblclick="enableGlobalValueUpdate(this.parentNode, '+j+')" >'+par_temp.valor.toFixed(1)+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableGlobalValueUpdate(this.parentNode, '+j+')"></i></div> ';
    			} else {
    				if (par_temp.tipo == tiposDados.boolean) {
	    				ret += '<div class="div_valor_var"><span class="span_value_variable" ondblclick="alternateBooleanGlobalValue(this.parentNode, '+j+')" >'+par_temp.valor+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="alternateBooleanGlobalValue(this.parentNode, '+j+')"></i></div> ';
	    			} else {
    					ret += '<div class="div_valor_var"><span class="span_value_variable" ondblclick="enableGlobalValueUpdate(this.parentNode, '+j+')" >'+par_temp.valor+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableGlobalValueUpdate(this.parentNode, '+j+')"></i></div> ';
	    			}
    			}
    		} else {
    			ret += '<table class="tabela_var">';

    			if (par_temp.dimensoes == 1) {
    				ret += '<tr>';
    				if (par_temp.tipo == tiposDados.real) {
    					for (var k = 0; k < par_temp.colunas; k++) {
	    					ret += '<td><span class="span_value_variable" ondblclick="enableGlobalVectorValueUpdate(this.parentNode, '+j+', '+k+')" >'+par_temp.valor[k].toFixed(1)+'</span>'+'</td>';
	    				}
    				} else {
    					for (var k = 0; k < par_temp.colunas; k++) {
    						if (par_temp.tipo == tiposDados.boolean) {
    							ret += '<td><span class="span_value_variable" ondblclick="alternateBooleanGlobalVectorValue(this.parentNode, '+j+', '+k+')" >'+par_temp.valor[k]+'</span>'+'</td>';
    						} else {
    							ret += '<td><span class="span_value_variable" ondblclick="enableGlobalVectorValueUpdate(this.parentNode, '+j+', '+k+')" >'+par_temp.valor[k]+'</span>'+'</td>';
    						}
    					}
    				}
    				
    				ret += '</tr>';
    				ret += '</table>';

    				ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline" onclick="removeGlobalColumnVector('+j+')"></i>'
    			    	+ ' <i class="ui icon plus square outline" onclick="addGlobalColumnVector('+j+')"></i></div>';
    			}

    			if (par_temp.dimensoes == 2) {
    				if (par_temp.tipo == tiposDados.real) {
    					for (var l = 0; l < par_temp.linhas; l++) {
		    				ret += '<tr>';
		    				for (var k = 0; k < par_temp.colunas; k++) {
		    					ret += '<td><span class="span_value_variable" ondblclick="enableGlobalMatrixValueUpdate(this.parentNode, '+j+', '+l+', '+k+')" >'+par_temp.valor[l][k].toFixed(1)+'</span>'+'</td>';
		    				} 
		    				ret += '</tr>';
	    				}
    				} else {
    					for (var l = 0; l < par_temp.linhas; l++) {
		    				ret += '<tr>';
		    				for (var k = 0; k < par_temp.colunas; k++) {
		    					if (par_temp.tipo == tiposDados.boolean) { 
		    						ret += '<td><span class="span_value_variable" ondblclick="alternateBooleanGlobalMatrixValue(this.parentNode, '+j+', '+l+', '+k+')" >'+par_temp.valor[l][k]+'</span>'+'</td>';
		    					} else {
		    						ret += '<td><span class="span_value_variable" ondblclick="enableGlobalMatrixValueUpdate(this.parentNode, '+j+', '+l+', '+k+')" >'+par_temp.valor[l][k]+'</span>'+'</td>';
		    					}
		    				} 
		    				ret += '</tr>';
	    				}
    				}
    				if (par_temp.linhas == 0) {
    					ret += '<tr><td></td></tr>';
    				}
    				ret += '<tr><td colspan="'+par_temp.colunas+'" class="tr_manage_lines"><i class="ui icon minus square outline" onclick="removeLineGlobalMatrix('+j+')"></i>'
    			    	+ ' <i class="ui icon plus square outline" onclick="addLineGlobalMatrix('+j+')"></i></td></tr>';
    				ret += '</table>';

    				ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline" onclick="removeColumnGlobalMatrix('+j+')"></i>'
    			    	+ ' <i class="ui icon plus square outline" onclick="addColumnGlobalMatrix('+j+')"></i></div>';
    			}
    			
    		}

			ret += ' <i class="red icon times remove_parameter" onclick="deleteGlobal('+j+')"></i></div>';

		}
	}

	$('.list_globals').append(ret); 
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
						updateParameterType(fun, seq, tm, dim);
						break;
					} 
				}

		    }
		});

  	$('.ui.dropdown.variable_type').dropdown({
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
					dim = value.split('[').length - 1;
				}
				for (tm in tiposDados) {
					if ($selectedItem.hasClass(tm)) {
						updateVariableType(fun, seq, tm, dim);
						break;
					} 
				}

		    }
		});

  	$('.ui.dropdown.global_type').dropdown({
		    onChange: function(value, text, $selectedItem) {

		    	classList = $selectedItem.attr('class').split(/\s+/);
		    	var fun;
				var seq;
				$.each(classList, function(index, item) {

				    if (item.indexOf("seq_") > -1) {
				        seq = item.split("seq_")[1];
				    }
				});
				var dim = 0;
				if (value.indexOf(i18n(tiposDados.vector)) > -1) {
					dim = value.split('[').length - 1;
				}
				for (tm in tiposDados) {
					if ($selectedItem.hasClass(tm)) {
						updateGlobalType(seq, tm, dim);
						break;
					} 
				}

		    }
		});

}

function updateVariableType(wich_function, wich_variable, new_value, new_dimensions) {
	programa.funcoes[wich_function].variaveis[wich_variable].tipo = new_value;
	programa.funcoes[wich_function].variaveis[wich_variable].dimensoes = new_dimensions;

	if (new_dimensions > 0) {
		programa.funcoes[wich_function].variaveis[wich_variable].linhas = new_dimensions;
		programa.funcoes[wich_function].variaveis[wich_variable].colunas = 2;
	}

	if (new_value == tiposDados.integer) {
		if (new_dimensions == 0) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = 1;
		}
		if (new_dimensions == 1) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [1, 1];
		}
		if (new_dimensions == 2) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [[1, 1], [1, 1]];
		}
	}

	if (new_value == tiposDados.real) {
		if (new_dimensions == 0) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = 1.0;
		}
		if (new_dimensions == 1) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [1.0, 1.0];
		}
		if (new_dimensions == 2) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [[1.0, 1.0], [1.0, 1.0]];
		}
	}

	if (new_value == tiposDados.text) {
		if (new_dimensions == 0) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = i18n(tiposDados.text);
		}
		if (new_dimensions == 1) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [i18n(tiposDados.text), i18n(tiposDados.text)];
		}
		if (new_dimensions == 2) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [[i18n(tiposDados.text), i18n(tiposDados.text)], [i18n(tiposDados.text), i18n(tiposDados.text)]];
		}
	}

	if (new_value == tiposDados.boolean) {
		if (new_dimensions == 0) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = true;
		}
		if (new_dimensions == 1) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [true, true];
		}
		if (new_dimensions == 2) {
			programa.funcoes[wich_function].variaveis[wich_variable].valor = [[true, true], [true, true]];
		}
	}

	renderAlgorithm();
}

function addColumnVector(which_function, which_variable) {
	programa.funcoes[which_function].variaveis[which_variable].colunas ++;

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.integer) {
		programa.funcoes[which_function].variaveis[which_variable].valor.push(1);
	}
	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.real) {
		programa.funcoes[which_function].variaveis[which_variable].valor.push(1.0);
	}
	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.text) {
		programa.funcoes[which_function].variaveis[which_variable].valor.push(i18n(tiposDados.text));
	}
	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.boolean) {
		programa.funcoes[which_function].variaveis[which_variable].valor.push(true);
	}
	renderAlgorithm();
}

function addColumnMatrix(which_function, which_variable) {
	programa.funcoes[which_function].variaveis[which_variable].colunas ++;

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.integer) {
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].linhas; i++) {
			programa.funcoes[which_function].variaveis[which_variable].valor[i].push(1);
		}
	}

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.real) {
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].linhas; i++) {
			programa.funcoes[which_function].variaveis[which_variable].valor[i].push(1.0);
		}
	}

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.text) {
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].linhas; i++) {
			programa.funcoes[which_function].variaveis[which_variable].valor[i].push(i18n(tiposDados.text));
		}
	}

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.boolean) {
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].linhas; i++) {
			programa.funcoes[which_function].variaveis[which_variable].valor[i].push(true);
		}
	}
	renderAlgorithm();
}

function addLineMatrix(which_function, which_variable) {
	programa.funcoes[which_function].variaveis[which_variable].linhas ++;

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.integer) {
		var n_l = [];
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].colunas; i++) {
			n_l.push(1);
		}
		programa.funcoes[which_function].variaveis[which_variable].valor.push(n_l);
	}
	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.real) {
		var n_l = [];
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].colunas; i++) {
			n_l.push(1.0);
		}
		programa.funcoes[which_function].variaveis[which_variable].valor.push(n_l);
	}

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.text) {
		var n_l = [];
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].colunas; i++) {
			n_l.push(i18n(tiposDados.text));
		}
		programa.funcoes[which_function].variaveis[which_variable].valor.push(n_l);
	}

	if (programa.funcoes[which_function].variaveis[which_variable].tipo == tiposDados.boolean) {
		var n_l = [];
		for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].colunas; i++) {
			n_l.push(true);
		}
		programa.funcoes[which_function].variaveis[which_variable].valor.push(n_l);
	}
	renderAlgorithm();
}

function removeColumnVector(which_function, which_variable) {
	if (programa.funcoes[which_function].variaveis[which_variable].colunas == 0) {
		return;
	}

	programa.funcoes[which_function].variaveis[which_variable].colunas --;
	programa.funcoes[which_function].variaveis[which_variable].valor.splice(programa.funcoes[which_function].variaveis[which_variable].valor.length - 1, 1);
	renderAlgorithm();
}

function removeColumnMatrix(which_function, which_variable) {
	if (programa.funcoes[which_function].variaveis[which_variable].colunas == 0) {
		return;
	}

	programa.funcoes[which_function].variaveis[which_variable].colunas --;

	for (i = 0; i < programa.funcoes[which_function].variaveis[which_variable].linhas; i++) {
		programa.funcoes[which_function].variaveis[which_variable].valor[i].splice(programa.funcoes[which_function].variaveis[which_variable].valor[i].length - 1, 1);
	}

	renderAlgorithm();
}

function removeLineMatrix(which_function, which_variable) {
	if (programa.funcoes[which_function].variaveis[which_variable].linhas == 0) {
		return;
	}

	programa.funcoes[which_function].variaveis[which_variable].linhas --;
	programa.funcoes[which_function].variaveis[which_variable].valor.splice(programa.funcoes[which_function].variaveis[which_variable].valor.length - 1, 1);

	renderAlgorithm();
}


function addVariable(sequence) {//tipo, nome, valor
	var v = new Variavel(tiposDados.integer, i18n('new_variable') + '_' + counter_new_variables, 1);
	adicionarVariavel(sequence, v);
	counter_new_variables ++;
	renderAlgorithm();
}

function deleteVariable(which_function, which_variable) {
	programa.funcoes[which_function].variaveis.splice(which_variable, 1);
	renderAlgorithm();
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

function alternateBooleanVarVectorValue(parent_node, which_function, which_var, column_index) {
	programa.funcoes[which_function].variaveis[which_var].valor[column_index] = !programa.funcoes[which_function].variaveis[which_var].valor[column_index];
	renderAlgorithm();
}

function alternateBooleanVarMatrixValue(parent_node, which_function, which_var, row_index, column_index) {
	programa.funcoes[which_function].variaveis[which_var].valor[row_index][column_index] = !programa.funcoes[which_function].variaveis[which_var].valor[row_index][column_index];
	renderAlgorithm();
}

function alternateBooleanVarValue(parent_node, which_function, which_var) {
	programa.funcoes[which_function].variaveis[which_var].valor = !programa.funcoes[which_function].variaveis[which_var].valor;
	renderAlgorithm();
}

var opened_name_value_vector_variable = false;
var opened_input_value_vector_variable = null;
var sequence_name_opened_value_vector_variable;
var sequence_function_opened_value_vector_variable;
function enableVarVectorValueUpdate(parent_node, which_function, which_parameter, column_index) {
	if (opened_name_value_vector_variable) {
		$(opened_input_value_vector_variable).focus();
		return;
	}
	opened_name_value_vector_variable = true;
	sequence_name_opened_value_vector_variable = which_parameter;
	sequence_function_opened_value_vector_variable = which_function;

	$(parent_node).find('.span_value_variable').text('');

	if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.funcoes[which_function].variaveis[which_parameter].valor[column_index].toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	}

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_value_vector_variable = this;
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
			if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
				programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] = parseFloat($(this).val().trim());
			} else {

				if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.integer) {
					programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] = parseInt($(this).val().trim());
				} else {
					programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] = $(this).val().trim();
				}

			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_vector_variable = false;
		opened_input_value_vector_variable = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
					programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] = parseFloat($(this).val().trim());
				} else {

					if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.integer) {
						programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] = parseInt($(this).val().trim());
					} else {
						programa.funcoes[which_function].variaveis[which_parameter].valor[column_index] = $(this).val().trim();
					}

				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_vector_variable = false;
			opened_input_value_vector_variable = false;

			renderAlgorithm();
		}
		if(code == 27) {
			if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
				$(parent_node).find('.span_value_variable').text(programa.funcoes[which_function].variaveis[which_parameter].valor[column_index].toFixed(1));
			} else {
				$(parent_node).find('.span_value_variable').text(programa.funcoes[which_function].variaveis[which_parameter].valor[column_index]);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_vector_variable = false;
			opened_input_value_vector_variable = false;
		}
	});
}

var opened_name_value_matrix_variable = false;
var opened_input_value_matrix_variable = null;
var sequence_name_opened_value_matrix_variable;
var sequence_function_opened_value_matrix_variable;
function enableVarMatrixValueUpdate(parent_node, which_function, which_parameter, row_index, column_index) {
	if (opened_name_value_matrix_variable) {
		$(opened_input_value_matrix_variable).focus();
		return;
	}
	opened_name_value_matrix_variable = true;
	sequence_name_opened_value_matrix_variable = which_parameter;
	sequence_function_opened_value_matrix_variable = which_function;

	$(parent_node).find('.span_value_variable').text('');

	if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index].toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	}

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_value_matrix_variable = this;
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
			if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
				programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] = parseFloat($(this).val().trim());
			} else {

				if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.integer) {
					programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] = parseInt($(this).val().trim());
				} else {
					programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] = $(this).val().trim();
				}

			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_matrix_variable = false;
		opened_input_value_matrix_variable = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
					programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] = parseFloat($(this).val().trim());
				} else {

					if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.integer) {
						programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] = parseInt($(this).val().trim());
					} else {
						programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index] = $(this).val().trim();
					}

				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_matrix_variable = false;
			opened_input_value_matrix_variable = false;

			renderAlgorithm();
		}
		if(code == 27) {
			if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
				$(parent_node).find('.span_value_variable').text(programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index].toFixed(1));
			} else {
				$(parent_node).find('.span_value_variable').text(programa.funcoes[which_function].variaveis[which_parameter].valor[row_index][column_index]);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_matrix_variable = false;
			opened_input_value_matrix_variable = false;
		}
	});
}

var opened_name_value_variable = false;
var opened_input_value_variable = null;
var sequence_name_opened_value_variable;
var sequence_function_opened_value_variable;
function enableVarValueUpdate(parent_node, which_function, which_parameter) {
	if (opened_name_value_variable) {
		$(opened_input_value_variable).focus();
		return;
	}
	opened_name_value_variable = true;
	sequence_name_opened_value_variable = which_parameter;
	sequence_function_opened_value_variable = which_function;

	$(parent_node).find('.span_value_variable').text('');
	if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.funcoes[which_function].variaveis[which_parameter].valor.toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.funcoes[which_function].variaveis[which_parameter].valor + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	}

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_value_variable = this;
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
			if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
				programa.funcoes[which_function].variaveis[which_parameter].valor = parseFloat($(this).val().trim());
			} else{
				if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.integer) {
					programa.funcoes[which_function].variaveis[which_parameter].valor = parseInt($(this).val().trim());
				} else {
					programa.funcoes[which_function].variaveis[which_parameter].valor = $(this).val().trim();
				}
				
			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_variable = false;
		opened_input_value_variable = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
					programa.funcoes[which_function].variaveis[which_parameter].valor = parseFloat($(this).val().trim());
				} else{
					if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.integer) {
						programa.funcoes[which_function].variaveis[which_parameter].valor = parseInt($(this).val().trim());
					} else {
						programa.funcoes[which_function].variaveis[which_parameter].valor = $(this).val().trim();
					}
					
				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_variable = false;
			opened_input_value_variable = false;

			renderAlgorithm();
		}
		if(code == 27) {
			if (programa.funcoes[which_function].variaveis[which_parameter].tipo == tiposDados.real) {
				$(parent_node).find('.span_value_variable').text(programa.funcoes[which_function].variaveis[which_parameter].valor.toFixed(1));
			} else{
				$(parent_node).find('.span_value_variable').text(programa.funcoes[which_function].variaveis[which_parameter].valor);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_variable = false;
			opened_input_value_variable = false;
		}
	});
}

var opened_name_variable = false;
var opened_input_variable = null;
var sequence_name_opened_variable;
var sequence_function_opened_variable;
function enableNameVariableUpdate(parent_node, which_function, which_parameter) {
	if (opened_name_variable) {
		$(opened_input_variable).focus();
		return;
	}
	opened_name_variable = true;
	sequence_name_opened_variable = which_parameter;
	sequence_function_opened_variable = which_function;

	$(parent_node).find('.span_name_variable').text('');
	$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
		+ programa.funcoes[which_function].variaveis[which_parameter].nome + "' />" ).insertBefore($(parent_node).find('.span_name_variable'));

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_variable = this;
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
			programa.funcoes[which_function].variaveis[which_parameter].nome = $(this).val().trim();
		}
		$(this).remove();

		/// update elements:
		opened_name_variable = false;
		opened_input_variable = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				programa.funcoes[which_function].variaveis[which_parameter].nome = $(this).val().trim();
			}
			$(this).remove();

			/// update elements:
			opened_name_variable = false;
			opened_input_variable = false;

			renderAlgorithm();
		}
		if(code == 27) {
			$(parent_node).find('.span_name_variable').text(programa.funcoes[which_function].variaveis[which_parameter].nome);

			$(this).remove();

			/// update elements:
			opened_name_variable = false;
			opened_input_variable = false;
		}
	});

}

var opened_name_comment = false;
var opened_input_comment = null;
var sequence_name_opened_comment;
var sequence_function_opened_comment;
function enableCommentUpdate(parent_node, function_index, is_function_comment, comment_index) {
	if (opened_name_comment) {
		$(opened_input_comment).focus();
		return;
	}
	opened_name_comment = true;
	sequence_name_opened_comment = comment_index;
	sequence_function_opened_comment = function_index;

	$(parent_node).find('.span_comment_text').text('');
	
	var temp_value = "";

	if (is_function_comment) {
		temp_value = programa.funcoes[function_index].comentario_funcao.texto_comentario;
	} else {
		temp_value = programa.funcoes[function_index].comandos[comment_index].texto_comentario;
	}
	$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+temp_value+"' />" ).insertBefore($(parent_node).find('.span_comment_text'));

	$('.width-dynamic').on('input', function() {
	    var inputWidth = $(this).textWidth()+10;
	    opened_input_comment = this;
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

			var n_value = $(this).val().trim();
			if (is_function_comment) {
				programa.funcoes[function_index].comentario_funcao.texto_comentario = n_value;
			} else {
				temp_value = programa.funcoes[function_index].comandos[comment_index].texto_comentario = n_value;
			}
		}
		$(this).remove();

		/// update elements:
		opened_name_comment = false;
		opened_input_comment = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				var n_value = $(this).val().trim();
				if (is_function_comment) {
					programa.funcoes[function_index].comentario_funcao.texto_comentario = n_value;
				} else {
					temp_value = programa.funcoes[function_index].comandos[comment_index].texto_comentario = n_value;
				}
			}
			$(this).remove();

			/// update elements:
			opened_name_comment = false;
			opened_input_comment = false;

			renderAlgorithm();
		}
		if(code == 27) {
			$(parent_node).find('.span_comment_text').text(temp_value);

			$(this).remove();

			/// update elements:
			opened_name_comment = false;
			opened_input_comment = false;
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
	var appender = '<div class="ui secondary segment function_div list-group-item">';

	if (function_obj.comentario_funcao) {
		appender += renderComment(function_obj.comentario_funcao, sequence, true, -1);
	}
		
	appender += '<span class="glyphicon glyphicon-move move_function" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>';

	appender += (!function_obj.eh_principal ? '<button class="ui icon button large remove_function_button" onclick="removeFunctionHandler(this.parentNode, '+sequence+')"><i class="red icon times"></i></button>' : '<div class="div_start_minimize_v"> </div>')
		+ '<button class="ui icon button tiny minimize_function_button" onclick="minimizeFunctionHandler(this.parentNode, '+sequence+')"><i class="icon window minimize"></i></button>';

	appender += '<div class="ui icon buttons add_var_top_button"><div class="ui icon button" onclick="addVariable('+sequence+')"><i class="icon superscript"></i></div><div class="ui icon button"><i class="icon code"></i></div></div>';

	appender += '<div class="function_signature_div">'+i18n('function')+' ';


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

		+ '<div class="ui top attached segment variables_list_div">'
		+ renderVariables(function_obj, sequence)
		+ '</div>'
		+ '<div class="ui bottom attached segment commands_list_div"></div>'		

		+ '<div class="function_close_div">}</div>'
		+ '</div>'
		+ '</div>';

	$('.all_functions').append(appender); 
}


function renderComment(comment_obj, function_index, is_function_comment, comment_index) {
	var ret = '';
	ret += '<div class="ui comment"> <i class="ui icon small quote left"></i> <span class="span_comment_text" ondblclick="enableCommentUpdate(this.parentNode, '+function_index+', '
		+is_function_comment+', '+comment_index+')"> ' + comment_obj.texto_comentario + ' </span>';
	ret += '</div>';

	return ret;
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
		  		if (i == (Object.keys(tiposDados).length)) { break; }

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


// Essa função imprime as variáveis e os recursos para sua manipulação
function renderVariables(function_obj, sequence) {
	var ret = "";
	if (function_obj.variaveis != null) {

		for (var j = 0; j < function_obj.variaveis.length; j++) {

			var par_temp = function_obj.variaveis[j];

			ret += '<div class="ui label name_variable"><span class="span_name_variable" ondblclick="enableNameVariableUpdate(this.parentNode, '+sequence+', '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameVariableUpdate(this.parentNode, '+sequence+', '+j+')"></i>';

			ret += '<div class="ui dropdown variable_type seq_'+j+' fun_'+sequence+'">';
  	
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
		  		if (i == (Object.keys(tiposDados).length)) { break; }

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
			      	+  '</div>'
			    	+ '</div>';	
		  	}

    		ret += '</div></div>  = ';

    		if (par_temp.dimensoes == 0) {
    			if (par_temp.tipo == tiposDados.real) {
    				ret += '<div class="div_valor_var"><span class="span_value_variable" ondblclick="enableVarValueUpdate(this.parentNode, '+sequence+', '+j+')" >'+par_temp.valor.toFixed(1)+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableVarValueUpdate(this.parentNode, '+sequence+', '+j+')"></i></div> ';
    			} else {
    				if (par_temp.tipo == tiposDados.boolean) {
	    				ret += '<div class="div_valor_var"><span class="span_value_variable" ondblclick="alternateBooleanVarValue(this.parentNode, '+sequence+', '+j+')" >'+par_temp.valor+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="alternateBooleanVarValue(this.parentNode, '+sequence+', '+j+')"></i></div> ';
	    			} else {
    					ret += '<div class="div_valor_var"><span class="span_value_variable" ondblclick="enableVarValueUpdate(this.parentNode, '+sequence+', '+j+')" >'+par_temp.valor+'</span> <i class="icon small pencil alternate enable_edit_name_function" onclick="enableVarValueUpdate(this.parentNode, '+sequence+', '+j+')"></i></div> ';
	    			}
    			}
    		} else {
    			ret += '<table class="tabela_var">';

    			if (par_temp.dimensoes == 1) {
    				ret += '<tr>';
    				if (par_temp.tipo == tiposDados.real) {
    					for (var k = 0; k < par_temp.colunas; k++) {
	    					ret += '<td><span class="span_value_variable" ondblclick="enableVarVectorValueUpdate(this.parentNode, '+sequence+', '+j+', '+k+')" >'+par_temp.valor[k].toFixed(1)+'</span>'+'</td>';
	    				}
    				} else {
    					for (var k = 0; k < par_temp.colunas; k++) {
    						if (par_temp.tipo == tiposDados.boolean) {
    							ret += '<td><span class="span_value_variable" ondblclick="alternateBooleanVarVectorValue(this.parentNode, '+sequence+', '+j+', '+k+')" >'+par_temp.valor[k]+'</span>'+'</td>';
    						} else {
    							ret += '<td><span class="span_value_variable" ondblclick="enableVarVectorValueUpdate(this.parentNode, '+sequence+', '+j+', '+k+')" >'+par_temp.valor[k]+'</span>'+'</td>';
    						}
    					}
    				}
    				
    				ret += '</tr>';
    				ret += '</table>';

    				ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline" onclick="removeColumnVector('+sequence+', '+j+')"></i>'
    			    	+ ' <i class="ui icon plus square outline" onclick="addColumnVector('+sequence+', '+j+')"></i></div>';
    			}

    			if (par_temp.dimensoes == 2) {
    				if (par_temp.tipo == tiposDados.real) {
    					for (var l = 0; l < par_temp.linhas; l++) {
		    				ret += '<tr>';
		    				for (var k = 0; k < par_temp.colunas; k++) {
		    					ret += '<td><span class="span_value_variable" ondblclick="enableVarMatrixValueUpdate(this.parentNode, '+sequence+', '+j+', '+l+', '+k+')" >'+par_temp.valor[l][k].toFixed(1)+'</span>'+'</td>';
		    				} 
		    				ret += '</tr>';
	    				}
    				} else {
    					for (var l = 0; l < par_temp.linhas; l++) {
		    				ret += '<tr>';
		    				for (var k = 0; k < par_temp.colunas; k++) {
		    					if (par_temp.tipo == tiposDados.boolean) { 
		    						ret += '<td><span class="span_value_variable" ondblclick="alternateBooleanVarMatrixValue(this.parentNode, '+sequence+', '+j+', '+l+', '+k+')" >'+par_temp.valor[l][k]+'</span>'+'</td>';
		    					} else {
		    						ret += '<td><span class="span_value_variable" ondblclick="enableVarMatrixValueUpdate(this.parentNode, '+sequence+', '+j+', '+l+', '+k+')" >'+par_temp.valor[l][k]+'</span>'+'</td>';
		    					}
		    				} 
		    				ret += '</tr>';
	    				}
    				}
    				if (par_temp.linhas == 0) {
    					ret += '<tr><td></td></tr>';
    				}
    				ret += '<tr><td colspan="'+par_temp.colunas+'" class="tr_manage_lines"><i class="ui icon minus square outline" onclick="removeLineMatrix('+sequence+', '+j+')"></i>'
    			    	+ ' <i class="ui icon plus square outline" onclick="addLineMatrix('+sequence+', '+j+')"></i></td></tr>';
    				ret += '</table>';

    				ret += '<div class="buttons_manage_columns"><i class="ui icon minus square outline" onclick="removeColumnMatrix('+sequence+', '+j+')"></i>'
    			    	+ ' <i class="ui icon plus square outline" onclick="addColumnMatrix('+sequence+', '+j+')"></i></div>';
    			}

    			
    		}




			ret += ' <i class="red icon times remove_parameter" onclick="deleteVariable('+sequence+', '+j+')"></i></div>';

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

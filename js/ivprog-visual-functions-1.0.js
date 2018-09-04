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
	//console.log("vai chamar.. "  +  programa.funcoes.length);

	for (var i = 0; i < programa.funcoes.length; i++) {
		//console.log("deve chamar: " + i);
		appendFunction(programa.funcoes[i], i);
	}

	$('.list_globals').empty();
	if (programa.globais.length > 0) {
		renderGlobals();
	}

	$('.data_types_dropdown').dropdown();
	
	$('.parameter_data_types_dropdown').dropdown();

	addHandlers();
	associateObjects();
}

function associateObjects() {
	$( "div" ).each(function( index ) { 
		if (typeof $(this).data('idcommand') !== 'undefined') {
			this.relatedObj = allCommandsReference[$(this).data('idcommand')];
		}
	});
}

function deleteGlobal(which_global) {
	updateReferencesToVarBeforeRemove(programa.globais[which_global]);

	programa.globais.splice(which_global, 1);
	renderAlgorithm();
}

function alternateGlobalConst(which_global) {
	programa.globais[which_global].eh_constante = !programa.globais[which_global].eh_constante;
	renderAlgorithm();
}

function renderGlobals() {
	var ret = "";
	if (programa.globais.length > 0) {

		for (var j = 0; j < programa.globais.length; j++) {

			var par_temp = programa.globais[j];

			ret += '<div class="ui label name_variable"><div class="global_const">const: ';

			ret += '<i class="ui icon toggle '+(par_temp.eh_constante?"on":"off")+' " onclick="alternateGlobalConst('+j+')"></i></div><span class="span_name_variable" ondblclick="enableNameGlobalUpdate(this.parentNode, '+j+')">'+par_temp.nome+'</span> <i class="icon small pencil alternate enable_edit_name_parameter" onclick="enableNameGlobalUpdate(this.parentNode, '+j+')"></i>';

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

var has_element_created_draged = false;
var which_element_is_draged = null;

function manageCommand(event) {
	$( ".created_element" ).each(function( index ) { 
		$(this).remove();
	});

	var el = document.elementFromPoint(event.clientX, event.clientY);

	// Primeiro verificar se ele soltou no espaço da função correta:
	var hier = $(el).parentsUntil(".all_functions");
	var esta_correto = false;
	var esta_na_div_correta = false;
	if ($(el).hasClass("commands_list_div")) {
		esta_na_div_correta = true;
	} 
	for (i = 0; i < hier.length; i++) {
		if ($(hier[i]).hasClass("commands_list_div")) {
			esta_na_div_correta = true;
		} 
		if ($(hier[i]).data('fun') == function_to_add) {
			esta_correto = true;
			break;
		}
	}
	if (!esta_correto) {
		has_element_created_draged = false;
		which_element_is_draged = null;
		function_to_add = -1;
		return;
	} else {
		if (!esta_na_div_correta) {
			has_element_created_draged = false;
			which_element_is_draged = null;
			function_to_add = -1;
			return;
		}
	}

	// Agora é descobrir qual o escopo para adicionar o comando:

	// Se o elemento clicado possuir o atributo "fun", então, é direto na div dos comandos:
	if (typeof $(el).data('fun') !== 'undefined') {

		// Se a lista de comandos estiver vazia, então é o primeiro.
		// Portanto, ele deve soltar o elemento obrigatoriamente no objeto vazio
		if ((el.relatedObj.comandos == null)  || (el.relatedObj.comandos.length == 0)) {
				// pode adicionar 
				el.relatedObj.comandos = [];

				el.relatedObj.comandos.push(createElementGenericFunction());
			
		} else { // Entra nesse else, caso já existam outros comandos no bloco:

			findNearbyCommandToAddInFunctionScope(el, event);
		}

	} else { // Se entrar nesse bloco 'else', quer dizer que o usuário não soltou o elemento necessariamente na div específica da função
			 // portanto, devemos procurar nos elementos DOM, em que lugar da função, ele soltou o comando

		/*var hier = $(el).parentsUntil(".all_functions");
		for (i = 0; i < hier.length; i++) {
			if ($(hier[i]).data('fun') == function_to_add) {

				programa.funcoes[function_to_add].comandos.push(createElementGenericFunction());

				break;
			}
		}*/

		//findPositionAndInsertCommand(el, event);
		var caminho = findPositionAndPathToElementTarget(el, event);

		console.log("soltou sobre o seguinte elemento: ");

		console.log(caminho);

		console.log("soltou sobre o seguinte DOM: ");

		console.log(el);

		// se for 1, então está no nível do corpo da função:
		if (caminho.length == 1) {

			console.log("o caminho é de tamanho 1 e o objeto é o seguinte: " + caminho[0]);
			console.log(programa.funcoes[function_to_add].comandos[caminho[0]]);

			// se for do tipo true ou false, temos que determinar se soltou no if ou no else: 
			if (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.iftrue) {

				if ($(el).data('if')) {

					if ((programa.funcoes[function_to_add].comandos[caminho[0]].commands_block == null) 
						|| (programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.length == 0)) {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block = [];
						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					} else {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					}
				} else if ($(el).data('else')) {

					if ((programa.funcoes[function_to_add].comandos[caminho[0]].commands_else == null) 
						|| (programa.funcoes[function_to_add].comandos[caminho[0]].commands_else.length == 0)) {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_else = [];
						programa.funcoes[function_to_add].comandos[caminho[0]].commands_else.push(createElementGenericFunction());

					} else {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_else.push(createElementGenericFunction());

					}
				} else {
					console.log("soltou dentro do if, fora dos divs corretos... VERIFICAR QUAL ESTÁ MAIS PRÓXIMO... O IF OU O ELSE  --- NNN11");
					discoveryIfOrElse(el, event);
				}

			} else {

				if ((programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.repeatNtimes)
					|| (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.whiletrue) 
					|| (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.dowhiletrue) 
					|| (programa.funcoes[function_to_add].comandos[caminho[0]].tipo == tiposComandos.switch) ) {
					
					if ((programa.funcoes[function_to_add].comandos[caminho[0]].commands_block == null) 
						|| (programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.length == 0)) {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block = [];
						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					} else {

						programa.funcoes[function_to_add].comandos[caminho[0]].commands_block.push(createElementGenericFunction());

					}
				} else {
				
					var result = getBeforeOrAfterOrEndAllocate(el, event);
					if (result == true) {
						console.log("adicionando ANTES");
						programa.funcoes[function_to_add].comandos.splice(caminho[0], 0, createElementGenericFunction());
					} else {
						console.log("adicionando DEPOIS");
						programa.funcoes[function_to_add].comandos.splice(caminho[0] + 1, 0, createElementGenericFunction());
					}
				}
			}

			

		} else { // caso seja mais de um, o caminho, então, precisamos percorrer até achar: 

			// CONTINUAR DAQUI: 
			console.log("ACHO QUE É A SITUAÇÃO DE BLOCO INTERNO");
			console.log("SOLTOU NO ELEMENTO A SEGUIR: ");
			console.log(el.relatedObj);
			console.log("PAI DO ELEMENTO QUE ELA SOLTOU: ");
			console.log(el.parentNode.relatedObj);
			//

			if ((el.parentNode.relatedObj.tipo == tiposComandos.iftrue)) {

				if ($(el.parentNode).data('if') || $(el).data('if')) {

					if ((el.parentNode.relatedObj.commands_block == null) 
						|| (el.parentNode.relatedObj.commands_block.length == 0)) {

						el.parentNode.relatedObj.commands_block = [];
						el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());

					} else {

						if ($(el).data('if')) {
							// Descobrir qual o elemento mais próximo de onde ele soltou o comando recém criado:
							console.log("SITUAÇÃO TRATADA NO K1!");
							getNearbyIndexOfElementOnClick(el, event);

						} else {
							if (getBeforeOrAfterOrEndAllocate(el, event)) {
								console.log("K1 ANTECAO! SOLTOU ANTES DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index'), 0, createElementGenericFunction());
							} else {
								console.log("K1 ANTECAO! SOLTOU DEPOIS DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index') + 1, 0, createElementGenericFunction());
							}
						}

						//el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());

					}
				} else if ($(el.parentNode).data('else') || $(el).data('else')) {

					if ((el.parentNode.relatedObj.commands_else == null) 
						|| (el.parentNode.relatedObj.commands_else.length == 0)) {

						el.parentNode.relatedObj.commands_else = [];
						el.parentNode.relatedObj.commands_else.push(createElementGenericFunction());

					} else {

						if ($(el).data('else')) {
							// Descobrir qual o elemento mais próximo de onde ele soltou o comando recém criado:
							console.log("SITUAÇÃO TRATADA NO K2!");
							getNearbyIndexOfElementOnClick(el, event);

						} else {

							if (getBeforeOrAfterOrEndAllocate(el, event)) {
								console.log("K2 ANTECAO! SOLTOU ANTES DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_else.splice($(el).data('index'), 0, createElementGenericFunction());

							} else {
								console.log("K2 ANTECAO! SOLTOU DEPOIS DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_else.splice($(el).data('index') + 1, 0, createElementGenericFunction());
							}
						}

						//el.parentNode.relatedObj.commands_else.push(createElementGenericFunction());

					}
				} else {
					console.log("soltou dentro do if, fora dos divs corretos... VERIFICAR QUAL ESTÁ MAIS PRÓXIMO... O IF OU O ELSE  --- NNN22");
					discoveryIfOrElse(el, event);
				}

			} else {
				console.log("COMEÇAR A TRATAR!...");

				if ((el.parentNode.relatedObj.tipo == tiposComandos.repeatNtimes)
					|| (el.parentNode.relatedObj.tipo == tiposComandos.whiletrue) 
					|| (el.parentNode.relatedObj.tipo == tiposComandos.dowhiletrue) 
					|| (el.parentNode.relatedObj.tipo == tiposComandos.switch) 
					|| (el.parentNode.relatedObj.tipo == tiposComandos.iftrue)) {


					if ((el.parentNode.relatedObj.commands_block == null) 
						|| (el.parentNode.relatedObj.commands_block.length == 0)) {
					
						el.parentNode.relatedObj.commands_block = [];
						el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());

					} else {

						if (typeof $(el).data('subblock') !== 'undefined') {
							
							console.log("SITUAÇÃO TRATADA NO K3!");

							getNearbyIndexOfElementOnClick(el, event);

						} else {
							if (getBeforeOrAfterOrEndAllocate(el, event)) {
								console.log("K3 ANTECAO! SOLTOU ANTES DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index'), 0, createElementGenericFunction());
							} else {
								console.log("K3 ANTECAO! SOLTOU DEPOIS DO ELEMENTO ALVO");

								el.parentNode.relatedObj.commands_block.splice($(el).data('index') + 1, 0, createElementGenericFunction());
							}
						}

						//el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());
					}


				} else {
					console.log("AGORA SIM! SITUAÇÃO K4!");
					console.log("VOU ADICIONAR NO SEGINTE ELEMENTO: ");
					console.log(el.parentNode.parentNode.relatedObj);

					if (getBeforeOrAfterOrEndAllocate(el.parentNode, event)) {
						el.parentNode.parentNode.relatedObj.commands_block.splice($(el.parentNode).data('index'), 0, createElementGenericFunction());
					} else {
						el.parentNode.parentNode.relatedObj.commands_block.splice($(el.parentNode).data('index') + 1, 0, createElementGenericFunction());
					}
				}

			}
			

			/*console.log("elemento superior: ");
			console.log(programa.funcoes[function_to_add].comandos[caminho[0]]);
			console.log("elemento específico: 
			console.log(findElementByPath(caminho));*/

		}

	}


	//console.log("onde soltou:");
	//console.log(el);

	
	has_element_created_draged = false;
	which_element_is_draged = null;
	function_to_add = -1;

	renderAlgorithm();

}

function discoveryIfOrElse(el, event) {

	var menor_distancia_acima = 999999999;
	var menor_distancia_abaixo = 999999999;
	var elemento_menor_distancia_acima = null;
	var elemento_menor_distancia_abaixo = null;
	var antes = true;

	var t_bot;
	var t_top;

	if ($(el.parentNode).children(".block_commands").length == 0) {
		

		$(el).children(".block_commands").each(function( index ) { 
			t_top = this.getBoundingClientRect().top;
			t_bot = this.getBoundingClientRect().top + this.getBoundingClientRect().height;

			if ((t_top - event.clientY) < menor_distancia_acima) {
				menor_distancia_acima = event.clientY - t_top;
				elemento_menor_distancia_acima = this;
			}

			if ((event.clientY - t_bot) < menor_distancia_abaixo) {
				menor_distancia_abaixo = event.clientY - t_bot;
				elemento_menor_distancia_abaixo = this;
			}		


		});

		if (elemento_menor_distancia_abaixo == null && elemento_menor_distancia_acima == null) {
			return;
		}

		if (menor_distancia_acima > menor_distancia_abaixo) {
			// quer adicionar na parte de cima
			if (typeof $(elemento_menor_distancia_acima).data('if') !== 'undefined') {
				el.relatedObj.commands_block.splice(0, 0, createElementGenericFunction());
			} else {
				el.relatedObj.commands_else.splice(0, 0, createElementGenericFunction());
			}
		} else {
			// quer adicionar na parte de baixo
			if (typeof $(elemento_menor_distancia_acima).data('if') !== 'undefined') {
				el.relatedObj.commands_block.push(createElementGenericFunction());
			} else {
				el.relatedObj.commands_else.push(createElementGenericFunction());
			}
		}

	} else {

		$(el.parentNode).children(".block_commands").each(function( index ) { 
			t_top = this.getBoundingClientRect().top;
			t_bot = this.getBoundingClientRect().top + this.getBoundingClientRect().height;

			if ((t_top - event.clientY) < menor_distancia_acima) {
				menor_distancia_acima = event.clientY - t_top;
				elemento_menor_distancia_acima = this;
			}

			if ((event.clientY - t_bot) < menor_distancia_abaixo) {
				menor_distancia_abaixo = event.clientY - t_bot;
				elemento_menor_distancia_abaixo = this;
			}		


		});

		if (elemento_menor_distancia_abaixo == null && elemento_menor_distancia_acima == null) {
			return;
		}

		if (menor_distancia_acima > menor_distancia_abaixo) {
			// quer adicionar na parte de cima
			if (typeof $(elemento_menor_distancia_acima).data('if') !== 'undefined') {
				el.parentNode.relatedObj.commands_block.splice(0, 0, createElementGenericFunction());
			} else {
				el.parentNode.relatedObj.commands_else.splice(0, 0, createElementGenericFunction());
			}
		} else {
			// quer adicionar na parte de baixo
			if (typeof $(elemento_menor_distancia_acima).data('if') !== 'undefined') {
				el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());
			} else {
				el.parentNode.relatedObj.commands_else.push(createElementGenericFunction());
			}
		}
	}

}

function getNearbyIndexOfElementOnClick(el, event) {

	var all_sub = $(el).find('div');

	var menor_distancia = 999999999;
	var elemento_menor_distancia = null;
	var antes = true;

	var t_bot;
	var t_top;
	// Descobrindo o elemento mais próximo:
	for (i = 0; i < all_sub.length; i++) {
		
		t_top = all_sub[i].getBoundingClientRect().top;
		t_bot = all_sub[i].getBoundingClientRect().top + all_sub[i].getBoundingClientRect().height;

		if ((t_top - event.clientY) < menor_distancia) {
			menor_distancia = event.clientY - t_top;
			elemento_menor_distancia = all_sub[i];
		}
	}

	borda_inferior = elemento_menor_distancia.parentNode.getBoundingClientRect().top + elemento_menor_distancia.parentNode.getBoundingClientRect().height;
	
	// Está mais próximo da borda de baixo, ou seja.. inserir por último:
	if ((borda_inferior - event.clientY) < menor_distancia) {

		if ((el.parentNode.relatedObj.tipo == tiposComandos.iftrue)) {
			if ($(el).data('else')) {
				el.parentNode.relatedObj.commands_else.push(createElementGenericFunction());
				return;
			}
		}

		el.parentNode.relatedObj.commands_block.push(createElementGenericFunction());
	} else {

		if ((el.parentNode.relatedObj.tipo == tiposComandos.iftrue)) {
			if ($(el).data('else')) {
				el.parentNode.relatedObj.commands_else.splice($(elemento_menor_distancia).data('index'), 0, createElementGenericFunction());
				return;
			}
		}

		el.parentNode.relatedObj.commands_block.splice($(elemento_menor_distancia).data('index'), 0, createElementGenericFunction());

	}
}

function findElementByPath(full_path_array) {

	var root_el = programa.funcoes[function_to_add].comandos[full_path_array[0]];
	for (i = 1; i < full_path_array.length; i++) {
		root_el = auxiliaryFindElement(root_el, full_path_array[i]);
	}
	return root_el;
}

function auxiliaryFindElement(element, index) {
	console.log("entrou: " + element.tipo);
	console.log("indice: " + index);
	console.log("desse indice: " + element.commands_block[index]);
	return element.commands_block[index];
}

function findPositionAndPathToElementTarget(el, event) {

	var full_path = [];
	var m;

	if (typeof $(el).data('fullpath') !== 'undefined') {
		m = $(el).data('fullpath');
	} else {
		
		var hier = $(el).parentsUntil(".all_functions");
		for (i = 0; i < hier.length; i++) {
			if (typeof $(hier[i]).data('fullpath') !== 'undefined') {
				m = $(hier[i]).data('fullpath');
				break;
			}
		}

	}

	if (isNaN(m)) {
		full_path = m.split(',');
		/*for (i = 0; i < full_path.length; i++) {
			full_path[i] = parseInt(full_path[i]);
		}*/
		return full_path;
	} else {
		return [m];
	}

}

// Função apenas para o caso de soltar elemento no corpo da função:
function findNearbyCommandToAddInFunctionScope(el, event) {

	var all_sub = $('#function_drag_cmd_' + function_to_add).find('div');

	var menor_distancia = 999999999;
	var elemento_menor_distancia = null;
	var antes = true;

	var t_bot;
	var t_top;
	// Descobrindo o elemento mais próximo:
	for (i = 0; i < all_sub.length; i++) {
		
		t_top = all_sub[i].getBoundingClientRect().top;
		t_bot = all_sub[i].getBoundingClientRect().top + all_sub[i].getBoundingClientRect().height;

		if ((t_top - event.clientY) < menor_distancia) {
			menor_distancia = event.clientY - t_top;
			elemento_menor_distancia = all_sub[i];
		}
	}

	borda_inferior = elemento_menor_distancia.parentNode.getBoundingClientRect().top + elemento_menor_distancia.parentNode.getBoundingClientRect().height;
	
	// Está mais próximo da borda de baixo, ou seja.. inserir por último:
	if ((borda_inferior - event.clientY) < menor_distancia) {
		programa.funcoes[function_to_add].comandos.push(createElementGenericFunction());
	} else {
		programa.funcoes[function_to_add].comandos.splice($(elemento_menor_distancia).data('index'), 0, createElementGenericFunction());
	}
}


function getBeforeOrAfterOrEndAllocate(el, event) {

	var m;
	if (typeof $(el).data('fullpath') !== 'undefined') {
		m = el;
	} else {
		
		var hier = $(el).parentsUntil(".all_functions");
		for (i = 0; i < hier.length; i++) {
			if (typeof $(hier[i]).data('fullpath') !== 'undefined') {
				m = hier[i];
				break;
			}
		}

	}

	// primeiro: descobrir se ele soltou para adicionar antes ou depois:
	var metade_componente = m.getBoundingClientRect().top + (m.getBoundingClientRect().height / 2);
	var antes = false;
	if (event.clientY < metade_componente) {
		antes = true;
	}

	return antes;

}

function findPositionAndInsertCommand(el, event) {

	var identificado_local = false;
	if ($(el).data('command') >= 0) {
		console.log("soltou em cima de um command: ");

		// primeiro: descobrir se ele soltou para adicionar antes ou depois:
		var metade_componente = el.getBoundingClientRect().top + (el.getBoundingClientRect().height / 2);
		var antes = false;
		if (event.clientY < metade_componente) {
			antes = true;
		}

		// segundo: descobrir o contexto que está sendo inserido o comando:
		// se o subblock for diferente 0, então, ele não está inserido em um sub-bloco
		if ($(el.parentNode).data('subblock') >= 0) {
			console.log("ATENÇÃO! soltou em cima de um command dentro de um subbloco");

			// se for do tipo if, precisamos descobrir se foi no if ou no else:
			if ($(el.parentNode).data('if')) {
				if (antes) {
					programa.funcoes[function_to_add].comandos[$(el).data('index')].commands_block.splice($(el).data('index'), 0, createElementGenericFunction());
				} else {
					programa.funcoes[function_to_add].comandos.splice($(el).data('index') + 1, 0, createElementGenericFunction());
				}

				
			} else { // se entrar aqui, ele soltou no else:
				
				var hier = $(el).parentsUntil(".all_functions");
				for (i = 0; i < hier.length; i++) {
					console.log("elemento índice: " + i);
					console.log(hier[i]);

					if ($(hier[i]).data('command') >= 0) {
						console.log("soltou em cima de um elemento dentro de um command!");
						identificado_local = true;
						break;
					}
				}

				addElementToIf("0", $(el.parentNode).data('index'), false);
			}


		} else {
			// vai adicionar no bloco da função 
			if (antes) {
				programa.funcoes[function_to_add].comandos.splice($(el).data('index'), 0, createElementGenericFunction());
			} else {
				programa.funcoes[function_to_add].comandos.splice($(el).data('index') + 1, 0, createElementGenericFunction());
			}
		}

		identificado_local = true;
	}

	// Soltou em cima de um bloco de comandos dentro do if, for, while...
	console.log("onde ele soltou: >>>> ");
	console.log(el);
	if ($(el).data('subblock') >= 0) {
		console.log("soltou dentro de um if, for, while...");

		// segundo: descobrir o contexto que está sendo inserido o comando:
		// se o data-parent for 0, então, ele não está inserido em um sub-bloco
		if ($(el.parentNode).data('parent') == "0") {
			// vai adicionar no bloco da função 
			console.log("vai adicionar....");
			// se for do tipo "if", então precisamos verificar se soltou no "if" ou no "else":
			if (programa.funcoes[function_to_add].comandos[$(el.parentNode).data('index')].tipo == tiposComandos.iftrue) {
				// se soltou no "if", então tem data-if
				if ($(el).data('if')) {
					console.log("PPPP2");
					addElementToIf("0", $(el.parentNode).data('index'), true);
				} else { // se entrar aqui, ele soltou no else:
					console.log("PPPP3");
					addElementToIf("0", $(el.parentNode).data('index'), false);
				}
			}

		} else { // caso exista mais informação no parent, então, deve-se descobrir a hierarquia

		}
	}

	if (identificado_local == false) {
		var hier = $(el).parentsUntil(".all_functions");
		for (i = 0; i < hier.length; i++) {
			console.log("elemento índice: " + i);
			console.log(hier[i]);

			if ($(hier[i]).data('command') >= 0) {
				console.log("soltou em cima de um elemento dentro de um command!");
				identificado_local = true;
				break;
			}
		}
	}

}

// o parent: para a posição na hierarquia, e se é dentro do corpo do if ou do else, se for true é if.
function addElementToIf(parent, if_index, is_in_if) {

	if (parent == "0") {
		// adicionar no bloco do if:
		if (is_in_if) {
			if ((programa.funcoes[function_to_add].comandos[if_index].commands_block == null)
				|| (programa.funcoes[function_to_add].comandos[if_index].commands_block.length == 0)) {

				programa.funcoes[function_to_add].comandos[if_index].commands_block = [];
			}
			programa.funcoes[function_to_add].comandos[if_index].commands_block.push(createElementGenericFunction());
		} else { // adicionar no bloco do else:
			if ((programa.funcoes[function_to_add].comandos[if_index].commands_else == null)
				|| (programa.funcoes[function_to_add].comandos[if_index].commands_else.length == 0)) {

				programa.funcoes[function_to_add].comandos[if_index].commands_else = [];
			}
			programa.funcoes[function_to_add].comandos[if_index].commands_else.push(createElementGenericFunction());
		}
	}
}


function createElementGenericFunction() {
	if (which_element_is_draged == tiposComandos.comment) {
		return new Comentario(i18n('text_comment'));
	}
	if (which_element_is_draged == tiposComandos.reader) {
		return new Leitura(null);
	}
	if (which_element_is_draged == tiposComandos.writer) {
		return new Escrita(null);
	}
	if (which_element_is_draged == tiposComandos.attribution) {
		return new Atribuicao(null, null);
	}
	if (which_element_is_draged == tiposComandos.iftrue) {
		return new SeVerdadeiro(null, null, null);
	}
	if (which_element_is_draged == tiposComandos.repeatNtimes) {
		return new RepitaNVezes(null, null, null, null);
	} 
	if (which_element_is_draged == tiposComandos.whiletrue) {
		return new EnquantoVerdadeiro(null, null);
	}
	if (which_element_is_draged == tiposComandos.dowhiletrue) {
		return new FacaEnquantoVerdadeiro(null, null);
	}
	if (which_element_is_draged == tiposComandos.switch) {
		return new Escolha(null, null);
	}
	if (which_element_is_draged == tiposComandos.functioncall) {
		return new ChamadaFuncao(null, null);
	}
}

function createWriterObject() {
	var ret = '';
	ret += '<div class="ui writer created_element" onclick="manageCommand(event)"> <i class="ui icon small upload"></i> <span> '+i18n('write')+' x </span>';
	ret += '</div>';

	return ret;
}

function createCommentDragObject() {
	var ret = '';
	ret += '<div class="ui comment created_element" onclick="manageCommand(event)"> <i class="ui icon small quote left"></i> <span class="span_comment_text" "> '+i18n('text_comment')+' </span>';
	ret += '</div>';

	return ret;
}

function createReaderObject() {
	var ret = '';
	ret += '<div class="ui reader created_element" onclick="manageCommand(event)"> <i class="ui icon small download"></i> <span> '+i18n('read')+' x </span>';
	ret += '</div>';

	return ret;
}

function createAttributionDragObject() {
	var ret = '';
	ret += '<div class="ui attribution created_element" onclick="manageCommand(event)"> <i class="ui icon small arrow left"></i> <span> x = 1 + 1 </span>';
	ret += '</div>';

	return ret;
}

function createIfTrueDragObject() {
	var ret = '';
	ret += '<div class="ui iftrue created_element" onclick="manageCommand(event)"> <i class="ui icon small random"></i> <span> if (x < 1) { } </span>';
	ret += '</div>';

	return ret;
}

function createRepeatNtimesDragObject() {
	var ret = '';
	ret += '<div class="ui repeatNtimes created_element" onclick="manageCommand(event)"> <i class="ui icon small sync"></i> <span> para (x = 0; x < 10; x ++) { } </span>';
	ret += '</div>';

	return ret;
}

function createWhileTrueDragObject() {
	var ret = '';
	ret += '<div class="ui whiletrue created_element" onclick="manageCommand(event)"> <i class="ui icon small sync"></i> <span> enquanto(x < 10) { } </span>';
	ret += '</div>';

	return ret;
}

function createDoWhileTrueDragObject() {
	var ret = '';
	ret += '<div class="ui dowhiletrue created_element" onclick="manageCommand(event)"> <i class="ui icon small sync"></i> <span> faça {<br>} enquanto(x < 10) </span>';
	ret += '</div>';

	return ret;
}

function createSwitchDragObject() {
	var ret = '';
	ret += '<div class="ui switch created_element" onclick="manageCommand(event)"> <i class="ui icon small random"></i> <span> escolha (x) { <br> caso 1: <br> caso 2: <br> } </span>';
	ret += '</div>';

	return ret;
}

function createFunctioncallDragObject() {
	var ret = '';
	ret += '<div class="ui functioncall created_element" onclick="manageCommand(event)"> <i class="hand point right icon"></i> <span> funcao() </span>';
	ret += '</div>';

	return ret;
}

var function_to_add = -1;

function addHandlers() {

	
	$('.create_functioncall').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.functioncall;

		function_to_add = $(e.target).data('fun'); 

		var inner = $(createFunctioncallDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_switch').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.switch;

		function_to_add = $(e.target).data('fun');

		var inner = $(createSwitchDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_dowhiletrue').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.dowhiletrue;

		function_to_add = $(e.target).data('fun');

		var inner = $(createDoWhileTrueDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_whiletrue').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.whiletrue;

		function_to_add = $(e.target).data('fun');

		var inner = $(createWhileTrueDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_repeatNtimes').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.repeatNtimes;

		function_to_add = $(e.target).data('fun');

		var inner = $(createRepeatNtimesDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_iftrue').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.iftrue;

		function_to_add = $(e.target).data('fun');

		var inner = $(createIfTrueDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_comment').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.comment;

		function_to_add = $(e.target).data('fun');

		var inner = $(createCommentDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_attribution').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.attribution;

		function_to_add = $(e.target).data('fun');

		var inner = $(createAttributionDragObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_writer').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.writer;

		function_to_add = $(e.target).data('fun');

		var inner = $(createWriterObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	$('.create_reader').on('click', function(e){

		has_element_created_draged = true;
		which_element_is_draged = tiposComandos.reader;

		function_to_add = $(e.target).data('fun');

		var inner = $(createReaderObject()).draggable().appendTo("body");
	    
	    inner.css("position", "absolute");
	    e.type = "mousedown.draggable";
	    e.target = inner[0];
	    inner.css("left", e.pageX - 15);
	    inner.css("top", e.pageY - 15);
	    inner.trigger(e);
		
	});

	for (i = 0; i < programa.funcoes.length; i++) {
		var x_temp = '#function_drag_cmd_' + i + " .block_commands";
		$( x_temp ).each(function( index ) { 
			Sortable.create(this, {
			    handle: '.command_drag',
			    animation: 50,
			    ghostClass: 'ghost',
			    group: 'commands_inside_function_drag_' + i,
			    onEnd: function (evt) {
			      //updateSequenceFunctionHandler(evt.oldIndex, evt.newIndex);
			    },
			    onStart: function (evt) {
					console.log("começou");
				}
			});
		});

		Sortable.create(document.getElementById('function_drag_cmd_' + i), {
		    handle: '.command_drag',
		    animation: 50,
		    ghostClass: 'ghost',
		    group: 'commands_inside_function_drag_' + i,
		    onEnd: function (evt) {
		      //updateSequenceFunctionHandler(evt.oldIndex, evt.newIndex);
		    },
		    onStart: function (evt) {
				console.log("começou");
			}
		});
	}

	$('.dropdown.change_column_reader_render').dropdown({
		    onChange: function(value, text, $selectedItem) {
		    	editing_element_index_value = this.parentNode.relatedObj;
		    	if ($($selectedItem).data('value')) {
		    		
		    		$(this).find('.text').text(' ');
		    		var el;
		    		if (isNaN(editing_element_index_value.coluna)) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		} else {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="'+editing_element_index_value.coluna+'" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		}
		    		
		    		//$(this).find('.text').text();
		    		//$( el ).insertBefore($(this).find('.text'));
		    		$(this).find('.text').append(el);
		    		el.focus();
		    		//editing_element_index_value = element.relatedObj;


		    	} else {

		    		classList = $selectedItem.attr('class').split(/\s+/);
			    	var seq;
			    	var func;
					$.each(classList, function(index, item) {
					    if (item.indexOf("seq_") > -1) {
					        seq = item.split("seq_")[1];
					    }
					    if (item.indexOf("func_") > -1) {
					        func = item.split("func_")[1];
					    }
					});

					if ($($selectedItem).hasClass('local_vars')) {
						this.parentNode.relatedObj.coluna = programa.funcoes[func].variaveis[seq];
					}
					if ($($selectedItem).hasClass('parameters_vars')) {
					    this.parentNode.relatedObj.coluna = programa.funcoes[func].lista_parametros[seq];
					}
					if ($($selectedItem).hasClass('global_vars')) {
					 	this.parentNode.relatedObj.coluna = programa.globais[seq];
					}


		    	}

		    }
		});

	$('.dropdown.change_column_reader_render_matrix_column').dropdown({
		    onChange: function(value, text, $selectedItem) {
		    	editing_element_index_value = this.parentNode.relatedObj;
		    	if ($($selectedItem).data('value')) {
		    		
		    		$(this).find('.text').text(' ');
		    		var el;
		    		if (isNaN(editing_element_index_value.coluna)) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		} else if (editing_element_index_value.coluna == null) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		} else {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="'+editing_element_index_value.coluna+'" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		}
		    		
		    		//$(this).find('.text').text();
		    		//$( el ).insertBefore($(this).find('.text'));
		    		$(this).find('.text').append(el);
		    		el.focus();
		    		//editing_element_index_value = element.relatedObj;


		    	} else {

		    		classList = $selectedItem.attr('class').split(/\s+/);
			    	var seq;
			    	var func;
					$.each(classList, function(index, item) {
					    if (item.indexOf("seq_") > -1) {
					        seq = item.split("seq_")[1];
					    }
					    if (item.indexOf("func_") > -1) {
					        func = item.split("func_")[1];
					    }
					});

					if ($($selectedItem).hasClass('local_vars')) {
						this.parentNode.relatedObj.coluna = programa.funcoes[func].variaveis[seq];
					}
					if ($($selectedItem).hasClass('parameters_vars')) {
					    this.parentNode.relatedObj.coluna = programa.funcoes[func].lista_parametros[seq];
					}
					if ($($selectedItem).hasClass('global_vars')) {
					 	this.parentNode.relatedObj.coluna = programa.globais[seq];
					}


		    	}

		    }
		});


	$('.dropdown.change_column_reader_render_matrix_line').dropdown({
		    onChange: function(value, text, $selectedItem) {
		    	editing_element_index_value = this.parentNode.relatedObj;
		    	if ($($selectedItem).data('value')) {
		    		
		    		$(this).find('.text').text(' ');
		    		var el;
		    		if (isNaN(editing_element_index_value.linha)) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.linha = this.value; renderAlgorithm();" onblur="editing_element_index_value.linha = this.value; renderAlgorithm();"  />');
		    		} else if (editing_element_index_value.linha == null) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.linha = this.value; renderAlgorithm();" onblur="editing_element_index_value.linha = this.value; renderAlgorithm();"  />');
		    		} else {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="'+editing_element_index_value.linha+'" onChange="editing_element_index_value.linha = this.value; renderAlgorithm();" onblur="editing_element_index_value.linha = this.value; renderAlgorithm();"  />');
		    		}
		    		
		    		//$(this).find('.text').text();
		    		//$( el ).insertBefore($(this).find('.text'));
		    		$(this).find('.text').append(el);
		    		el.focus();
		    		//editing_element_index_value = element.relatedObj;


		    	} else {

		    		classList = $selectedItem.attr('class').split(/\s+/);
			    	var seq;
			    	var func;
					$.each(classList, function(index, item) {
					    if (item.indexOf("seq_") > -1) {
					        seq = item.split("seq_")[1];
					    }
					    if (item.indexOf("func_") > -1) {
					        func = item.split("func_")[1];
					    }
					});

					if ($($selectedItem).hasClass('local_vars')) {
						this.parentNode.relatedObj.linha = programa.funcoes[func].variaveis[seq];
					}
					if ($($selectedItem).hasClass('parameters_vars')) {
					    this.parentNode.relatedObj.linha = programa.funcoes[func].lista_parametros[seq];
					}
					if ($($selectedItem).hasClass('global_vars')) {
					 	this.parentNode.relatedObj.linha = programa.globais[seq];
					}


		    	}

		    }
		});


	$('.ui.dropdown.variable_reader')
    	.dropdown({
		    onChange: function(value, text, $selectedItem) {

		    	classList = $selectedItem.attr('class').split(/\s+/);
		    	var seq;
		    	var func;
				$.each(classList, function(index, item) {
				    if (item.indexOf("seq_") > -1) {
				        seq = item.split("seq_")[1];
				    }
				    if (item.indexOf("func_") > -1) {
				        func = item.split("func_")[1];
				    }
				});

				if ($($selectedItem).hasClass('local_vars')) {
					this.parentNode.relatedObj.variavel = programa.funcoes[func].variaveis[seq];
				}
				if ($($selectedItem).hasClass('parameters_vars')) {
				    this.parentNode.relatedObj.variavel = programa.funcoes[func].lista_parametros[seq];
				}
				if ($($selectedItem).hasClass('global_vars')) {
				 	this.parentNode.relatedObj.variavel = programa.globais[seq];
				}

				$(this.parentNode).find(".change_column_reader").remove();
				$(this.parentNode).find(".change_column_reader_render").remove();
				$(this.parentNode).find(".change_column_reader_render_matrix_line").remove();
				$(this.parentNode).find(".change_column_reader_render_matrix_column").remove();
				$(this.parentNode).find(".change_column_reader_render").remove();
				$(this.parentNode).find(".change_column_reader_render_matrix_line").remove();
				$(this.parentNode).find(".change_line_reader").remove();

				



				if (this.parentNode.relatedObj.variavel.dimensoes == 1) { // 
					this.parentNode.relatedObj.linha = null;
					this.parentNode.relatedObj.coluna = null;
					
					addOptionsReaderVector(this.parentNode, $(this.parentNode).data('fun'));

				} else if (this.parentNode.relatedObj.variavel.dimensoes == 2) { // 
					this.parentNode.relatedObj.linha = null;
					this.parentNode.relatedObj.coluna = null;
					
					addOptionsReaderMatrix(this.parentNode, $(this.parentNode).data('fun'));
				}


		    }
		})
  	;

	$('.ui.buttons .dropdown').dropdown();

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


var editing_element_index_value = null;
function  addOptionsReaderMatrix(element, function_index) {

	var ret;
	if (element.relatedObj.variavel.dimensoes == 2) {

		// LINHA: 
		var ret = ('<div class="ui dropdown change_line_reader"><span class="opened_index">[ </span> <div class="text"> </div> ]<i class="dropdown icon"></i><div class="menu"><div class="item" data-value="true">Valor</div><div class="item">Variável');
		ret += '<i class="dropdown icon"></i>'
	  		+ '<div class="menu func_'+function_index+'">';

		if (programa.funcoes[function_index].variaveis) {
			for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
				ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
	  		}
		}
	  	
	  	if (programa.funcoes[function_index].lista_parametros) {
	  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
	  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
	  		}
	  	}

	  	if (programa.globais) {
	  		for (var ij = 0; ij < programa.globais.length; ij++) {
	  			if (!programa.globais[ij].eh_constante) {
	  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
	  			}
	  		}
	  	}

	  	ret += '</div></div></div>';

		$( ret ).insertBefore($(element).find('.close_parentheses'));

		$('.dropdown.change_line_reader').dropdown({
		    onChange: function(value, text, $selectedItem) {

		    	if ($($selectedItem).data('value')) {
		    		var el;
		    		$(this).find('.text').text(' ');
		    		if (!isNaN(element.relatedObj.linha)) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.linha = this.value; renderAlgorithm();" onblur="editing_element_index_value.linha = this.value; renderAlgorithm();"  />');
		    		} else if (element.relatedObj.linha == null) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.linha = this.value; renderAlgorithm();" onblur="editing_element_index_value.linha = this.value; renderAlgorithm();"  />');
		    		} else {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="'+element.relatedObj.linha+'" onChange="editing_element_index_value.linha = this.value; renderAlgorithm();" onblur="editing_element_index_value.linha = this.value; renderAlgorithm();"  />');
		    		}
		    		
		    		//$(this).find('.text').text();
		    		//$( el ).insertBefore($(this).find('.text'));
		    		$(this).find('.text').append(el);
		    		el.focus();
		    		editing_element_index_value = element.relatedObj;


		    	} else {

		    		classList = $selectedItem.attr('class').split(/\s+/);
			    	var seq;
			    	var func;
					$.each(classList, function(index, item) {
					    if (item.indexOf("seq_") > -1) {
					        seq = item.split("seq_")[1];
					    }
					    if (item.indexOf("func_") > -1) {
					        func = item.split("func_")[1];
					    }
					});

					if ($($selectedItem).hasClass('local_vars')) {
						this.parentNode.relatedObj.linha = programa.funcoes[func].variaveis[seq];
					}
					if ($($selectedItem).hasClass('parameters_vars')) {
					    this.parentNode.relatedObj.linha = programa.funcoes[func].lista_parametros[seq];
					}
					if ($($selectedItem).hasClass('global_vars')) {
					 	this.parentNode.relatedObj.linha = programa.globais[seq];
					}


		    	}

		    }
		});

		// COLUNA: 
		var ret = ('<div class="ui dropdown change_column_reader"><span class="opened_index">[ </span> <div class="text"> </div> ]<i class="dropdown icon"></i><div class="menu"><div class="item" data-value="true">Valor</div><div class="item">Variável');
		ret += '<i class="dropdown icon"></i>'
	  		+ '<div class="menu func_'+function_index+'">';

		if (programa.funcoes[function_index].variaveis) {
			for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
				ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
	  		}
		}
	  	
	  	if (programa.funcoes[function_index].lista_parametros) {
	  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
	  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
	  		}
	  	}

	  	if (programa.globais) {
	  		for (var ij = 0; ij < programa.globais.length; ij++) {
	  			if (!programa.globais[ij].eh_constante) {
	  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
	  			}
	  		}
	  	}

	  	ret += '</div></div></div>';

		$( ret ).insertBefore($(element).find('.close_parentheses'));

		$('.dropdown.change_column_reader').dropdown({
		    onChange: function(value, text, $selectedItem) {
		    	console.log('QQQ2');

		    	if ($($selectedItem).data('value')) {
		    		var el;
		    		$(this).find('.text').text(' ');

		    		if (!isNaN(element.relatedObj.coluna)) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		} else if (element.relatedObj.coluna == null) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		} else {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="'+element.relatedObj.coluna+'" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		}
		    		
		    		//$(this).find('.text').text();
		    		//$( el ).insertBefore($(this).find('.text'));
		    		$(this).find('.text').append(el);
		    		el.focus();
		    		editing_element_index_value = element.relatedObj;


		    	} else {

		    		classList = $selectedItem.attr('class').split(/\s+/);
			    	var seq;
			    	var func;
					$.each(classList, function(index, item) {
					    if (item.indexOf("seq_") > -1) {
					        seq = item.split("seq_")[1];
					    }
					    if (item.indexOf("func_") > -1) {
					        func = item.split("func_")[1];
					    }
					});

					if ($($selectedItem).hasClass('local_vars')) {
						this.parentNode.relatedObj.coluna = programa.funcoes[func].variaveis[seq];
					}
					if ($($selectedItem).hasClass('parameters_vars')) {
					    this.parentNode.relatedObj.coluna = programa.funcoes[func].lista_parametros[seq];
					}
					if ($($selectedItem).hasClass('global_vars')) {
					 	this.parentNode.relatedObj.coluna = programa.globais[seq];
					}


		    	}

		    }
		});
	}

}


////$( "<span>oi</span>" ).insertBefore($(this).find('.close_parentheses'));
var editing_element_index_value = null;
function  addOptionsReaderVector(element, function_index) {


	if (element.relatedObj.variavel.dimensoes == 1) {

		var ret = ('<div class="ui dropdown change_column_reader"><span class="opened_index">[ </span> <div class="text"> </div> ]<i class="dropdown icon"></i><div class="menu"><div class="item" data-value="true">Valor</div><div class="item">Variável');
		ret += '<i class="dropdown icon"></i>'
	  		+ '<div class="menu func_'+function_index+'">';

		if (programa.funcoes[function_index].variaveis) {
			for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
				ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
	  		}
		}
	  	
	  	if (programa.funcoes[function_index].lista_parametros) {
	  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
	  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
	  		}
	  	}

	  	if (programa.globais) {
	  		for (var ij = 0; ij < programa.globais.length; ij++) {
	  			if (!programa.globais[ij].eh_constante) {
	  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
	  			}
	  		}
	  	}

	  	ret += '</div></div></div>';

		$( ret ).insertBefore($(element).find('.close_parentheses'));

		$('.dropdown.change_column_reader').dropdown({
		    onChange: function(value, text, $selectedItem) {
		    	console.log('QQQ2');

		    	if ($($selectedItem).data('value')) {
		    		var el;
		    		$(this).find('.text').text(' ');
		    		if (!isNaN(element.relatedObj.coluna)) {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="0" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		} else {
		    			el = $('<input type="text" style="z-index: 999;" size="2" value="'+element.relatedObj.coluna+'" onChange="editing_element_index_value.coluna = this.value; renderAlgorithm();" onblur="editing_element_index_value.coluna = this.value; renderAlgorithm();"  />');
		    		}
		    		
		    		//$(this).find('.text').text();
		    		//$( el ).insertBefore($(this).find('.text'));
		    		$(this).find('.text').append(el);
		    		el.focus();
		    		editing_element_index_value = element.relatedObj;


		    	} else {

		    		classList = $selectedItem.attr('class').split(/\s+/);
			    	var seq;
			    	var func;
					$.each(classList, function(index, item) {
					    if (item.indexOf("seq_") > -1) {
					        seq = item.split("seq_")[1];
					    }
					    if (item.indexOf("func_") > -1) {
					        func = item.split("func_")[1];
					    }
					});

					if ($($selectedItem).hasClass('local_vars')) {
						this.parentNode.relatedObj.coluna = programa.funcoes[func].variaveis[seq];
					}
					if ($($selectedItem).hasClass('parameters_vars')) {
					    this.parentNode.relatedObj.coluna = programa.funcoes[func].lista_parametros[seq];
					}
					if ($($selectedItem).hasClass('global_vars')) {
					 	this.parentNode.relatedObj.coluna = programa.globais[seq];
					}


		    	}

		    }
		});
	}

}

function updateGlobalType(wich_variable, new_value, new_dimensions) {
	programa.globais[wich_variable].tipo = new_value;
	programa.globais[wich_variable].dimensoes = new_dimensions;

	if (new_dimensions > 0) {
		programa.globais[wich_variable].linhas = new_dimensions;
		programa.globais[wich_variable].colunas = 2;
	}

	if (new_value == tiposDados.integer) {
		if (new_dimensions == 0) {
			programa.globais[wich_variable].valor = 1;
		}
		if (new_dimensions == 1) {
			programa.globais[wich_variable].valor = [1, 1];
		}
		if (new_dimensions == 2) {
			programa.globais[wich_variable].valor = [[1, 1], [1, 1]];
		}
	}

	if (new_value == tiposDados.real) {
		if (new_dimensions == 0) {
			programa.globais[wich_variable].valor = 1.0;
		}
		if (new_dimensions == 1) {
			programa.globais[wich_variable].valor = [1.0, 1.0];
		}
		if (new_dimensions == 2) {
			programa.globais[wich_variable].valor = [[1.0, 1.0], [1.0, 1.0]];
		}
	}

	if (new_value == tiposDados.text) {
		if (new_dimensions == 0) {
			programa.globais[wich_variable].valor = i18n(tiposDados.text);
		}
		if (new_dimensions == 1) {
			programa.globais[wich_variable].valor = [i18n(tiposDados.text), i18n(tiposDados.text)];
		}
		if (new_dimensions == 2) {
			programa.globais[wich_variable].valor = [[i18n(tiposDados.text), i18n(tiposDados.text)], [i18n(tiposDados.text), i18n(tiposDados.text)]];
		}
	}

	if (new_value == tiposDados.boolean) {
		if (new_dimensions == 0) {
			programa.globais[wich_variable].valor = true;
		}
		if (new_dimensions == 1) {
			programa.globais[wich_variable].valor = [true, true];
		}
		if (new_dimensions == 2) {
			programa.globais[wich_variable].valor = [[true, true], [true, true]];
		}
	}

	renderAlgorithm();
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

function addGlobalColumnVector(which_variable) {
	programa.globais[which_variable].colunas ++;

	if (programa.globais[which_variable].tipo == tiposDados.integer) {
		programa.globais[which_variable].valor.push(1);
	}
	if (programa.globais[which_variable].tipo == tiposDados.real) {
		programa.globais[which_variable].valor.push(1.0);
	}
	if (programa.globais[which_variable].tipo == tiposDados.text) {
		programa.globais[which_variable].valor.push(i18n(tiposDados.text));
	}
	if (programa.globais[which_variable].tipo == tiposDados.boolean) {
		programa.globais[which_variable].valor.push(true);
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

function addColumnGlobalMatrix(which_variable) {
	programa.globais[which_variable].colunas ++;

	if (programa.globais[which_variable].tipo == tiposDados.integer) {
		for (i = 0; i < programa.globais[which_variable].linhas; i++) {
			programa.globais[which_variable].valor[i].push(1);
		}
	}

	if (programa.globais[which_variable].tipo == tiposDados.real) {
		for (i = 0; i < programa.globais[which_variable].linhas; i++) {
			programa.globais[which_variable].valor[i].push(1.0);
		}
	}

	if (programa.globais[which_variable].tipo == tiposDados.text) {
		for (i = 0; i < programa.globais[which_variable].linhas; i++) {
			programa.globais[which_variable].valor[i].push(i18n(tiposDados.text));
		}
	}

	if (programa.globais[which_variable].tipo == tiposDados.boolean) {
		for (i = 0; i < programa.globais[which_variable].linhas; i++) {
			programa.globais[which_variable].valor[i].push(true);
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


function addLineGlobalMatrix(which_variable) {
	programa.globais[which_variable].linhas ++;

	if (programa.globais[which_variable].tipo == tiposDados.integer) {
		var n_l = [];
		for (i = 0; i < programa.globais[which_variable].colunas; i++) {
			n_l.push(1);
		}
		programa.globais[which_variable].valor.push(n_l);
	}
	if (programa.globais[which_variable].tipo == tiposDados.real) {
		var n_l = [];
		for (i = 0; i < programa.globais[which_variable].colunas; i++) {
			n_l.push(1.0);
		}
		programa.globais[which_variable].valor.push(n_l);
	}

	if (programa.globais[which_variable].tipo == tiposDados.text) {
		var n_l = [];
		for (i = 0; i < programa.globais[which_variable].colunas; i++) {
			n_l.push(i18n(tiposDados.text));
		}
		programa.globais[which_variable].valor.push(n_l);
	}

	if (programa.globais[which_variable].tipo == tiposDados.boolean) {
		var n_l = [];
		for (i = 0; i < programa.globais[which_variable].colunas; i++) {
			n_l.push(true);
		}
		programa.globais[which_variable].valor.push(n_l);
	}
	renderAlgorithm();
}


function removeGlobalColumnVector(which_variable) {
	if (programa.globais[which_variable].colunas == 0) {
		return;
	}

	programa.globais[which_variable].colunas --;
	programa.globais[which_variable].valor.splice(programa.globais[which_variable].valor.length - 1, 1);
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

function removeColumnGlobalMatrix(which_variable) {
	if (programa.globais[which_variable].colunas == 0) {
		return;
	}

	programa.globais[which_variable].colunas --;

	for (i = 0; i < programa.globais[which_variable].linhas; i++) {
		programa.globais[which_variable].valor[i].splice(programa.globais[which_variable].valor[i].length - 1, 1);
	}

	renderAlgorithm();
}

function removeLineGlobalMatrix(which_variable) {
	if (programa.globais[which_variable].linhas == 0) {
		return;
	}

	programa.globais[which_variable].linhas --;
	programa.globais[which_variable].valor.splice(programa.globais[which_variable].valor.length - 1, 1);

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
	updateReferencesToVarBeforeRemove(programa.funcoes[which_function].variaveis[which_variable]);

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

var opened_name_global = false;
var opened_input_global = null;
var sequence_name_opened_global;
function enableNameGlobalUpdate(div_el, sequence) {
	if (opened_name_global) {
		$(opened_input_global).focus();
		return;
	}
	opened_name_global = true;
	sequence_name_opened_global = sequence;

	$(div_el).find('.span_name_variable').text('');
	$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"+programa.globais[sequence].nome+"' />" ).insertBefore($(div_el).find('.span_name_variable'));

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
			programa.globais[sequence].nome = $(this).val().trim();
		}
		$(this).remove();

		/// update elements:
		opened_name_global = false;
		opened_input_global = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				programa.globais[sequence].nome = $(this).val().trim();
			}
			$(this).remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;

			renderAlgorithm();
		}
		if(code == 27) {
			$(div_el).find('.span_name_function').text(programa.globais[sequence].nome);

			$(this).remove();

			/// update elements:
			opened_name_global = false;
			opened_input_global = false;
		}
	});
	
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

function alternateBooleanGlobalVectorValue(parent_node, which_var, column_index) {
	programa.globais[which_var].valor[column_index] = !programa.globais[which_var].valor[column_index];
	renderAlgorithm();
}

function alternateBooleanVarMatrixValue(parent_node, which_function, which_var, row_index, column_index) {
	programa.funcoes[which_function].variaveis[which_var].valor[row_index][column_index] = !programa.funcoes[which_function].variaveis[which_var].valor[row_index][column_index];
	renderAlgorithm();
}

function alternateBooleanGlobalMatrixValue(parent_node, which_var, row_index, column_index) {
	programa.globais[which_var].valor[row_index][column_index] = !programa.globais[which_var].valor[row_index][column_index];
	renderAlgorithm();
}

function alternateBooleanGlobalValue(parent_node, which_var) {
	programa.globais[which_var].valor = !programa.globais[which_var].valor;
	renderAlgorithm();
}

function alternateBooleanVarValue(parent_node, which_function, which_var) {
	programa.funcoes[which_function].variaveis[which_var].valor = !programa.funcoes[which_function].variaveis[which_var].valor;
	renderAlgorithm();
}


var opened_name_value_vector_global_ = false;
var opened_input_value_vector_global_ = null;
var sequence_name_opened_value_vector_global_;
function enableGlobalVectorValueUpdate(parent_node, which_parameter, column_index) {
	if (opened_name_value_vector_global_) {
		$(opened_input_value_vector_global_).focus();
		return;
	}
	opened_name_value_vector_global_ = true;
	sequence_name_opened_value_vector_global_ = which_parameter;

	$(parent_node).find('.span_value_variable').text('');

	if (programa.globais[which_parameter].tipo == tiposDados.real) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.globais[which_parameter].valor[column_index].toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.globais[which_parameter].valor[column_index] + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
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
			if (programa.globais[which_parameter].tipo == tiposDados.real) {
				programa.globais[which_parameter].valor[column_index] = parseFloat($(this).val().trim());
			} else {

				if (programa.globais[which_parameter].tipo == tiposDados.integer) {
					programa.globais[which_parameter].valor[column_index] = parseInt($(this).val().trim());
				} else {
					programa.globais[which_parameter].valor[column_index] = $(this).val().trim();
				}

			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_vector_global_ = false;
		opened_input_value_vector_global_ = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (programa.globais[which_parameter].tipo == tiposDados.real) {
					programa.globais[which_parameter].valor[column_index] = parseFloat($(this).val().trim());
				} else {

					if (programa.globais[which_parameter].tipo == tiposDados.integer) {
						programa.globais[which_parameter].valor[column_index] = parseInt($(this).val().trim());
					} else {
						programa.globais[which_parameter].valor[column_index] = $(this).val().trim();
					}

				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;

			renderAlgorithm();
		}
		if(code == 27) {
			if (programa.globais[which_parameter].tipo == tiposDados.real) {
				$(parent_node).find('.span_value_variable').text(programa.globais[which_parameter].valor[column_index].toFixed(1));
			} else {
				$(parent_node).find('.span_value_variable').text(programa.globais[which_parameter].valor[column_index]);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_vector_global_ = false;
			opened_input_value_vector_global_ = false;
		}
	});
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


var opened_name_value_matrix_global_v = false;
var opened_input_value_matrix_global_v = null;
var sequence_name_opened_value_matrix_global_v;
function enableGlobalMatrixValueUpdate(parent_node, which_parameter, row_index, column_index) {
	if (opened_name_value_matrix_global_v) {
		$(opened_input_value_matrix_global_v).focus();
		return;
	}
	opened_name_value_matrix_global_v = true;
	sequence_name_opened_value_matrix_global_v = which_parameter;

	$(parent_node).find('.span_value_variable').text('');

	if (programa.globais[which_parameter].tipo == tiposDados.real) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.globais[which_parameter].valor[row_index][column_index].toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.globais[which_parameter].valor[row_index][column_index] + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
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
			if (programa.globais[which_parameter].tipo == tiposDados.real) {
				programa.globais[which_parameter].valor[row_index][column_index] = parseFloat($(this).val().trim());
			} else {

				if (programa.globais[which_parameter].tipo == tiposDados.integer) {
					programa.globais[which_parameter].valor[row_index][column_index] = parseInt($(this).val().trim());
				} else {
					programa.globais[which_parameter].valor[row_index][column_index] = $(this).val().trim();
				}

			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_matrix_global_v = false;
		opened_input_value_matrix_global_v = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (programa.globais[which_parameter].tipo == tiposDados.real) {
					programa.globais[which_parameter].valor[row_index][column_index] = parseFloat($(this).val().trim());
				} else {

					if (programa.globais[which_parameter].tipo == tiposDados.integer) {
						programa.globais[which_parameter].valor[row_index][column_index] = parseInt($(this).val().trim());
					} else {
						programa.globais[which_parameter].valor[row_index][column_index] = $(this).val().trim();
					}

				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;

			renderAlgorithm();
		}
		if(code == 27) {
			if (programa.globais[which_parameter].tipo == tiposDados.real) {
				$(parent_node).find('.span_value_variable').text(programa.globais[which_parameter].valor[row_index][column_index].toFixed(1));
			} else {
				$(parent_node).find('.span_value_variable').text(programa.globais[which_parameter].valor[row_index][column_index]);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_matrix_global_v = false;
			opened_input_value_matrix_global_v = false;
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


var opened_name_value_global_var = false;
var opened_input_value_global_ar = null;
var sequence_name_opened_value_global_var;
function enableGlobalValueUpdate(parent_node, which_parameter) {
	if (opened_name_value_global_var) {
		$(opened_input_value_global_ar).focus();
		return;
	}
	opened_name_value_global_var = true;
	sequence_name_opened_value_global_var = which_parameter;

	$(parent_node).find('.span_value_variable').text('');
	if (programa.globais[which_parameter].tipo == tiposDados.real) {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.globais[which_parameter].valor.toFixed(1) + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
	} else {
		$( "<input type='text' class='width-dynamic input_name_function' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false' value='"
			+ programa.globais[which_parameter].valor + "' />" ).insertBefore($(parent_node).find('.span_value_variable'));
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
			if (programa.globais[which_parameter].tipo == tiposDados.real) {
				programa.globais[which_parameter].valor = parseFloat($(this).val().trim());
			} else{
				if (programa.globais[which_parameter].tipo == tiposDados.integer) {
					programa.globais[which_parameter].valor = parseInt($(this).val().trim());
				} else {
					programa.globais[which_parameter].valor = $(this).val().trim();
				}
				
			}
		}
		$(this).remove();

		/// update elements:
		opened_name_value_global_var = false;
		opened_input_value_global_ar = false;

		renderAlgorithm();
	});

	$('.width-dynamic').on('keydown', function(e) {
		var code = e.keyCode || e.which;
		if(code == 13) {
			if ($(this).val().trim()) {
				if (programa.globais[which_parameter].tipo == tiposDados.real) {
					programa.globais[which_parameter].valor = parseFloat($(this).val().trim());
				} else{
					if (programa.globais[which_parameter].tipo == tiposDados.integer) {
						programa.globais[which_parameter].valor = parseInt($(this).val().trim());
					} else {
						programa.globais[which_parameter].valor = $(this).val().trim();
					}
					
				}
			}
			$(this).remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;

			renderAlgorithm();
		}
		if(code == 27) {
			if (programa.globais[which_parameter].tipo == tiposDados.real) {
				$(parent_node).find('.span_value_variable').text(programa.globais[which_parameter].valor.toFixed(1));
			} else{
				$(parent_node).find('.span_value_variable').text(programa.globais[which_parameter].valor);
			}

			$(this).remove();

			/// update elements:
			opened_name_value_global_var = false;
			opened_input_value_global_ar = false;
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
		temp_value = parent_node.relatedObj.texto_comentario;
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
				parent_node.relatedObj.texto_comentario = n_value;
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
					parent_node.relatedObj.texto_comentario = n_value;
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
	updateReferencesToVarBeforeRemove(programa.funcoes[which_function].lista_parametros[which_parameter]);

	programa.funcoes[which_function].lista_parametros.splice(which_parameter, 1);
	renderAlgorithm();
}

function updateReferencesToVarBeforeRemove(which_var) {
	for (var i = 1; i < allCommandsReference.length; i++) {
		if (allCommandsReference[i].tipo) {
			if (allCommandsReference[i].tipo == tiposComandos.reader) {

				if (allCommandsReference[i].variavel == which_var) {
					allCommandsReference[i].variavel = null;
					allCommandsReference[i].linha = 0;
					allCommandsReference[i].coluna = 0;
				}

				if (allCommandsReference[i].coluna == which_var) {
					allCommandsReference[i].coluna = 0;
				}

				if (allCommandsReference[i].linha == which_var) {
					allCommandsReference[i].linha = 0;
				}

			}
		}
	}
}


function appendFunction(function_obj, sequence) {
	console.log("appendFunction called: " + sequence);
	var appender = '<div class="ui secondary segment function_div list-group-item" data-fun="'+sequence+'" data-idcommand="'+function_obj.id_command+'">';

	if (function_obj.comentario_funcao) {
		appender += renderComment(function_obj.comentario_funcao, sequence, true, -1);
	}
		
	appender += '<span class="glyphicon glyphicon-move move_function" aria-hidden="true"><i class="icon sort alternate vertical"></i></span>';

	appender += (!function_obj.eh_principal ? '<button class="ui icon button large remove_function_button" onclick="removeFunctionHandler(this.parentNode, '+sequence+')"><i class="red icon times"></i></button>' : '<div class="div_start_minimize_v"> </div>')
		+ '<button class="ui icon button tiny minimize_function_button" onclick="minimizeFunctionHandler(this.parentNode, '+sequence+')"><i class="icon window minimize"></i></button>';

	appender += '<div class="ui icon buttons add_var_top_button"><div class="ui icon button" onclick="addVariable('+sequence+')"><i class="icon superscript"></i></div>';
	
	appender += '<div class="ui icon button dropdown" ><i class="icon code"></i> <div class="menu"> ';
	appender += '<a class="item create_reader" data-text="'+tiposComandos.reader+'" data-fun="'+sequence+'"><i class="download icon"></i> ' +i18n('text_read_var')+ '</a>'
			  + '<a class="item create_writer" data-text="'+tiposComandos.writer+'" data-fun="'+sequence+'"><i class="upload icon"></i> '+i18n('text_write_var')+'</a>'
			  + '<a class="item create_comment" data-text="'+tiposComandos.comment+'" data-fun="'+sequence+'"><i class="quote left icon"></i> '+i18n('text_comment')+'</a>'
			  + '<a class="item create_attribution" data-text="'+tiposComandos.comment+'" data-fun="'+sequence+'"><i class="arrow left icon"></i> '+i18n('text_attribution')+'</a>'
			  + '<a class="item create_iftrue" data-text="'+tiposComandos.iftrue+'" data-fun="'+sequence+'"><i class="random icon"></i> '+i18n('text_iftrue')+'</a>'
			  + '<a class="item create_repeatNtimes" data-text="'+tiposComandos.repeatNtimes+'" data-fun="'+sequence+'"><i class="sync icon"></i> '+i18n('text_repeatNtimes')+'</a>'
			  + '<a class="item create_whiletrue" data-text="'+tiposComandos.whiletrue+'" data-fun="'+sequence+'"><i class="sync icon"></i> '+i18n('text_whiletrue')+'</a>'
			  + '<a class="item create_dowhiletrue" data-text="'+tiposComandos.dowhiletrue+'" data-fun="'+sequence+'"><i class="sync icon"></i> '+i18n('text_dowhiletrue')+'</a>'
			  + '<a class="item create_switch" data-text="'+tiposComandos.switch+'" data-fun="'+sequence+'"><i class="list icon"></i> '+i18n('text_switch')+'</a>'
			  + '<a class="item create_functioncall" data-text="'+tiposComandos.functioncall+'" data-fun="'+sequence+'"><i class="hand point right icon"></i> '+i18n('text_functioncall')+'</a>'
				+ '</div></div></div>';



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
		+ '<div class="ui bottom attached segment commands_list_div" id="function_drag_cmd_'+sequence+'" data-fun="'+sequence+'" data-idcommand="'+function_obj.id_command+'">';


	if (programa.funcoes[sequence].comandos) {
		for (l = 0; l < programa.funcoes[sequence].comandos.length; l++) {
			appender += renderElementCommandGeneric(programa.funcoes[sequence].comandos[l], sequence, l, -1, l);
			
		}
	}

	appender += '</div>';

	appender += '<div class="function_close_div">}</div>'
		+ '</div>'
		+ '</div>';

	$('.all_functions').append(appender);
}

function renderElementCommandGeneric(command, sequence, l, parent, fullpath) {
	if (command.tipo == tiposComandos.comment) {
		return renderComment(command, sequence, false, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.reader) {
		return renderReader(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.writer) {
		return renderWriter(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.attribution) {
		return renderAttribution(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.iftrue) {
		return renderIfTrue(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.repeatNtimes) {
		return renderRepeatNtimes(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.whiletrue) {
		return renderWhiletrue(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.dowhiletrue) {
		return renderDowhiletrue(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.switch) {
		return renderSwitch(command, sequence, l, parent, fullpath);
	}
	if (command.tipo == tiposComandos.functioncall) {
		return renderFunctioncall(command, sequence, l, parent, fullpath);
	}
}

function renderFunctioncall(functioncall_obj, function_index, functioncall_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui functioncall" data-index="'+functioncall_index+'" data-command="'+functioncall_index+'" data-idcommand="'+functioncall_obj.id_command+'" data-parent="'+data_parent+'" data-fullpath="'+fullpath+'"> <i class="hand point right icon command_drag"></i> <span> funcao() </span>';
	ret += '</div>';

	return ret;
}

function renderSwitch(switch_obj, function_index, repeat_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui switch" data-index="'+repeat_index+'" data-parent="'+data_parent+'" data-idcommand="'+switch_obj.id_command+'"  data-fullpath="'+fullpath+'"> <i class="ui icon small random command_drag" ></i> <span> escolha (x) { <br> caso 1: <br> caso 2: <br> }</span>';
	ret += '</div>';

	return ret;
}

function renderDowhiletrue(dowhiletrue_obj, function_index, repeat_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui dowhiletrue" data-index="'+repeat_index+'" data-parent="'+data_parent+'" data-idcommand="'+dowhiletrue_obj.id_command+'"  data-fullpath="'+fullpath+'"> <i class="ui icon small random command_drag"></i> <span> faça  { </span>';

	ret += '<div class="ui block_commands" data-subblock="'+repeat_index+'" data-idcommand="'+dowhiletrue_obj.id_command+'">';

	if ((dowhiletrue_obj.commands_block == null)
			|| (dowhiletrue_obj.commands_block.length == 0)) {
	} else {
		for (i = 0; i < dowhiletrue_obj.commands_block.length; i ++) {
			ret += renderElementCommandGeneric(dowhiletrue_obj.commands_block[i], function_index, i, repeat_index, (fullpath + ',' + i));
		}
	}

	ret += '</div>';
	ret += '<span> } enquanto (x < 10); </span>';

	ret += '</div>';

	return ret;
}

function renderWhiletrue(whiletrue_obj, function_index, repeat_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui whiletrue" data-index="'+repeat_index+'" data-idcommand="'+whiletrue_obj.id_command+'" data-parent="'+data_parent+'" data-fullpath="'+fullpath+'"> <i class="ui icon small random command_drag"></i> <span> enquanto (x < 10) { </span>';
	
	ret += '<div class="ui block_commands" data-subblock="'+repeat_index+'" data-idcommand="'+whiletrue_obj.id_command+'">';

	if ((whiletrue_obj.commands_block == null)
			|| (whiletrue_obj.commands_block.length == 0)) {
	} else {
		for (i = 0; i < whiletrue_obj.commands_block.length; i ++) {
			ret += renderElementCommandGeneric(whiletrue_obj.commands_block[i], function_index, i, repeat_index, (fullpath + ',' + i));
		}
	}

	ret += '</div>';
	ret += '<span> }</span>';

	ret += '</div>';

	return ret;
}

function renderRepeatNtimes(repeat_obj, function_index, repeat_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui iftrue" data-index="'+repeat_index+'" data-parent="'+data_parent+'" data-idcommand="'+repeat_obj.id_command+'" data-fullpath="'+fullpath+'"> <i class="ui icon small random command_drag"></i> <span> para (x = 0; x < 10; x ++) { </span>';
	ret += '<div class="ui block_commands" data-subblock="'+repeat_index+'" data-idcommand="'+repeat_obj.id_command+'">';

	if ((repeat_obj.commands_block == null)
			|| (repeat_obj.commands_block.length == 0)) {
	} else {
		for (i = 0; i < repeat_obj.commands_block.length; i ++) {
			ret += renderElementCommandGeneric(repeat_obj.commands_block[i], function_index, i, repeat_index, (fullpath + ',' + i));
		}
	}

	ret += '</div>';
	ret += '<span> }</span>';
	ret += '</div>';

	return ret;
}

function renderIfTrue(writer_obj, function_index, iftrue_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui iftrue" data-index="'+iftrue_index+'" data-parent="'+data_parent+'" data-idcommand="'+writer_obj.id_command+'" data-block="'+iftrue_index+'" data-fullpath="'+fullpath+'"> <i class="ui icon small random command_drag"></i> <span> if (x < 1) { </span>';
	ret += '<div class="ui block_commands" data-subblock="'+iftrue_index+'" data-if="true" data-idcommand="'+writer_obj.id_command+'">';

	if ((writer_obj.commands_block == null)
			|| (writer_obj.commands_block.length == 0)) {
	} else {
		for (ki = 0; ki < writer_obj.commands_block.length; ki ++) {
			ret += renderElementCommandGeneric(writer_obj.commands_block[ki], function_index, ki, iftrue_index, (fullpath + ',' + ki));
		}
	}

	ret += '</div>';
	ret += '<span> } else { </span>';

	ret += '<div class="ui block_commands" data-subblock="'+iftrue_index+'" data-else="true" data-idcommand="'+writer_obj.id_command+'">';

	if ((writer_obj.commands_else == null)
			|| (writer_obj.commands_else.length == 0)) {
	} else {
		for (ki = 0; ki < writer_obj.commands_else.length; ki ++) {
			ret += renderElementCommandGeneric(writer_obj.commands_else[ki], function_index, ki, iftrue_index, (fullpath + ',' + ki));
		}
	}

	ret += '</div>';

	ret += '<span> }</span>';
	ret += '</div>';

	return ret;
}

function renderAttribution(writer_obj, function_index, attr_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui attribution" data-index="'+attr_index+'" data-command="'+attr_index+'" data-idcommand="'+writer_obj.id_command+'" data-parent="'+data_parent+'" data-fullpath="'+fullpath+'"> <i class="ui icon small arrow left command_drag"></i> <span> x =  1 + 1</span>';
	ret += '</div>';

	return ret;
}

function renderWriter(writer_obj, function_index, reader_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui writer" data-index="'+reader_index+'" data-command="'+reader_index+'" data-idcommand="'+writer_obj.id_command+'" data-parent="'+data_parent+'" data-fullpath="'+fullpath+'"> <i class="ui icon small upload command_drag"></i> <span>'+i18n('write')+' x</span>';
	ret += '</div>';

	return ret;
}

function renderReader(reader_obj, function_index, reader_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui reader" data-fun="'+function_index+'" data-index="'+reader_index+'" data-command="'+reader_index+'" data-idcommand="'+reader_obj.id_command+'" data-parent="'+data_parent+'" data-fullpath="'+fullpath+'"> <i class="ui icon small download command_drag"></i> <span>'+i18n('read')+' ( </span>';

	
	ret += '<div class="ui dropdown variable_reader">';

	if (reader_obj.variavel) {
		if (reader_obj.variavel.eh_constante) {
			reader_obj.variavel = null;
			ret += '<div class="text seq_ func_'+function_index+'">'+i18n('ui:text_select_var')+'</div>';
		} else {
			ret += '<div class="text seq_ func_'+function_index+'">'+reader_obj.variavel.nome+'</div>';
		}
	} else {
		ret += '<div class="text seq_ func_'+function_index+'">'+i18n('ui:text_select_var')+'</div>';
	}
  	
  	ret += '<i class="dropdown icon"></i>'
	  	+ '<div class="menu func_'+function_index+'">';

	if (programa.funcoes[function_index].variaveis) {
		for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
			ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
  		}
	}
  	
  	if (programa.funcoes[function_index].lista_parametros) {
  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
  		}
  	}

  	if (programa.globais) {
  		for (var ij = 0; ij < programa.globais.length; ij++) {
  			if (!programa.globais[ij].eh_constante) {
  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
  			}
  		}
  	}
  	
	ret += '</div></div> ';

	if (reader_obj.variavel) {
		if (reader_obj.variavel.dimensoes == 1) {


			ret += '<div class="ui dropdown change_column_reader_render"><span class="opened_index">[ </span> <div class="text"> ';

			if (reader_obj.coluna) {
				if (reader_obj.coluna.nome) {
					ret += reader_obj.coluna.nome;
				} else {
					ret += reader_obj.coluna;
				}
			}

			ret += '</div> ]<i class="dropdown icon"></i><div class="menu"><div class="item" data-value="true">Valor</div><div class="item">Variável';
			ret += '<i class="dropdown icon"></i>'
		  		+ '<div class="menu func_'+function_index+'">';

			if (programa.funcoes[function_index].variaveis) {
				for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
					ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
		  		}
			}
		  	
		  	if (programa.funcoes[function_index].lista_parametros) {
		  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
		  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
		  		}
		  	}

		  	if (programa.globais) {
		  		for (var ij = 0; ij < programa.globais.length; ij++) {
		  			if (!programa.globais[ij].eh_constante) {
		  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
		  			}
		  		}
		  	}

		  	ret += '</div></div></div></div>';

		} else if (reader_obj.variavel.dimensoes == 2) {


		  	// Linha: 
			ret += '<div class="ui dropdown change_column_reader_render_matrix_line"><span class="opened_index">[ </span> <div class="text"> ';

			if (reader_obj.linha) {
				if (reader_obj.linha.nome) {
					ret += reader_obj.linha.nome;
				} else {
					ret += reader_obj.linha;
				}
			}

			ret += '</div> ]<i class="dropdown icon"></i><div class="menu"><div class="item" data-value="true">Valor</div><div class="item">Variável';
			ret += '<i class="dropdown icon"></i>'
		  		+ '<div class="menu func_'+function_index+'">';

			if (programa.funcoes[function_index].variaveis) {
				for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
					ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
		  		}
			}
		  	
		  	if (programa.funcoes[function_index].lista_parametros) {
		  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
		  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
		  		}
		  	}

		  	if (programa.globais) {
		  		for (var ij = 0; ij < programa.globais.length; ij++) {
		  			if (!programa.globais[ij].eh_constante) {
		  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
		  			}
		  		}
		  	}

		  	ret += '</div></div></div></div>';



		  	// Coluna: 
			ret += '<div class="ui dropdown change_column_reader_render_matrix_column"><span class="opened_index">[ </span> <div class="text"> ';

			if (reader_obj.coluna) {
				if (reader_obj.coluna.nome) {
					ret += reader_obj.coluna.nome;
				} else {
					ret += reader_obj.coluna;
				}
			}

			ret += '</div> ]<i class="dropdown icon"></i><div class="menu"><div class="item" data-value="true">Valor</div><div class="item">Variável';
			ret += '<i class="dropdown icon"></i>'
		  		+ '<div class="menu func_'+function_index+'">';

			if (programa.funcoes[function_index].variaveis) {
				for (var i = 0; i < programa.funcoes[function_index].variaveis.length; i++) {
					ret += '<div class="item local_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].variaveis[i].nome + ' </div>';
		  		}
			}
		  	
		  	if (programa.funcoes[function_index].lista_parametros) {
		  		for (var i = 0; i < programa.funcoes[function_index].lista_parametros.length; i++) {
		  			ret += '<div class="item parameters_vars seq_'+i+' func_'+function_index+'"> ' + programa.funcoes[function_index].lista_parametros[i].nome + ' </div>';
		  		}
		  	}

		  	if (programa.globais) {
		  		for (var ij = 0; ij < programa.globais.length; ij++) {
		  			if (!programa.globais[ij].eh_constante) {
		  				ret += '<div class="item global_vars seq_'+ij+' func_'+function_index+'"> ' + programa.globais[ij].nome + ' </div>';
		  			}
		  		}
		  	}

		  	ret += '</div></div></div></div>';

		}
	}

			
	ret += '<span class="close_parentheses">)</span> </div>';
	return ret;
}

function renderComment(comment_obj, function_index, is_function_comment, comment_index, data_parent, fullpath) {
	var ret = '';
	ret += '<div class="ui comment" data-index="'+comment_index+'" data-command="'+comment_index+'" data-idcommand="'+comment_obj.id_command+'" data-parent="'+data_parent+'" data-fullpath="'+fullpath+'"> <i class="ui icon small quote left '+(is_function_comment?'':'command_drag')+'"></i> <span class="span_comment_text" ondblclick="enableCommentUpdate(this.parentNode, '+function_index+', '
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

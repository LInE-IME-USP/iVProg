import $ from 'jquery';
import { Types } from './../ast/types';
import * as Modelos from './visual';
import { LocalizedStrings } from './../services/localizedStringsService';

var counter_new_functions = 0;
var counter_new_parameters = 0;
var counter_new_variables = 0;
var counter_new_globals = 0;

const programa = new Modelos.Programa()
const funcaoPrincipal = new Modelos.Funcao(LocalizedStrings.getUI("start"), Types.VOID, 0, [], true);
funcaoPrincipal.comentario_funcao = new Modelos.Comentario(LocalizedStrings.getUI('text_comment_main'));
programa.adicionarFuncao(funcaoPrincipal);

function addFunctionHandler() {
	new_function = new Modelos.Funcao(LocalizedStrings.getUI("new_function") + "_" + counter_new_functions, Types.VOID, 0, new Array(), false, false, null, new Comentario(LocalizedStrings.getUI('text_comment_start')));
	programa.adicionarFuncao(new_function);

	counter_new_functions ++;
	//renderAlgorithm();
}

function addGlobalVar() {
	var v = new Modelos.Variavel(Types.INTEGER, LocalizedStrings.getUI('new_global') + '_' + counter_new_globals, 1);
	counter_new_globals ++;

	programa.adicionarGlobal(v);
	//renderAlgorithm();
}

export function initVisualUI () {
  // MUST USE CONST, LET, OR VAR !!!!!!
  const mainDiv = $('#visual-main-div');
  // fill mainDiv with functions and globals...
  // renderAlgorithm()...
  $('.add_function_button').on('click', () => {
    addFunctionHandler()
  });
  $('.add-globalVar-button').on('click', () => {
    addGlobalVar()
  });
}
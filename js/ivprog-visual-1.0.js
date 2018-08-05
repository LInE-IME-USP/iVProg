// Definição das classes utilizadas para armazenar o algoritmo

var tiposDados = Object.freeze({void:"void", integer:"integer", real:"real", text:"text", boolean:"boolean"});

var Variavel = function(tipo, nome, dimensoes = 0) {
	this.tipo = tipo;
	this.nome = nome;
	this.dimensoes = dimensoes;
};

var Funcao = function(nome, tipo_retorno = tiposDados.void, dimensoes_retorno = 0, lista_parametros = null, eh_principal = false, esta_oculta = false) {
	this.nome = nome;
	this.tipo_retorno = tipo_retorno;
	this.dimensoes_retorno = dimensoes_retorno;
	this.lista_parametros = lista_parametros;
	this.eh_principal = eh_principal;
	this.esta_oculta = esta_oculta;
};

var Programa = function () {
	this.funcoes = new Array();
};

function adicionarFuncao(funcao) {
	programa.funcoes.push(funcao);
};

// Adicionando a função principal automaticamente
var programa = new Programa();
var funcaoPrincipal = new Funcao("principal", tiposDados.void, 0, null, true);

adicionarFuncao(funcaoPrincipal);
// Definição das classes utilizadas para armazenar o algoritmo

var tiposDados = Object.freeze({void:"void", integer:"integer", real:"real", text:"text", boolean:"boolean"});

var Variavel = function(tipo, nome, valor, dimensoes = 0, eh_constante = false) {
	this.tipo = tipo;
	this.nome = nome;
	this.valor = valor;
	this.dimensoes = dimensoes;
	this.eh_constante = eh_constante;
};

var Funcao = function(nome, tipo_retorno = tiposDados.void, dimensoes_retorno = 0, lista_parametros = null, eh_principal = false, esta_oculta = false) {
	this.nome = nome;
	this.tipo_retorno = tipo_retorno;
	this.dimensoes_retorno = dimensoes_retorno;
	this.lista_parametros = lista_parametros;
	this.eh_principal = eh_principal;
	this.esta_oculta = esta_oculta;
};

var Comando = function(tipo) {
	this.tipo = tipo;
};

var Expressao = function(conteudo) {
	this.conteudo = conteudo;

};


var Programa = function () {
	this.funcoes = new Array();
};

function adicionarFuncao(funcao) {
	programa.funcoes.push(funcao);
}

// Adicionando a função principal automaticamente
var programa = new Programa();
var funcaoPrincipal = new Funcao("principal", tiposDados.void, 0, new Array(), true);

funcaoPrincipal.lista_parametros.push(new Variavel(tiposDados.text, "args"));


adicionarFuncao(funcaoPrincipal);
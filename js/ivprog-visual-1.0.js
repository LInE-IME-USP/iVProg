// Definição das classes utilizadas para armazenar o algoritmo

// Não adicionar elementos ao tipoDados, pois existem componentes que dependem do seu tamanho e isso afetará seu funcionamento
var tiposDados = Object.freeze({void:"void", integer:"integer", real:"real", text:"text", boolean:"boolean", vector:"vector"});

var tiposComandos = Object.freeze({comment:"comment", reader:"reader", writer:"writer", attribution:"attribution", iftrue:"iftrue",
 repeatNtimes:"repeatNtimes", whiletrue:"whiletrue", dowhiletrue:"dowhiletrue", switch:"switch", functioncall:"functioncall"});

var Variavel = function(tipo, nome, valor, dimensoes = 0, eh_constante = false, linhas = 0, colunas = 0) {
	this.tipo = tipo;
	this.nome = nome;
	this.valor = valor;
	this.dimensoes = dimensoes;
	this.eh_constante = eh_constante;
	this.linhas = linhas;
	this.colunas = colunas;
};

var Funcao = function(nome, tipo_retorno = tiposDados.void, dimensoes_retorno = 0, lista_parametros = null, eh_principal = false, esta_oculta = false, variaveis = null, comentario_funcao = null) {
	this.nome = nome;
	this.tipo_retorno = tipo_retorno;
	this.dimensoes_retorno = dimensoes_retorno;
	this.lista_parametros = lista_parametros;
	this.eh_principal = eh_principal;
	this.esta_oculta = esta_oculta;
	this.variaveis = variaveis;
	this.comentario_funcao = comentario_funcao;
	this.comandos = null;
};

var Comentario = function(texto_comentario) {
	this.tipo = tiposComandos.comment;
	this.texto_comentario = texto_comentario;
};

var Leitura = function(variavel) {
	this.tipo = tiposComandos.reader;
	this.variavel = variavel;
};

var Escrita = function(conteudo) {
	this.tipo = tiposComandos.writer;
	this.conteudo = conteudo;
};

var Atribuicao = function(variavel, expressao) {
	this.tipo = tiposComandos.attribution;
	this.variavel = variavel;
	this.expressao = expressao;
};

var SeVerdadeiro = function(expressao, commands_block, commands_else) {
	this.tipo = tiposComandos.iftrue;
	this.expressao = expressao;
	this.commands_block = commands_block;
	this.commands_else = commands_else;
};

var RepitaNVezes = function(expressao1, expressao2, expressao3, commands_block) {
	this.tipo = tiposComandos.repeatNtimes;
	this.expressao1 = expressao1;
	this.expressao2 = expressao2;
	this.expressao3 = expressao3;
	this.commands_block = commands_block;
};

var EnquantoVerdadeiro = function(expressao, commands_block) {
	this.tipo = tiposComandos.whiletrue;
	this.expressao = expressao;
	this.commands_block = commands_block;
};

var FacaEnquantoVerdadeiro = function(expressao, commands_block) {
	this.tipo = tiposComandos.dowhiletrue;
	this.expressao = expressao;
	this.commands_block = commands_block;
};

var Escolha = function(variavel, lista_casos_e_blocos) {
	this.tipo = tiposComandos.switch;
	this.variavel = variavel;
	this.lista_casos_e_blocos = lista_casos_e_blocos;

};

var ChamadaFuncao = function(funcao, lista_parametros) {
	this.tipo = tiposComandos.functioncall;
	this.funcao = funcao;
	this.lista_parametros = lista_parametros;
}

var Programa = function () {
	this.funcoes = new Array();
	this.globais = new Array();
};

function adicionarFuncao(funcao) {
	programa.funcoes.push(funcao);
}

function adicionarVariavel(funcao, variavel) {
	if (programa.funcoes[funcao].variaveis == null) {
		programa.funcoes[funcao].variaveis = new Array();
	}
	programa.funcoes[funcao].variaveis.push(variavel);
}

//var tex = i18n('text');

// Adicionando a função principal automaticamente
var programa = new Programa();
var funcaoPrincipal = new Funcao(i18n("start"), tiposDados.void, 0, [], true);
funcaoPrincipal.comentario_funcao = new Comentario(i18n('text_comment_main'));

//funcaoPrincipal.lista_parametros.push(new Variavel(tiposDados.text, "args"));


adicionarFuncao(funcaoPrincipal);

var funcaoSomar = new Funcao("somar", tiposDados.integer, 0, [], false, false, null, new Comentario(i18n('text_comment_start')));
funcaoSomar.lista_parametros.push(new Variavel(tiposDados.integer, "a"));
funcaoSomar.lista_parametros.push(new Variavel(tiposDados.integer, "b"));

//adicionarFuncao(funcaoSomar);
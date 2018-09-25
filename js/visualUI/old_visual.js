// Definição das classes utilizadas para armazenar o algoritmo

// Não adicionar elementos ao tipoDados, pois existem componentes que dependem do seu tamanho e isso afetará seu funcionamento
var tiposDados = Object.freeze({void:"void", integer:"integer", real:"real", text:"text", boolean:"boolean", vector:"vector"});

var tiposComandos = Object.freeze({comment:"comment", reader:"reader", writer:"writer"});

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

var Comando = function(tipo) {
	this.tipo = tipo;
};

var Expressao = function(conteudo) {
	this.conteudo = conteudo;

};

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

adicionarFuncao(funcaoSomar);
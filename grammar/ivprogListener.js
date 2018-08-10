// Generated from grammar/ivprog.g4 by ANTLR 4.7.1
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete listener for a parse tree produced by ivprogParser.
function ivprogListener() {
	antlr4.tree.ParseTreeListener.call(this);
	return this;
}

ivprogListener.prototype = Object.create(antlr4.tree.ParseTreeListener.prototype);
ivprogListener.prototype.constructor = ivprogListener;

// Enter a parse tree produced by ivprogParser#parse.
ivprogListener.prototype.enterParse = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#parse.
ivprogListener.prototype.exitParse = function(ctx) {
};


// Enter a parse tree produced by ivprogParser#programa.
ivprogListener.prototype.enterPrograma = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#programa.
ivprogListener.prototype.exitPrograma = function(ctx) {
};


// Enter a parse tree produced by ivprogParser#declaracoesGlobais.
ivprogListener.prototype.enterDeclaracoesGlobais = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#declaracoesGlobais.
ivprogListener.prototype.exitDeclaracoesGlobais = function(ctx) {
};


// Enter a parse tree produced by ivprogParser#listaDeclaracoes.
ivprogListener.prototype.enterListaDeclaracoes = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#listaDeclaracoes.
ivprogListener.prototype.exitListaDeclaracoes = function(ctx) {
};


// Enter a parse tree produced by ivprogParser#declaracao.
ivprogListener.prototype.enterDeclaracao = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#declaracao.
ivprogListener.prototype.exitDeclaracao = function(ctx) {
};


// Enter a parse tree produced by ivprogParser#declaracaoTipoDado.
ivprogListener.prototype.enterDeclaracaoTipoDado = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#declaracaoTipoDado.
ivprogListener.prototype.exitDeclaracaoTipoDado = function(ctx) {
};


// Enter a parse tree produced by ivprogParser#expressao.
ivprogListener.prototype.enterExpressao = function(ctx) {
};

// Exit a parse tree produced by ivprogParser#expressao.
ivprogListener.prototype.exitExpressao = function(ctx) {
};



exports.ivprogListener = ivprogListener;
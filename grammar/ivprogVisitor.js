// Generated from ../ivprog/grammar/ivprog.g4 by ANTLR 4.7.1
// jshint ignore: start
var antlr4 = require('antlr4/index');

// This class defines a complete generic visitor for a parse tree produced by ivprogParser.

function ivprogVisitor() {
	antlr4.tree.ParseTreeVisitor.call(this);
	return this;
}

ivprogVisitor.prototype = Object.create(antlr4.tree.ParseTreeVisitor.prototype);
ivprogVisitor.prototype.constructor = ivprogVisitor;

// Visit a parse tree produced by ivprogParser#parse.
ivprogVisitor.prototype.visitParse = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by ivprogParser#programa.
ivprogVisitor.prototype.visitPrograma = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by ivprogParser#declaracoesGlobais.
ivprogVisitor.prototype.visitDeclaracoesGlobais = function(ctx) {
  return this.visitChildren(ctx);
};


// Visit a parse tree produced by ivprogParser#listaDeclaracoes.
ivprogVisitor.prototype.visitListaDeclaracoes = function(ctx) {
  return this.visitChildren(ctx);
};



exports.ivprogVisitor = ivprogVisitor;
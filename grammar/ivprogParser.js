// Generated from grammar/ivprog.g4 by ANTLR 4.7.1
// jshint ignore: start
var antlr4 = require('antlr4/index');
var ivprogListener = require('./ivprogListener').ivprogListener;

  import {ASA, NoGlobal} from '../js/asa';

var grammarFileName = "ivprog.g4";

var serializedATN = ["\u0003\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964",
    "\u0003&S\u0004\u0002\t\u0002\u0004\u0003\t\u0003\u0004\u0004\t\u0004",
    "\u0004\u0005\t\u0005\u0004\u0006\t\u0006\u0004\u0007\t\u0007\u0004\b",
    "\t\b\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0002\u0003\u0003\u0003",
    "\u0003\u0003\u0003\u0003\u0003\u0007\u0003\u0019\n\u0003\f\u0003\u000e",
    "\u0003\u001c\u000b\u0003\u0003\u0003\u0003\u0003\u0003\u0004\u0003\u0004",
    "\u0003\u0004\u0003\u0005\u0005\u0005$\n\u0005\u0003\u0005\u0003\u0005",
    "\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005\u0003\u0005",
    "\u0007\u0005.\n\u0005\f\u0005\u000e\u00051\u000b\u0005\u0003\u0006\u0003",
    "\u0006\u0003\u0006\u0005\u00066\n\u0006\u0003\u0006\u0003\u0006\u0003",
    "\u0006\u0005\u0006;\n\u0006\u0003\u0006\u0005\u0006>\n\u0006\u0005\u0006",
    "@\n\u0006\u0003\u0006\u0003\u0006\u0005\u0006D\n\u0006\u0003\u0006\u0003",
    "\u0006\u0003\u0007\u0003\u0007\u0003\u0007\u0003\u0007\u0003\u0007\u0005",
    "\u0007M\n\u0007\u0003\u0007\u0003\u0007\u0003\b\u0003\b\u0003\b\u0002",
    "\u0002\t\u0002\u0004\u0006\b\n\f\u000e\u0002\u0003\u0004\u0002!!##\u0002",
    "W\u0002\u0010\u0003\u0002\u0002\u0002\u0004\u0014\u0003\u0002\u0002",
    "\u0002\u0006\u001f\u0003\u0002\u0002\u0002\b#\u0003\u0002\u0002\u0002",
    "\n2\u0003\u0002\u0002\u0002\fL\u0003\u0002\u0002\u0002\u000eP\u0003",
    "\u0002\u0002\u0002\u0010\u0011\u0005\u0004\u0003\u0002\u0011\u0012\u0007",
    "\u0002\u0002\u0003\u0012\u0013\b\u0002\u0001\u0002\u0013\u0003\u0003",
    "\u0002\u0002\u0002\u0014\u0015\u0007\t\u0002\u0002\u0015\u0016\u0007",
    "\u0003\u0002\u0002\u0016\u001a\b\u0003\u0001\u0002\u0017\u0019\u0005",
    "\u0006\u0004\u0002\u0018\u0017\u0003\u0002\u0002\u0002\u0019\u001c\u0003",
    "\u0002\u0002\u0002\u001a\u0018\u0003\u0002\u0002\u0002\u001a\u001b\u0003",
    "\u0002\u0002\u0002\u001b\u001d\u0003\u0002\u0002\u0002\u001c\u001a\u0003",
    "\u0002\u0002\u0002\u001d\u001e\u0007\u0004\u0002\u0002\u001e\u0005\u0003",
    "\u0002\u0002\u0002\u001f \u0005\b\u0005\u0002 !\b\u0004\u0001\u0002",
    "!\u0007\u0003\u0002\u0002\u0002\"$\u0007\u0013\u0002\u0002#\"\u0003",
    "\u0002\u0002\u0002#$\u0003\u0002\u0002\u0002$%\u0003\u0002\u0002\u0002",
    "%&\u0005\f\u0007\u0002&\'\u0005\n\u0006\u0002\'(\b\u0005\u0001\u0002",
    "(/\u0003\u0002\u0002\u0002)*\u0007\u0005\u0002\u0002*+\u0005\n\u0006",
    "\u0002+,\b\u0005\u0001\u0002,.\u0003\u0002\u0002\u0002-)\u0003\u0002",
    "\u0002\u0002.1\u0003\u0002\u0002\u0002/-\u0003\u0002\u0002\u0002/0\u0003",
    "\u0002\u0002\u00020\t\u0003\u0002\u0002\u00021/\u0003\u0002\u0002\u0002",
    "2?\u0007\u001f\u0002\u000235\u0007\u0006\u0002\u000246\u0005\u000e\b",
    "\u000254\u0003\u0002\u0002\u000256\u0003\u0002\u0002\u000267\u0003\u0002",
    "\u0002\u00027=\u0007\u0007\u0002\u00028:\u0007\u0006\u0002\u00029;\u0005",
    "\u000e\b\u0002:9\u0003\u0002\u0002\u0002:;\u0003\u0002\u0002\u0002;",
    "<\u0003\u0002\u0002\u0002<>\u0007\u0007\u0002\u0002=8\u0003\u0002\u0002",
    "\u0002=>\u0003\u0002\u0002\u0002>@\u0003\u0002\u0002\u0002?3\u0003\u0002",
    "\u0002\u0002?@\u0003\u0002\u0002\u0002@C\u0003\u0002\u0002\u0002AB\u0007",
    "\b\u0002\u0002BD\u0005\u000e\b\u0002CA\u0003\u0002\u0002\u0002CD\u0003",
    "\u0002\u0002\u0002DE\u0003\u0002\u0002\u0002EF\b\u0006\u0001\u0002F",
    "\u000b\u0003\u0002\u0002\u0002GM\u0007\u000e\u0002\u0002HM\u0007\n\u0002",
    "\u0002IM\u0007\u000f\u0002\u0002JM\u0007\r\u0002\u0002KM\u0007\f\u0002",
    "\u0002LG\u0003\u0002\u0002\u0002LH\u0003\u0002\u0002\u0002LI\u0003\u0002",
    "\u0002\u0002LJ\u0003\u0002\u0002\u0002LK\u0003\u0002\u0002\u0002MN\u0003",
    "\u0002\u0002\u0002NO\b\u0007\u0001\u0002O\r\u0003\u0002\u0002\u0002",
    "PQ\t\u0002\u0002\u0002Q\u000f\u0003\u0002\u0002\u0002\u000b\u001a#/",
    "5:=?CL"].join("");


var atn = new antlr4.atn.ATNDeserializer().deserialize(serializedATN);

var decisionsToDFA = atn.decisionToState.map( function(ds, index) { return new antlr4.dfa.DFA(ds, index); });

var sharedContextCache = new antlr4.PredictionContextCache();

var literalNames = [ null, "'{'", "'}'", "','", "'['", "']'", "'='", "'programa'", 
                     "'real'", "'vazio'", "'logico'", "'cadeia'", "'inteiro'", 
                     "'caracter'", "'escolha'", "'caso'", "'contrario'", 
                     "'const'", "'funcao'", "'retorne'", "'para'", "'pare'", 
                     "'faca'", "'enquanto'", "'se'", "'senao'", null, "'nao'" ];

var symbolicNames = [ null, null, null, null, null, null, null, "PR_PROGRAMA", 
                      "PR_REAL", "PR_VAZIO", "PR_LOGICO", "PR_CADEIA", "PR_INTEIRO", 
                      "PR_CARACTER", "PR_ESCOLHA", "PR_CASO", "PR_CONTRARIO", 
                      "PR_CONST", "PR_FUNCAO", "PR_RETORNE", "PR_PARA", 
                      "PR_PARE", "PR_FACA", "PR_ENQUANTO", "PR_SE", "PR_SENAO", 
                      "GAMBIARRA", "OPERADOR_NAO", "LOGICO", "ID", "ID_BIBLIOTECA", 
                      "INTEIRO", "REAL", "CADEIA", "CARACTER", "ESPACO", 
                      "COMENTARIO" ];

var ruleNames =  [ "parse", "programa", "declaracoesGlobais", "listaDeclaracoes", 
                   "declaracao", "declaracaoTipoDado", "expressao" ];

function ivprogParser (input) {
	antlr4.Parser.call(this, input);
    this._interp = new antlr4.atn.ParserATNSimulator(this, atn, decisionsToDFA, sharedContextCache);
    this.ruleNames = ruleNames;
    this.literalNames = literalNames;
    this.symbolicNames = symbolicNames;

	  this.criarTrechoCodigoFonte = function(tokenAntlr)
	    {
	      if (tokenAntlr != null)
	      {
	        const linha = tokenAntlr.getLine();
	        const coluna = tokenAntlr.getCharPositionInLine();
	        const tamanhoTexto = tokenAntlr.getText().length();
	        
	        return {linha: linha, coluna: coluna, tamanhoTexto: tamanhoTexto};
	      }
	      
	      return null;
	  }

    return this;
}

ivprogParser.prototype = Object.create(antlr4.Parser.prototype);
ivprogParser.prototype.constructor = ivprogParser;

Object.defineProperty(ivprogParser.prototype, "atn", {
	get : function() {
		return atn;
	}
});

ivprogParser.EOF = antlr4.Token.EOF;
ivprogParser.T__0 = 1;
ivprogParser.T__1 = 2;
ivprogParser.T__2 = 3;
ivprogParser.T__3 = 4;
ivprogParser.T__4 = 5;
ivprogParser.T__5 = 6;
ivprogParser.PR_PROGRAMA = 7;
ivprogParser.PR_REAL = 8;
ivprogParser.PR_VAZIO = 9;
ivprogParser.PR_LOGICO = 10;
ivprogParser.PR_CADEIA = 11;
ivprogParser.PR_INTEIRO = 12;
ivprogParser.PR_CARACTER = 13;
ivprogParser.PR_ESCOLHA = 14;
ivprogParser.PR_CASO = 15;
ivprogParser.PR_CONTRARIO = 16;
ivprogParser.PR_CONST = 17;
ivprogParser.PR_FUNCAO = 18;
ivprogParser.PR_RETORNE = 19;
ivprogParser.PR_PARA = 20;
ivprogParser.PR_PARE = 21;
ivprogParser.PR_FACA = 22;
ivprogParser.PR_ENQUANTO = 23;
ivprogParser.PR_SE = 24;
ivprogParser.PR_SENAO = 25;
ivprogParser.GAMBIARRA = 26;
ivprogParser.OPERADOR_NAO = 27;
ivprogParser.LOGICO = 28;
ivprogParser.ID = 29;
ivprogParser.ID_BIBLIOTECA = 30;
ivprogParser.INTEIRO = 31;
ivprogParser.REAL = 32;
ivprogParser.CADEIA = 33;
ivprogParser.CARACTER = 34;
ivprogParser.ESPACO = 35;
ivprogParser.COMENTARIO = 36;

ivprogParser.RULE_parse = 0;
ivprogParser.RULE_programa = 1;
ivprogParser.RULE_declaracoesGlobais = 2;
ivprogParser.RULE_listaDeclaracoes = 3;
ivprogParser.RULE_declaracao = 4;
ivprogParser.RULE_declaracaoTipoDado = 5;
ivprogParser.RULE_expressao = 6;

function ParseContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_parse;
    this.asa = null
    this.prog = null; // ProgramaContext
    return this;
}

ParseContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ParseContext.prototype.constructor = ParseContext;

ParseContext.prototype.EOF = function() {
    return this.getToken(ivprogParser.EOF, 0);
};

ParseContext.prototype.programa = function() {
    return this.getTypedRuleContext(ProgramaContext,0);
};

ParseContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterParse(this);
	}
};

ParseContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitParse(this);
	}
};




ivprogParser.ParseContext = ParseContext;

ivprogParser.prototype.parse = function() {

    var localctx = new ParseContext(this, this._ctx, this.state);
    this.enterRule(localctx, 0, ivprogParser.RULE_parse);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 14;
        localctx.prog = this.programa();
        this.state = 15;
        this.match(ivprogParser.EOF);

            localctx.asa =  localctx.prog.asa
          
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ProgramaContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_programa;
    this.asa = null
    return this;
}

ProgramaContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ProgramaContext.prototype.constructor = ProgramaContext;

ProgramaContext.prototype.PR_PROGRAMA = function() {
    return this.getToken(ivprogParser.PR_PROGRAMA, 0);
};

ProgramaContext.prototype.declaracoesGlobais = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(DeclaracoesGlobaisContext);
    } else {
        return this.getTypedRuleContext(DeclaracoesGlobaisContext,i);
    }
};

ProgramaContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterPrograma(this);
	}
};

ProgramaContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitPrograma(this);
	}
};




ivprogParser.ProgramaContext = ProgramaContext;

ivprogParser.prototype.programa = function() {

    var localctx = new ProgramaContext(this, this._ctx, this.state);
    this.enterRule(localctx, 2, ivprogParser.RULE_programa);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 18;
        this.match(ivprogParser.PR_PROGRAMA);
        this.state = 19;
        this.match(ivprogParser.T__0);

              localctx.asa =  new ASA()
            
        this.state = 24;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while((((_la) & ~0x1f) == 0 && ((1 << _la) & ((1 << ivprogParser.PR_REAL) | (1 << ivprogParser.PR_LOGICO) | (1 << ivprogParser.PR_CADEIA) | (1 << ivprogParser.PR_INTEIRO) | (1 << ivprogParser.PR_CARACTER) | (1 << ivprogParser.PR_CONST))) !== 0)) {
            this.state = 21;
            this.declaracoesGlobais(localctx.asa);
            this.state = 26;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
        this.state = 27;
        this.match(ivprogParser.T__1);
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function DeclaracoesGlobaisContext(parser, parent, invokingState, asa) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_declaracoesGlobais;
    this.asa = null
    this.vList = null; // ListaDeclaracoesContext
    this.asa = asa || null;
    return this;
}

DeclaracoesGlobaisContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DeclaracoesGlobaisContext.prototype.constructor = DeclaracoesGlobaisContext;

DeclaracoesGlobaisContext.prototype.listaDeclaracoes = function() {
    return this.getTypedRuleContext(ListaDeclaracoesContext,0);
};

DeclaracoesGlobaisContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterDeclaracoesGlobais(this);
	}
};

DeclaracoesGlobaisContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitDeclaracoesGlobais(this);
	}
};




ivprogParser.DeclaracoesGlobaisContext = DeclaracoesGlobaisContext;

ivprogParser.prototype.declaracoesGlobais = function(asa) {

    var localctx = new DeclaracoesGlobaisContext(this, this._ctx, this.state, asa);
    this.enterRule(localctx, 4, ivprogParser.RULE_declaracoesGlobais);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 29;
        localctx.vList = this.listaDeclaracoes();

            if (localctx.asa != null) {
              this.globais = new NoGlobal();
              localctx.vList.lista.forEach( v => this.globais.addDeclaracao(v));
              localctx.asa.nos.push(localctx.vList.lista);
            }
          
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ListaDeclaracoesContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_listaDeclaracoes;
    this.lista = null
    this.tokenConst = null; // Token
    this.informacaoTipoDado = null; // DeclaracaoTipoDadoContext
    this.vDeclaracao = null; // DeclaracaoContext
    return this;
}

ListaDeclaracoesContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ListaDeclaracoesContext.prototype.constructor = ListaDeclaracoesContext;

ListaDeclaracoesContext.prototype.declaracaoTipoDado = function() {
    return this.getTypedRuleContext(DeclaracaoTipoDadoContext,0);
};

ListaDeclaracoesContext.prototype.declaracao = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(DeclaracaoContext);
    } else {
        return this.getTypedRuleContext(DeclaracaoContext,i);
    }
};

ListaDeclaracoesContext.prototype.PR_CONST = function() {
    return this.getToken(ivprogParser.PR_CONST, 0);
};

ListaDeclaracoesContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterListaDeclaracoes(this);
	}
};

ListaDeclaracoesContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitListaDeclaracoes(this);
	}
};




ivprogParser.ListaDeclaracoesContext = ListaDeclaracoesContext;

ivprogParser.prototype.listaDeclaracoes = function() {

    var localctx = new ListaDeclaracoesContext(this, this._ctx, this.state);
    this.enterRule(localctx, 6, ivprogParser.RULE_listaDeclaracoes);

      localctx.lista =  []

    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 33;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===ivprogParser.PR_CONST) {
            this.state = 32;
            localctx.tokenConst = this.match(ivprogParser.PR_CONST);
        }

        this.state = 35;
        localctx.informacaoTipoDado = this.declaracaoTipoDado();

        this.state = 36;
        localctx.vDeclaracao = this.declaracao(localctx.tokenConst, localctx.informacaoTipoDado.informacaoTipoDado);

              if(localctx.vDeclaracao) {
                localctx.lista.push(localctx.vDeclaracao.rDeclaracao);
              }
              localctx.vDeclaracao = null;
            
        this.state = 45;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        while(_la===ivprogParser.T__2) {
            this.state = 39;
            this.match(ivprogParser.T__2);
            this.state = 40;
            localctx.vDeclaracao = this.declaracao(localctx.tokenConst, localctx.informacaoTipoDado.informacaoTipoDado);

                  if(localctx.vDeclaracao) {
                    localctx.lista.push(localctx.vDeclaracao.rDeclaracao);
                  }
                  localctx.vDeclaracao = null;
                
            this.state = 47;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function DeclaracaoContext(parser, parent, invokingState, tokenConst, informacaoTipoDado) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_declaracao;
    this.tokenConst = null
    this.informacaoTipoDado = null
    this.rDeclaracao = null
    this._ID = null; // Token
    this.tk1 = null; // Token
    this.ind1 = null; // ExpressaoContext
    this.tk2 = null; // Token
    this.ind2 = null; // ExpressaoContext
    this.inicializacao = null; // ExpressaoContext
    this.tokenConst = tokenConst || null;
    this.informacaoTipoDado = informacaoTipoDado || null;
    return this;
}

DeclaracaoContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DeclaracaoContext.prototype.constructor = DeclaracaoContext;

DeclaracaoContext.prototype.ID = function() {
    return this.getToken(ivprogParser.ID, 0);
};

DeclaracaoContext.prototype.expressao = function(i) {
    if(i===undefined) {
        i = null;
    }
    if(i===null) {
        return this.getTypedRuleContexts(ExpressaoContext);
    } else {
        return this.getTypedRuleContext(ExpressaoContext,i);
    }
};

DeclaracaoContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterDeclaracao(this);
	}
};

DeclaracaoContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitDeclaracao(this);
	}
};




ivprogParser.DeclaracaoContext = DeclaracaoContext;

ivprogParser.prototype.declaracao = function(tokenConst, informacaoTipoDado) {

    var localctx = new DeclaracaoContext(this, this._ctx, this.state, tokenConst, informacaoTipoDado);
    this.enterRule(localctx, 8, ivprogParser.RULE_declaracao);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 48;
        localctx._ID = this.match(ivprogParser.ID);
        this.state = 61;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===ivprogParser.T__3) {
            this.state = 49;
            localctx.tk1 = this.match(ivprogParser.T__3);
            this.state = 51;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===ivprogParser.INTEIRO || _la===ivprogParser.CADEIA) {
                this.state = 50;
                localctx.ind1 = this.expressao();
            }

            this.state = 53;
            this.match(ivprogParser.T__4);
            this.state = 59;
            this._errHandler.sync(this);
            _la = this._input.LA(1);
            if(_la===ivprogParser.T__3) {
                this.state = 54;
                localctx.tk2 = this.match(ivprogParser.T__3);
                this.state = 56;
                this._errHandler.sync(this);
                _la = this._input.LA(1);
                if(_la===ivprogParser.INTEIRO || _la===ivprogParser.CADEIA) {
                    this.state = 55;
                    localctx.ind2 = this.expressao();
                }

                this.state = 58;
                this.match(ivprogParser.T__4);
            }

        }

        this.state = 65;
        this._errHandler.sync(this);
        _la = this._input.LA(1);
        if(_la===ivprogParser.T__5) {
            this.state = 63;
            this.match(ivprogParser.T__5);
            this.state = 64;
            localctx.inicializacao = this.expressao();
        }


            const constante = (localctx.tokenConst != null);
            const tipoDado = (localctx.informacaoTipoDado != null)? localctx.informacaoTipoDado.tipoDado : null;
            const nome = (localctx._ID != null)? (localctx._ID===null ? null : localctx._ID.text) : null;
            
            if ((localctx.tk1 == null) && (localctx.tk2 == null))
              localctx.rDeclaracao =  new NoDeclaracaoVariavel(nome, tipoDado, constante)
            else
            
            if ((localctx.tk1 != null) && (localctx.tk2 == null))
              localctx.rDeclaracao =  new NoDeclaracaoVetor(nome, tipoDado, (localctx.ind1===null ? null : this._input.getText(new antlr4.Interval(localctx.ind1.start,localctx.ind1.stop))), constante)
            
            else
            
            if ((localctx.tk1 != null) && (localctx.tk2 != null))
              localctx.rDeclaracao =  new NoDeclaracaoMatriz(nome, tipoDado, (localctx.ind1===null ? null : this._input.getText(new antlr4.Interval(localctx.ind1.start,localctx.ind1.stop))), (localctx.ind2===null ? null : this._input.getText(new antlr4.Interval(localctx.ind2.start,localctx.ind2.stop))), constante)
          
            localctx.rDeclaracao.setInicializacao(inicializacao);
            localctx.rDeclaracao.setTrechoCodigoFonteNome(criarTrechoCodigoFonte(localctx._ID));
            localctx.rDeclaracao.setTrechoCodigoFonteTipoDado((localctx.informacaoTipoDado != null)? localctx.informacaoTipoDado.getTrechoCodigoFonte(): null);
          
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function DeclaracaoTipoDadoContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_declaracaoTipoDado;
    this.informacaoTipoDado = null
    this.tokenTipoDado = null; // Token
    return this;
}

DeclaracaoTipoDadoContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
DeclaracaoTipoDadoContext.prototype.constructor = DeclaracaoTipoDadoContext;

DeclaracaoTipoDadoContext.prototype.PR_INTEIRO = function() {
    return this.getToken(ivprogParser.PR_INTEIRO, 0);
};

DeclaracaoTipoDadoContext.prototype.PR_REAL = function() {
    return this.getToken(ivprogParser.PR_REAL, 0);
};

DeclaracaoTipoDadoContext.prototype.PR_CARACTER = function() {
    return this.getToken(ivprogParser.PR_CARACTER, 0);
};

DeclaracaoTipoDadoContext.prototype.PR_CADEIA = function() {
    return this.getToken(ivprogParser.PR_CADEIA, 0);
};

DeclaracaoTipoDadoContext.prototype.PR_LOGICO = function() {
    return this.getToken(ivprogParser.PR_LOGICO, 0);
};

DeclaracaoTipoDadoContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterDeclaracaoTipoDado(this);
	}
};

DeclaracaoTipoDadoContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitDeclaracaoTipoDado(this);
	}
};




ivprogParser.DeclaracaoTipoDadoContext = DeclaracaoTipoDadoContext;

ivprogParser.prototype.declaracaoTipoDado = function() {

    var localctx = new DeclaracaoTipoDadoContext(this, this._ctx, this.state);
    this.enterRule(localctx, 10, ivprogParser.RULE_declaracaoTipoDado);
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 74;
        this._errHandler.sync(this);
        switch(this._input.LA(1)) {
        case ivprogParser.PR_INTEIRO:
            this.state = 69;
            localctx.tokenTipoDado = this.match(ivprogParser.PR_INTEIRO);
            break;
        case ivprogParser.PR_REAL:
            this.state = 70;
            localctx.tokenTipoDado = this.match(ivprogParser.PR_REAL);
            break;
        case ivprogParser.PR_CARACTER:
            this.state = 71;
            localctx.tokenTipoDado = this.match(ivprogParser.PR_CARACTER);
            break;
        case ivprogParser.PR_CADEIA:
            this.state = 72;
            localctx.tokenTipoDado = this.match(ivprogParser.PR_CADEIA);
            break;
        case ivprogParser.PR_LOGICO:
            this.state = 73;
            localctx.tokenTipoDado = this.match(ivprogParser.PR_LOGICO);
            break;
        default:
            throw new antlr4.error.NoViableAltException(this);
        }

            localctx.informacaoTipoDado =  new InformacaoTipoDado()
            localctx.informacaoTipoDado.setTipoDado(localctx.tokenTipoDado);
            localctx.informacaoTipoDado.setTrechoCodigoFonte(criarTrechoCodigoFonte(localctx.tokenTipoDado));
          
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};

function ExpressaoContext(parser, parent, invokingState) {
	if(parent===undefined) {
	    parent = null;
	}
	if(invokingState===undefined || invokingState===null) {
		invokingState = -1;
	}
	antlr4.ParserRuleContext.call(this, parent, invokingState);
    this.parser = parser;
    this.ruleIndex = ivprogParser.RULE_expressao;
    return this;
}

ExpressaoContext.prototype = Object.create(antlr4.ParserRuleContext.prototype);
ExpressaoContext.prototype.constructor = ExpressaoContext;

ExpressaoContext.prototype.INTEIRO = function() {
    return this.getToken(ivprogParser.INTEIRO, 0);
};

ExpressaoContext.prototype.CADEIA = function() {
    return this.getToken(ivprogParser.CADEIA, 0);
};

ExpressaoContext.prototype.enterRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.enterExpressao(this);
	}
};

ExpressaoContext.prototype.exitRule = function(listener) {
    if(listener instanceof ivprogListener ) {
        listener.exitExpressao(this);
	}
};




ivprogParser.ExpressaoContext = ExpressaoContext;

ivprogParser.prototype.expressao = function() {

    var localctx = new ExpressaoContext(this, this._ctx, this.state);
    this.enterRule(localctx, 12, ivprogParser.RULE_expressao);
    var _la = 0; // Token type
    try {
        this.enterOuterAlt(localctx, 1);
        this.state = 78;
        _la = this._input.LA(1);
        if(!(_la===ivprogParser.INTEIRO || _la===ivprogParser.CADEIA)) {
        this._errHandler.recoverInline(this);
        }
        else {
        	this._errHandler.reportMatch(this);
            this.consume();
        }
    } catch (re) {
    	if(re instanceof antlr4.error.RecognitionException) {
	        localctx.exception = re;
	        this._errHandler.reportError(this, re);
	        this._errHandler.recover(this, re);
	    } else {
	    	throw re;
	    }
    } finally {
        this.exitRule();
    }
    return localctx;
};


exports.ivprogParser = ivprogParser;

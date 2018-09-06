import { Types } from './../ast/types';

const tiposComandos = Object.freeze({comment:"comment", reader:"reader", writer:"writer"});

export class Variavel {

  constructor (tipo, nome, valor, dimensoes = 0, eh_constante = false, linhas = 0, colunas = 0) {
    this.tipo = tipo;
    this.nome = nome;
    this.valor = valor;
    this.dimensoes = dimensoes;
    this.eh_constante = eh_constante;
    this.linhas = linhas;
    this.colunas = colunas;
  }
}

export class Funcao {

  constructor (nome, tipo_retorno = Types.VOID, dimensoes_retorno = 0, lista_parametros = null, eh_principal = false, esta_oculta = false, variaveis = [], comentario_funcao = null) {
    this.nome = nome;
    this.tipo_retorno = tipo_retorno;
    this.dimensoes_retorno = dimensoes_retorno;
    this.lista_parametros = lista_parametros;
    this.eh_principal = eh_principal;
    this.esta_oculta = esta_oculta;
    this.variaveis = variaveis;
    this.comentario_funcao = comentario_funcao;
    this.comandos = [];
  }
}

export class Comentario {
  
  constructor (texto_comentario) {
    this.tipo = tiposComandos.comment;
    this.texto_comentario = texto_comentario;
  }
}

export class Comando {

  constructor (tipo) {
    this.tipo = tipo;
  }
} 

export class Expressao {

  constructor (conteudo) {
    this.conteudo = conteudo;
  }
}


export class Programa {
  constructor () {
    this.funcoes = [];
    this.globais = [];
  };

  adicionarFuncao (funcao) {
    this.funcoes.push(funcao);
  }

  adicionarVariavel(funcao, variavel) {
    if (this.funcoes[funcao].variaveis === null) {
      this.funcoes[funcao].variaveis = [];
    }
    this.funcoes[funcao].variaveis.push(variavel);
  }

  adicionarGlobal (variavel) {
    this.globais.push(variavel);
  }
}

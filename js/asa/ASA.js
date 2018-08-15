export class ASA {

  constructor() {
    this.nos = [];
  }
}

/*
  Raiz(ASA)
    |
    |
  DeclaraçõesGlobais + Funções

  DeclaracaoGlobal
    |
    |
  const? TIPO ID (= EAnd)?

  Função
    |
    |
  Declaracao => TIPO ID (= EAnd)?
  Attribuição => ID = EAnd
  IF, WHILE, SWITCH, FuncCall, RETURN

  EAnd
    |
    |
  EOR ( 'and' EAnd)?

  EOR   => ENot ('or' EAnd)?

  ENot  => 'not'? ER

  ER    => E ((>=, <=, ==, >, <) E)?

  E     => factor ((+, -) E)?

  factor=> term ((*, /, %) factor)?

  term  => literal || arrayAccess || FuncCall || ID || '('EAnd')'

  arrayAccess
    |
    |
  ID'['E']'('['E']')*

  FuncCall
    |
    |
  ID'('p.a')'

  p.a => E (, p.a)?
**/
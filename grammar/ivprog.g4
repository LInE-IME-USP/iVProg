grammar ivprog;
@parser::header
{
  import {ASA, NoGlobal} from '../js/asa';
}

@parser::members
{
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
}



PR_PROGRAMA     : 'programa'    ;
PR_REAL       : 'real'    ;
PR_VAZIO        : 'vazio'   ;
PR_LOGICO       : 'logico'    ;
PR_CADEIA       : 'cadeia'    ;
PR_INTEIRO        : 'inteiro'   ;
PR_CARACTER     : 'caracter'    ;    
PR_ESCOLHA        : 'escolha'   ;
PR_CASO       : 'caso'    ;
PR_CONTRARIO      : 'contrario' ;
PR_CONST        : 'const'   ;
PR_FUNCAO       : 'funcao'    ;
PR_RETORNE        : 'retorne'   ;  
PR_PARA       : 'para'    ;
PR_PARE       : 'pare'    ;
PR_FACA       : 'faca'    ;
PR_ENQUANTO     : 'enquanto'    ;
PR_SE       : 'se'    ;
PR_SENAO        : 'senao'   ;


GAMBIARRA   : '.' |'á'| 'à'| 'ã'|'â'|'é'|'ê'|'í'|'ó'|'ô'|'õ'|'ú'|'ü'|'ç'|'Ä'|'À'|'Ã'|'Â'|'É'|'Ê'|'Ë'|'Ó'|'Ô'|'Õ'|'Ú'|'Ü'|'Ç'|'#'|'$'|'"'|'§'|'?'|'¹'|'²'|'³'|'£'|'¢'|'¬'|'ª'|'º'|'~'|'\''|'`'|'\\'|'@';
 
fragment PR_FALSO     : 'falso'   ;
fragment PR_VERDADEIRO    : 'verdadeiro'    ;

OPERADOR_NAO      : 'nao'   ;

LOGICO        :   PR_VERDADEIRO | PR_FALSO  ;

ID        : ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*  ;

ID_BIBLIOTECA     : ID '.' ID;

INTEIRO         : '0'..'9'+ | ('0x'|'0X')(DIGIT_HEX)+ | ('0b'|'0B')('0'|'1')+;

REAL          :   ('0'..'9')+ '.' ('0'..'9')+ ;
    
CADEIA        : '"' ( SEQ_ESC | ~('\\'|'"') )* '"'  ;

CARACTER        :   '\'' ( SEQ_ESC | ~('\''|'\\') ) '\''  ;

ESPACO        : ( ' ' | '\t' | '\r' | '\n') -> skip  ;


fragment DIGIT_HEX    :   ('0'..'9'|'a'..'f'|'A'..'F')  ;

fragment SEQ_ESC      :  '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')  |   ESC_UNICODE  |   ESC_OCTAL   ;

fragment ESC_OCTAL    : '\\' ('0'..'3') ('0'..'7') ('0'..'7')  |   '\\' ('0'..'7') ('0'..'7')    |   '\\' ('0'..'7')    ;

fragment ESC_UNICODE    : '\\' 'u' DIGIT_HEX DIGIT_HEX DIGIT_HEX DIGIT_HEX  ;

COMENTARIO      :   

  ('//' ~('\n'|'\r')* '\r'? '\n' |  
  
   '/*' ( options {greedy=false;} : . )* '*/') -> skip
 ;


parse returns [asa]:

  prog = programa EOF
  {
    $asa = $prog.asa;
  }
;


programa returns [asa] :

  PR_PROGRAMA
  '{'   
    {
      $asa = new ASA();
    }

    (declaracoesGlobais[$asa])*
  '}'
;

declaracoesGlobais [asa]:
  vList = listaDeclaracoes
  {
    if ($asa != null) {
      this.globais = new NoGlobal();
      $vList.lista.forEach( v => this.globais.addDeclaracao(v));
      $asa.nos.push($vList.lista);
    }
  }
;

listaDeclaracoes returns [lista]
@init{
  $lista = [];
}:
  (tokenConst = PR_CONST)? informacaoTipoDado = declaracaoTipoDado

  ( vDeclaracao = declaracao[$tokenConst, $informacaoTipoDado.informacaoTipoDado]
    {
      if($ctx.vDeclaracao) {
        $lista.push($vDeclaracao.rDeclaracao);
      }
      $ctx.vDeclaracao = null;
    }
  )
  ( ',' vDeclaracao = declaracao[$tokenConst, $informacaoTipoDado.informacaoTipoDado]
    {
      if($ctx.vDeclaracao) {
        $lista.push($vDeclaracao.rDeclaracao);
      }
      $ctx.vDeclaracao = null;
    }
  )*
;

declaracao [tokenConst, informacaoTipoDado] returns[rDeclaracao]:  

  (ID (tk1 = '[' (ind1 = expressao)? ']' (tk2 = '[' (ind2 = expressao)? ']')?)? ('=' inicializacao = expressao)?) 
  {
    const constante = ($tokenConst != null);
    const tipoDado = ($informacaoTipoDado != null)? $informacaoTipoDado.tipoDado : null;
    const nome = ($ID != null)? $ID.text : null;
    
    if (($tk1 == null) && ($tk2 == null))
      $rDeclaracao = new NoDeclaracaoVariavel(nome, tipoDado, constante);
    else
    
    if (($tk1 != null) && ($tk2 == null))
      $rDeclaracao = new NoDeclaracaoVetor(nome, tipoDado, $ind1.text, constante);
    
    else
    
    if (($tk1 != null) && ($tk2 != null))
      $rDeclaracao = new NoDeclaracaoMatriz(nome, tipoDado, $ind1.text, $ind2.text, constante);
  
    $rDeclaracao.setInicializacao(inicializacao);
    $rDeclaracao.setTrechoCodigoFonteNome(criarTrechoCodigoFonte($ID));
    $rDeclaracao.setTrechoCodigoFonteTipoDado(($informacaoTipoDado != null)? $informacaoTipoDado.getTrechoCodigoFonte(): null);
  }
;

declaracaoTipoDado returns[informacaoTipoDado]:

  (tokenTipoDado = PR_INTEIRO | tokenTipoDado = PR_REAL | tokenTipoDado = PR_CARACTER | tokenTipoDado = PR_CADEIA | tokenTipoDado = PR_LOGICO)
  {
    $informacaoTipoDado = new InformacaoTipoDado();
    $informacaoTipoDado.setTipoDado($tokenTipoDado);
    $informacaoTipoDado.setTrechoCodigoFonte(criarTrechoCodigoFonte($tokenTipoDado));
  }
;

expressao:
  INTEIRO
  | CADEIA
;
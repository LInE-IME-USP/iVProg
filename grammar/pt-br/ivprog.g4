lexer grammar ivprog;

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

ABRE_PAR: '(';
FECHA_PAR: ')';
ABRE_COL: '[';
FECHA_COL: ']';
ABRE_CHA: '{';
FECHA_CHA: '}';
VIRGULA: ',';
PONTO_VIRGULA: ';';
ATRIBUICAO: '=';
OPERADOR_ARITMETICO: ('+'|'-'|'*'|'/'|'%');
OPERADOR_E: 'E';
OPERADOR_OU: 'OU';
OPERADOR_RELACIONAL: ('>='|'=='|'<='|'>'|'<');
DPONTOS: ':';

OPERADOR_NAO      : 'nao'   ;

LOGICO        :   PR_VERDADEIRO | PR_FALSO  ;

ID        : ('a'..'z'|'A'..'Z'|'_') ('a'..'z'|'A'..'Z'|'0'..'9'|'_')*  ;

// ID_BIBLIOTECA     : ID '.' ID;

INTEIRO         : '0'..'9'+ | ('0x'|'0X')(DIGIT_HEX)+ | ('0b'|'0B')('0'|'1')+;

REAL          :   ('0'..'9')+ '.' ('0'..'9')+ ;
    
CADEIA        : '"' ( SEQ_ESC | ~('\\'|'"') )* '"'  ;

CARACTER        :   '\'' ( SEQ_ESC | ~('\''|'\\') ) '\''  ;

ESPACO        : ( ' ' | '\t' | '\r' | '\n') -> skip  ;


fragment DIGIT_HEX    :   ('0'..'9'|'a'..'f'|'A'..'F')  ;

fragment SEQ_ESC      :  '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')  |   ESC_UNICODE  |   ESC_OCTAL   ;

fragment ESC_OCTAL    : '\\' ('0'..'3') ('0'..'7') ('0'..'7')  |   '\\' ('0'..'7') ('0'..'7')    |   '\\' ('0'..'7')    ;

fragment ESC_UNICODE    : '\\' 'u' DIGIT_HEX DIGIT_HEX DIGIT_HEX DIGIT_HEX  ;

COMENTARIO      :   

  ('//' ~('\n'|'\r')* '\r'? '\n' |  
  
  '/*' .*? '*/') -> channel(HIDDEN)
 ;

lexer grammar ivprog;

PR_PROGRAMA
  : 'programa'
  ;

PR_REAL
  : 'real'
  ;

PR_VAZIO
  : 'vazio'
  ;

PR_LOGICO
  : 'logico'
  ;

PR_CADEIA
  : 'cadeia'
  ;

PR_INTEIRO
  : 'inteiro'
  ;

PR_CARACTER
  : 'caractere'
  ;    

PR_ESCOLHA
  : 'escolha'
  ;

PR_CASO
  : 'caso'
  ;

PR_CONTRARIO
  : 'contrario'
  ;

PR_CONST
  : 'const'
  ;

PR_FUNCAO
  : 'funcao'
  ;

PR_RETORNE
  : 'retorne'
  ;  

PR_PARA
  : 'para'
  ;

PR_PARE
  : 'pare'
  ;

PR_FACA
  : 'faca'
  ;

PR_ENQUANTO
  : 'enquanto'
  ;

PR_SE
  : 'se'
  ;

PR_SENAO
  : 'senao'
  ;

fragment PR_FALSO
  : 'falso'
  ;

fragment PR_VERDADEIRO
  : 'verdadeiro'
  ;

fragment PR_NAO_LOGICO
  : 'nao'
  ;

fragment PR_E_LOGICO
  : 'E'
  ;

fragment PR_OU_LOGICO
  : 'OU'
  ;

// GAMBIARRA   : '.' |'á'| 'à'| 'ã'|'â'|'é'|'ê'|'í'|'ó'|'ô'|'õ'|'ú'|'ü'|'ç'|'Ä'|'À'|'Ã'|'Â'|'É'|'Ê'|'Ë'|'Ó'|'Ô'|'Õ'|'Ú'|'Ü'|'Ç'|'#'|'$'|'"'|'§'|'?'|'¹'|'²'|'³'|'£'|'¢'|'¬'|'ª'|'º'|'~'|'\''|'`'|'\\'|'@';

ABRE_PAR
  : '('
  ;

FECHA_PAR
  : ')'
  ;

ABRE_COL
  : '['
  ;

FECHA_COL
  : ']'
  ;

ABRE_CHA
  : '{'
  ;

FECHA_CHA
  : '}'
  ;

VIRGULA
  : ','
  ;

ATRIBUICAO
  : '='
  ;

OPERADOR_SOMA
  : ('+'|'-')
  ;

OPERADOR_MULTIPLICATIVO
  : ('*'|'/'|'%')
  ;

OPERADOR_E
  : PR_E_LOGICO
  ;

OPERADOR_OU
  : PR_OU_LOGICO
  ;

OPERADOR_RELACIONAL
  : ('>='|'=='|'<='|'>'|'<')
  ;

DPONTOS
  : ':'
  ;

OPERADOR_NAO
  : PR_NAO_LOGICO
  ;

LOGICO
  : PR_VERDADEIRO
  | PR_FALSO
  ;

ID
  : [a-zA-Z_] [a-zA-Z0-9_]*
  ;

// ID_BIBLIOTECA     : ID '.' ID;

INTEIRO
  : [0-9]+
  | ('0x'|'0X')(DIGIT_HEX)+
  | ('0b'|'0B')[0-1]+
  ;

REAL
  : [0-9]+ '.' [0-9]+
  ;

CADEIA
  : '"' CADEIA_CARACTER* '"'
  ;
    
fragment CADEIA_CARACTER //String como definido em https://github.com/antlr/grammars-v4/blob/master/java8/Java8.g4
  : ~["\\\r\n]
  | SEQ_ESC
  ;

CARACTER //Caracter como definido em https://github.com/antlr/grammars-v4/blob/master/java8/Java8.g4
  : '\'' ( SEQ_ESC | ~['\\\r\n]) '\''
  ;

ESPACO 
  : ( ' ' | '\t') -> skip
  ;

fragment PONTO_VIRGULA
  : ';'
  ;

EOS
  : [\r\n]+
  | PONTO_VIRGULA
  ;

fragment DIGIT_HEX
  : [0-9a-fA-F]
  ;

fragment DIGIT_OCTAL
  : [0-7]
  ;

fragment SEQ_ESC
  : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')
  | ESC_UNICODE
  | ESC_OCTAL
  ;

fragment ESC_OCTAL
  : '\\' [0-3] DIGIT_OCTAL DIGIT_OCTAL
  | '\\' DIGIT_OCTAL DIGIT_OCTAL
  | '\\' DIGIT_OCTAL
  ;

fragment ESC_UNICODE
  : '\\' 'u' DIGIT_HEX DIGIT_HEX DIGIT_HEX DIGIT_HEX
  ;

COMENTARIO
  : ('//' ~('\n'|'\r')* '\r'? '\n'
    | '/*' .*? '*/') -> channel(HIDDEN)
  ;
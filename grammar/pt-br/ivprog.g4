lexer grammar ivprog;

@lexer::members{
  //Translate to fit your language
  ivprog.MAIN_FUNCTION_NAME = "inicio";
}

// BEGIN i18n Lexical rules
RK_PROGRAM
  : 'programa'
  ;

RK_REAL
  : 'real'
  ;

RK_VOID
  : 'vazio'
  ;

RK_BOOLEAN
  : 'logico'
  ;

RK_STRING
  : 'cadeia'
  ;

RK_INTEGER
  : 'inteiro'
  ;

RK_CHARACTER
  : 'caractere'
  ;    

RK_SWITCH
  : 'escolha'
  ;

RK_CASE
  : 'caso'
  ;

RK_DEFAULT
  : 'contrario'
  ;

RK_CONST
  : 'const'
  ;

RK_FUNCTION
  : 'funcao'
  ;

RK_RETURN
  : 'retorne'
  ;  

RK_FOR
  : 'para'
  ;

RK_BREAK
  : 'pare'
  ;

RK_DO
  : 'faca'
  ;

RK_WHILE
  : 'enquanto'
  ;

RK_IF
  : 'se'
  ;

RK_ELSE
  : 'senao'
  ;

RK_FALSE
  : 'falso'
  ;

RK_TRUE
  : 'verdadeiro'
  ;

fragment RK_LOGICAL_NOT
  : 'nao'
  ;

fragment RK_LOGICAL_AND
  : 'E'
  ;

fragment RK_LOGICAL_OR
  : 'OU'
  ;
// END i18n Lexical rules

// GAMBIARRA   : '.' |'á'| 'à'| 'ã'|'â'|'é'|'ê'|'í'|'ó'|'ô'|'õ'|'ú'|'ü'|'ç'|'Ä'|'À'|'Ã'|'Â'|'É'|'Ê'|'Ë'|'Ó'|'Ô'|'Õ'|'Ú'|'Ü'|'Ç'|'#'|'$'|'"'|'§'|'?'|'¹'|'²'|'³'|'£'|'¢'|'¬'|'ª'|'º'|'~'|'\''|'`'|'\\'|'@';

OPEN_PARENTHESIS
  : '('
  ;

CLOSE_PARENTHESIS
  : ')'
  ;

OPEN_BRACE
  : '['
  ;

CLOSE_BRACE
  : ']'
  ;

OPEN_CURLY
  : '{'
  ;

CLOSE_CURLY
  : '}'
  ;

COMMA
  : ','
  ;

EQUAL
  : '='
  ;

SUM_OP
  : ('+'|'-')
  ;

MULTI_OP
  : ('*'|'/'|'%')
  ;

AND_OPERATOR
  : RK_LOGICAL_AND
  ;

OR_OPERATOR
  : RK_LOGICAL_OR
  ;

RELATIONAL_OPERATOR
  : ('>='|'=='|'<='|'>'|'<')
  ;

COLON
  : ':'
  ;

NOT_OPERATOR
  : RK_LOGICAL_NOT
  ;

ID
  : [a-zA-Z_] [a-zA-Z0-9_]*
  ;

// ID_BIBLIOTECA     : ID '.' ID;

INTEGER
  : [0-9]+
  | ('0x'|'0X')(HEX_DIGIT)+
  | ('0b'|'0B')[0-1]+
  ;

REAL
  : [0-9]+ '.' [0-9]+
  ;

STRING
  : '"' STRING_CHARACTER* '"'
  ;
    
fragment STRING_CHARACTER //String como definido em https://github.com/antlr/grammars-v4/blob/master/java8/Java8.g4
  : ~["\\\r\n]
  | ESC_SEQ
  ;

CHARACTER //Caracter como definido em https://github.com/antlr/grammars-v4/blob/master/java8/Java8.g4
  : '\'' ( ESC_SEQ | ~['\\\r\n]) '\''
  ;

WHITESPACE 
  : ( ' ' | '\t') -> skip
  ;

fragment SEMICOLON
  : ';'
  ;

EOS
  : [\r\n]+
  | SEMICOLON
  ;

fragment HEX_DIGIT
  : [0-9a-fA-F]
  ;

fragment OCTAL_DIGIT
  : [0-7]
  ;

fragment ESC_SEQ
  : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')
  | ESC_UNICODE
  | ESC_OCTAL
  ;

fragment ESC_OCTAL
  : '\\' [0-3] OCTAL_DIGIT OCTAL_DIGIT
  | '\\' OCTAL_DIGIT OCTAL_DIGIT
  | '\\' OCTAL_DIGIT
  ;

fragment ESC_UNICODE
  : '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
  ;

COMMENTS
  : ('//' ~('\n'|'\r')* '\r'? '\n'
    | '/*' .*? '*/') -> channel(HIDDEN)
  ;

lexer grammar ivprog;

@lexer::members{
  //Translate to fit your language
  ivprog.MAIN_FUNCTION_NAME = "start";
  ivprog.READ_FUNCTION_NAME = "read";
  ivprog.WRITE_FUNCTION_NAME = "write";
}

// BEGIN i18n Lexical rules
RK_PROGRAM
  : 'program'
  ;

RK_REAL
  : 'real'
  ;

RK_VOID
  : 'void'
  ;

RK_BOOLEAN
  : 'boolean'
  ;

RK_STRING
  : 'string'
  ;

RK_INTEGER
  : 'integer'
  ;

RK_CHARACTER
  : 'character'
  ;    

RK_SWITCH
  : 'switch'
  ;

RK_CASE
  : 'case'
  ;

RK_DEFAULT
  : 'otherwise'
  ;

RK_CONST
  : 'const'
  ;

RK_FUNCTION
  : 'function'
  ;

RK_RETURN
  : 'return'
  ;  

RK_FOR
  : 'for'
  ;

RK_BREAK
  : 'break'
  ;

RK_DO
  : 'do'
  ;

RK_WHILE
  : 'while'
  ;

RK_IF
  : 'if'
  ;

RK_ELSE
  : 'else'
  ;

RK_FALSE
  : 'false'
  ;

RK_TRUE
  : 'true'
  ;

fragment RK_LOGICAL_NOT
  : 'not'
  ;

fragment RK_LOGICAL_AND
  : 'AND'
  ;

fragment RK_LOGICAL_OR
  : 'OR'
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
  : ('>='|'=='|'<='|'>'|'<'|'!=')
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
  | [0-9]+ '.' [0-9]* ExponentPart
  ;

fragment ExponentPart
  : [eE] [+-]? [0-9]+
  ;

STRING
  : '"' STRING_CHARACTER* '"'
  ;
    
fragment STRING_CHARACTER //String as defined at https://github.com/antlr/grammars-v4/blob/master/java8/Java8.g4
  : ~["\\\r\n]
  | ESC_SEQ
  ;

CHARACTER //Character as defined at https://github.com/antlr/grammars-v4/blob/master/java8/Java8.g4
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

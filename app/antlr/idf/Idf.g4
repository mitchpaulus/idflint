grammar Idf;

options {
    language=CSharp;
}

idf : (COMMENT | object)* EOF;

object : ALPHA FIELD_SEPARATOR COMMENT* fields ;

fields : field (FIELD_SEPARATOR COMMENT* field)* OBJECT_TERMINATOR COMMENT* ;

COMMENT :  '!' .*? '\r'?'\n' ;

FIELD_SEPARATOR   : ',' ;

OBJECT_TERMINATOR : ';' ;

NUMERIC : '-'?(([1-9][0-9]*|'0')('.'[0-9]*)? | ('.'[0-9]+)) ([eE]'-'?[0-9]+)? ;

ALPHA : ~[ \t,;!\r\n] (~[,;!\r\n])* ;

field : NUMERIC | ALPHA | ;

WS : [ \t\r\n]+ -> skip ;


grammar Idf;

options {
    language=CSharp;
}

idf : (COMMENT | object)* EOF;

object : ALPHA FIELD_SEPARATOR COMMENT* fields ;

fields : field (FIELD_SEPARATOR COMMENT* field)* OBJECT_TERMINATOR COMMENT* ;

COMMENT :  '!' .*? '\n' ;

FIELD_SEPARATOR   : ',' ;
OBJECT_TERMINATOR : ';' ;

/*OBJECT_TYPE : [a-zA-Z0-9:-]+ ;*/

NUMERIC : '-'?(([1-9][0-9]*|'0')('.'[0-9]+)? | ('.'[0-9]+))([eE]'-'?[0-9]+)? ;

ALPHA : [a-zA-Z0-9] ~[,;!]* [a-zA-Z0-9] ;

field : NUMERIC | ALPHA | ;

WS : [ \t\r\n]+ -> skip ;


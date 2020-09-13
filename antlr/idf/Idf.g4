grammar Idf;

options {
    language=CSharp;
}

idf : (COMMENT | object)* EOF;

object : object_type FIELD_SEPARATOR COMMENT* fields ;

fields : field (FIELD_SEPARATOR COMMENT* field)* OBJECT_TERMINATOR COMMENT* ;

COMMENT :  '!' .*? '\n' ;

FIELD_SEPARATOR   : ',' ;
OBJECT_TERMINATOR : ';' ;

NUMERIC : '-'?(([1-9][0-9]*|'0')('.'[0-9]+)? | ('.'[0-9]+))([eE]'-'?[0-9]+)? ;

ALPHA : [a-zA-Z0-9] ~[,!]* [a-zA-Z0-9] ;

field : NUMERIC | ALPHA ;

object_type : 'SimulationControl' | 'Building' ;

WS : [ \t\n] -> skip ;


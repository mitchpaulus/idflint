grammar Idd;

idd : ( COMMENT | group | NEWLINE )* ;

object : object_header fields;

object_header : OBJECT_NAME FIELD_SEPARATOR (COMMENT | NEWLINE)* object_properties ;

fields  : terminating_field
        | field* terminating_field
        ;

terminating_field : field_id OBJECT_TERMINATOR NEWLINE* object_properties ;

field : field_id FIELD_SEPARATOR NEWLINE* object_properties ;

field_id : ALPHA_OPTION | NUMERIC_OPTION ;

group : GROUPSTATEMENT (object | COMMENT)* ;

object_properties : (OBJECT_PROPERTY | COMMENT)* ;

GROUPSTATEMENT : '\\group ' .*? NEWLINE  ;

OBJECT_PROPERTY : '\\' .*? NEWLINE ;

ALPHA_OPTION : 'A'[1-9][0-9]* ;

NUMERIC_OPTION : 'N'[1-9][0-9]* ;

OBJECT_NAME : [A-Z][a-zA-Z0-9:-]+ ;

COMMENT : '!' .*? NEWLINE ;

FIELD_SEPARATOR : ',' ;

OBJECT_TERMINATOR : ';' ;

/*Grab as many newlines, including if there is trailing whitespace on previous lines*/
NEWLINE : ('\r'? '\n')([ ]*('\r'? '\n'))* ;

WS : [ \t]+ -> skip ;

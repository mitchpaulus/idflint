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

object_properties : (object_property | COMMENT)* ;

GROUPSTATEMENT : '\\group ' .*? NEWLINE  ;


object_property : 
    AUTOCALCULATABLE_STATEMENT
    | AUTOSIZABLE_STATEMENT
    | BEGIN_EXTENSIBLE_STATEMENT
    | DEFAULT_STATEMENT
    | DEPRECATED_STATEMENT
    | EXTENSIBLE_STATEMENT
    | EXTERNAL_LIST_STATEMENT
    | FIELD_STATEMENT
    | FORMAT_STATEMENT
    | GROUP_STATEMENT
    | IP_UNITS_STATEMENT
    | KEY_STATEMENT
    | MAXIMUM_STATEMENT
    | MEMO_STATEMENT
    | MIN_FIELDS_STATEMENT
    | MINIMUM_STATEMENT
    | NOTE_STATEMENT
    | OBJECT_LIST_STATEMENT
    | OBSOLETE_STATEMENT
    | REFERENCE_STATEMENT
    | REFERENCE_CLASS_NAME_STATEMENT
    | REQUIRED_FIELD_STATEMENT
    | REQUIRED_OBJECT_STATEMENT
    | RETAINCASE_STATEMENT
    | TYPE_STATEMENT
    | UNIQUE_OBJECT_STATEMENT
    | UNITS_STATEMENT
    | GENERAL_PROPERTY
    ;

AUTOCALCULATABLE_STATEMENT     : '\\autocalculatable' NEWLINE ;
AUTOSIZABLE_STATEMENT          : '\\autosizable' NEWLINE ;
BEGIN_EXTENSIBLE_STATEMENT     : '\\begin-extensible' NEWLINE ;
DEFAULT_STATEMENT              : '\\default ' .*? NEWLINE ;
DEPRECATED_STATEMENT           : '\\deprecated ' NEWLINE ;
EXTENSIBLE_STATEMENT           : '\\extensible' .*? NEWLINE ;
EXTERNAL_LIST_STATEMENT        : '\\external-list ' .*? NEWLINE ;
FIELD_STATEMENT                : '\\field ' .*? NEWLINE ;
FORMAT_STATEMENT               : '\\format ' .*? NEWLINE ;
GROUP_STATEMENT                : '\\group ' .*? NEWLINE ;
IP_UNITS_STATEMENT             : '\\ip-units ' .*? NEWLINE ;
KEY_STATEMENT                  : '\\key ' .*? NEWLINE ;
MAXIMUM_STATEMENT              : '\\maximum ' .*? NEWLINE ;
MEMO_STATEMENT                 : '\\memo ' .*? NEWLINE ;
MIN_FIELDS_STATEMENT           : '\\min-fields ' .*? NEWLINE ;
MINIMUM_STATEMENT              : '\\minimum ' .*? NEWLINE ;
NOTE_STATEMENT                 : '\\note ' .*? NEWLINE ;
OBJECT_LIST_STATEMENT          : '\\object-list ' .*? NEWLINE ;
OBSOLETE_STATEMENT             : '\\obsolete ' .*? NEWLINE ;
REFERENCE_STATEMENT            : '\\reference ' .*? NEWLINE ;
REFERENCE_CLASS_NAME_STATEMENT : '\\reference-class-name ' .*? NEWLINE ;
REQUIRED_FIELD_STATEMENT       : '\\required-field' NEWLINE ;
REQUIRED_OBJECT_STATEMENT      : '\\required-object' NEWLINE ;
RETAINCASE_STATEMENT           : '\\retaincase' NEWLINE ;
TYPE_STATEMENT                 : '\\type ' .*? NEWLINE ;
UNIQUE_OBJECT_STATEMENT        : '\\unique-object' NEWLINE ;
UNITS_STATEMENT                : '\\units ' .*? NEWLINE ;

GENERAL_PROPERTY : '\\' .*? NEWLINE ;

ALPHA_OPTION : 'A'[1-9][0-9]* ;

NUMERIC_OPTION : 'N'[1-9][0-9]* ;

OBJECT_NAME : [A-Z][a-zA-Z0-9:-]+ ;

COMMENT : '!' .*? NEWLINE ;

FIELD_SEPARATOR : ',' ;

OBJECT_TERMINATOR : ';' ;

/*Grab as many newlines, including if there is trailing whitespace on previous lines*/
NEWLINE : ('\r'? '\n')([ ]*('\r'? '\n'))* ;

WS : [ \t]+ -> skip ;

grammar Idd;

options {
    language=CSharp;
}

idd : ( COMMENT | group | NEWLINE )* ;

object : object_header fields;

object_header : OBJECT_NAME FIELD_SEPARATOR (COMMENT | NEWLINE)* object_properties ;

fields  : field* terminating_field ;

terminating_field : field_id OBJECT_TERMINATOR NEWLINE* field_properties ;

field : field_id FIELD_SEPARATOR NEWLINE* field_properties ;

field_id : ALPHA_OPTION | NUMERIC_OPTION ;

group : GROUP_STATEMENT (object | COMMENT)* ;

object_properties : (object_property | COMMENT)* ;

field_properties : (field_property | COMMENT)* ;

object_property :
      EXTENSIBLE_STATEMENT
    | FORMAT_STATEMENT
    | MEMO_STATEMENT
    | min_fields_statement
    | OBSOLETE_STATEMENT
    | REQUIRED_OBJECT_STATEMENT
    | UNIQUE_OBJECT_STATEMENT
    ;

field_property :
      AUTOCALCULATABLE_STATEMENT
    | AUTOSIZABLE_STATEMENT
    | BEGIN_EXTENSIBLE_STATEMENT
    | DEFAULT_STATEMENT
    | DEPRECATED_STATEMENT
    | EXTERNAL_LIST_STATEMENT
    | FIELD_STATEMENT
    | IP_UNITS_STATEMENT
    | KEY_STATEMENT
    | maximum_inclusive_statement
    | maximum_exclusive_statement
    | minimum_inclusive_statement
    | minimum_exclusive_statement
    | NOTE_STATEMENT
    | OBJECT_LIST_STATEMENT
    | REFERENCE_STATEMENT
    | REFERENCE_CLASS_NAME_STATEMENT
    | REQUIRED_FIELD_STATEMENT
    | RETAINCASE_STATEMENT
    | type_statement
    | UNITS_STATEMENT
    | units_based_on_field_statement
    ;


FIELD_TYPE :
      'integer'
    | 'real'
    | 'alpha'
    | 'choice'
    | 'object-list'
    | 'external-list'
    | 'node'
    ;


AUTOCALCULATABLE_STATEMENT     : '\\autocalculatable' NEWLINE ;
AUTOSIZABLE_STATEMENT          : '\\autosizable' NEWLINE ;
BEGIN_EXTENSIBLE_STATEMENT     : '\\begin-extensible' NEWLINE ;
DEFAULT_STATEMENT              : '\\default ' .*? NEWLINE ;
DEPRECATED_STATEMENT           : '\\deprecated' NEWLINE ;
EXTENSIBLE_STATEMENT           : '\\extensible' .*? NEWLINE ;
EXTERNAL_LIST_STATEMENT        : '\\external-list ' .*? NEWLINE ;
FIELD_STATEMENT                : '\\field ' .*? NEWLINE ;
FORMAT_STATEMENT               : '\\format ' .*? NEWLINE ;
GROUP_STATEMENT                : '\\group ' .*? NEWLINE ;
IP_UNITS_STATEMENT             : '\\ip-units ' .*? NEWLINE ;
KEY_STATEMENT                  : '\\key ' .*? NEWLINE ;
maximum_exclusive_statement    : '\\maximum< ' maxval=(REALNUMBER | INTEGER) NEWLINE ;
maximum_inclusive_statement    : '\\maximum ' maxval=(REALNUMBER | INTEGER)  NEWLINE ;
MEMO_STATEMENT                 : '\\memo ' .*? NEWLINE ;
min_fields_statement           : '\\min-fields ' INTEGER NEWLINE ;
minimum_exclusive_statement    : '\\minimum>' minval=(REALNUMBER | INTEGER) NEWLINE ;
minimum_inclusive_statement    : '\\minimum ' minval=(REALNUMBER | INTEGER) NEWLINE ;
NOTE_STATEMENT                 : '\\note ' .*? NEWLINE ;
OBJECT_LIST_STATEMENT          : '\\object-list ' .*? NEWLINE ;
OBSOLETE_STATEMENT             : '\\obsolete ' .*? NEWLINE ;
REFERENCE_STATEMENT            : '\\reference ' .*? NEWLINE ;
REFERENCE_CLASS_NAME_STATEMENT : '\\reference-class-name ' .*? NEWLINE ;
REQUIRED_FIELD_STATEMENT       : '\\required-field' NEWLINE ;
REQUIRED_OBJECT_STATEMENT      : '\\required-object' NEWLINE ;
RETAINCASE_STATEMENT           : '\\retaincase' NEWLINE ;
type_statement                 : '\\type ' FIELD_TYPE NEWLINE ;
UNIQUE_OBJECT_STATEMENT        : '\\unique-object' NEWLINE ;
units_based_on_field_statement : '\\unitsBasedOnField ' (ALPHA_OPTION | NUMERIC_OPTION) NEWLINE ;
UNITS_STATEMENT                : '\\units ' .*? NEWLINE ;

INTEGER : [1-9][0-9]* ;

REALNUMBER : ('-'|'+')?  // On some numbers, they put a '+'
                    ((
                        ([1-9][0-9]*|'0')('.'[0-9]+)? |
                        ('.'[0-9]+)      // This captures numbers entered with no leading zero '.0'
                    )
                ([eE]'-'?[0-9]+)?  |
                    ([1-9][0-9]*|'0')'.' // Needed to add this nonsense for numbers added like: '0.'
                )
                ;

ALPHA_OPTION : 'A'[1-9][0-9]* ;

NUMERIC_OPTION : 'N'[1-9][0-9]* ;

OBJECT_NAME : [A-Z][a-zA-Z0-9:-]+ ;

COMMENT : '!' .*? NEWLINE ;

FIELD_SEPARATOR : ',' ;

OBJECT_TERMINATOR : ';' ;

/*Grab as many newlines, including if there is trailing whitespace on previous lines*/
NEWLINE : ('\r'? '\n')([ ]*('\r'? '\n'))* ;

WS : [ \t]+ -> skip ;

%{

#include <stdio.h> /* printf() */
#include <string.h> /* strcpy() */
#include "common.h" /* MAX_STR_LEN */
int yylex(void);
void yyerror(const char *txt);

void found( const char *nonterminal, const char *value );
%}


%union 
{
	char s[ MAX_STR_LEN + 1 ];
	int i;
	double d;
}

%token KW_PROGRAM KW_BEGIN KW_END KW_USES KW_VAR KW_CONST KW_IF KW_THEN KW_ELSE
%token KW_CHAR KW_STRING KW_INTEGER KW_REAL KW_FOR KW_TO KW_DO KW_FUNCTION
%token KW_PROCEDURE KW_ARRAY KW_RECORD KW_OF KW_DOWNTO
%token ASSIGN LE RANGE
%token<s> IDENT STRING_CONST
%token<d> FLOAT_CONST
%token<i> INTEGER_CONST CHARACTER_CONST

%type<s> FUN_HEAD

 /* precedence of operators */
%left '+' '-'
%left '*' '/'
%right NEG

%%

 /* a program can be empty (semantic error), it may contain syntactic error
    or it may contain:
    program name (PROGRAM_NAME), declaration section
    (SECTION_LIST), and the main block (BLOCK) ending with a dot */
Grammar: %empty { yyerror( "Empty input source is not valid!" ); YYERROR; }
	| error
	  /* Start here! */
   | PROGRAM_NAME SECTION_LIST /*BLOCK*/ '.'
;

PROGRAM_NAME: %empty
   | KW_PROGRAM IDENT ';' { found("PROGRAM_NAME", $2); }
 /* May be empty, or it may contain: 
    keyword PROGRAM, program name (IDENT), and a semicolon */

SECTION_LIST: %empty
   | SECTION
   | SECTION_LIST SECTION
 /* Possibly empty sequence of sections (SECTION) */

SECTION: CONST_SECT
   | VAR_SECT
   | FUNCTION ';'
   | PROCEDURE ';'
 /* One of the following:
    section of declaractions of constants (CONST_SECT),
    section of declaractions of variables (VAR_SECT),
    functions (FUNCTION) followed by a semicolon,
    procedure (PROCEDURE) followed by a semicolon */


CONST_SECT: KW_CONST CONST_LIST ';' { found("CONST_SECT", ""); }
 // keyword CONST, constant declaration list (CONST_LIST) followed by a semicolon

CONST_LIST: CONST
   | CONST_LIST ';' CONST
 // list of constant declarations (CONST) separated with semicolons

CONST: IDENT '=' LITERAL { found("CONST", $1); }
 // identifier, equal sign ('=') , and a literal value (LITERAL)

LITERAL: INTEGER_CONST
   | FLOAT_CONST
   | STRING_CONST
 // Either an integer constant (INTEGER_CONST), a real constant (FLOAT_CONST), or a string constant (STRING_CONST)

VAR_SECT: KW_VAR VAR_LIST ';' { found("VAR_SECT", ""); }
 // keyword VAR, followed by a list of declarations of variables (VAR_LIST), ending with a semicolon, 
 // eg. "Var i : Integer; c : Char;

VAR_LIST: VAR
   | VAR_LIST ';' VAR
 /* list of declarations of variables (VAR) separated with semicolons */

VAR: IDENT_LIST ':' DATA_TYPE { found("VAR", ""); }
 /* identifier list (IDENT_LIST), colon, and data type (DATA_TYPE) */

IDENT_LIST: IDENT
   | IDENT ';' IDENT_LIST
 /* list of identifiers (IDENT) separated with commas */

DATA_TYPE: DATA_TYPE_NAME
   | ARRAY_TYPE
   | RECORD_TYPE
 /* Either: data type name (DATA_TYPE_NAME),
    array type (ARRAY_TYPE),
    or record type (RECORD_TYPE) */

DATA_TYPE_NAME: KW_INTEGER
   | KW_REAL
   | KW_CHAR
   | KW_STRING
 /* One of the lfollowing keywords: Integer, Real, Char, String */

ARRAY_TYPE: KW_ARRAY '[' DIMENSIONS ']' KW_OF DATA_TYPE
 /* keyword ARRAY, left square bracket, dimensions (DIMENSIONS),
   right square bracket, keyword OF, and data type (DATA_TYPE) */

DIMENSIONS: DIMENSION
   | DIMENSION ';' DIMENSIONS 
 /* list of dimensions (DIMENSION) separated with commas */

DIMENSION: LITERAL RANGE LITERAL
 /* literal value (LITERAL), RANGE operator, and literal value */

RECORD_TYPE: KW_RECORD FIELD_LIST KW_END
 /* keyword RECORD, field list (FIELD_LIST), and keyword END */

FIELD_LIST: FIELD
   | FIELD_LIST ';' FIELD
 /* list of fields (FIELD) separated with semicolons */

FIELD: IDENT_LIST ':' DATA_TYPE
 /* identifier list (IDENT_LIST), colon, and data type (DATA_TYPE) */

 /* PROCEDURE AND FUNCTION DECLARATIONS */

PROCEDURE: KW_PROCEDURE FUN_HEAD ':' SECTION_LIST BLOCK { found("PROCEDURE", $2); }
 /* keyword PROCEDURE, function header (FUN_HEAD), semicolon, section list
    (SECTION_LIST), and block (BLOCK) */

FUNCTION: KW_FUNCTION FUN_HEAD ':' DATA_TYPE ';' SECTION_LIST BLOCK { found("FUNCTION", $2); }
/* keyword FUNCTION, function head (FUN_HEAD), colon, return data type
    (DATA_TYPE), semicolon, section list (SECTION_LIST), and block (BLOCK) */

FUN_HEAD: IDENT FORM_PARAMS { found("FUN_HEAD", $1); }
/* identifier (IDENT) followed by formal parameters (FORM_PARAMS) */

FORM_PARAMS: %empty
   | '(' FORM_PARAM_LIST ')'
 /* Either empty, or a list of formal parameters (FORM_PARAM_LIST)
    in parentheses */

FORM_PARAM_LIST: FORM_PARAM
   | FORM_PARAM_LIST ',' FORM_PARAM
 /* list of formal parameters (FORM_PARAM) separated with commas */

FORM_PARAM: IDENT_LIST ':' DATA_TYPE { found("FORM_PARAM", ""); }
 /* identifier list (IDENT_LIST), colon, and data type (DATA_TYPE) */

BLOCK: KW_BEGIN KW_END { found("BLOCK", ""); }
   | KW_BEGIN INSTR_LIST KW_END { found("BLOCK", ""); }
   | KW_BEGIN INSTR_LIST ';' KW_END { found("BLOCK", ""); }
 /* Either keyword BEGIN, and keyword END,
    or keyword BEGIN, instruction list (INSTR_LIST), and keyword END,
    or keyword BEGIN, instruction list, semicolon, and keyword END */


INSTR_LIST: INSTRUCTION
   | INSTRUCTION ';' INSTR_LIST
/* Nonempty list of instructions (INSTRUCTION) separated with semicolons */

INSTRUCTION: FUNCT_CALL
   | FOR_INSTR
   | ASSIGN_INSTR
   | IF_INSTR
 /* One of the following:
    function call (FUNCT_CALL),
    for loop (FOR_INSTR),
    assignment (ASSIGN_INSTR),
    conditional instruction (IF_INSTR) */

 /* SIMPLE AND COMPLEX INSTRUCTIONS */

FUNCT_CALL: IDENT '(' ACT_PARAMS ')' { found("FUNCT_CALL", $1); }
 /* identifier followed by actual parameters (ACT_PARAMS) */

ACT_PARAMS: %empty
   | '(' ACT_PARAM_LIST ')'
 /* Either empty, or actual parameter list (ACT_PARAM_LIST) in parentheses */

ACT_PARAM_LIST: ACT_PARAM
   | ACT_PARAM ',' ACT_PARAM_LIST
 /* Nonempty list of actual parameters (ACT_PARAM) separated with commas */

ACT_PARAM: STRING_CONST { found("ACT_PARAM", ""); }
   | NUMBER { found("ACT_PARAM", ""); }
   | FUNCT_CALL { found("ACT_PARAM", ""); }
 /* Either: string constant (STRING_CONST),
    number (NUMBER), 
    or function call (FUNCT_CALL) */

NUMBER: INTEGER_CONST
   | FLOAT_CONST
 /* Either an integer constant (INTEGER_CONST)
    or a real constant (FLOAT_CONST) */

ASSIGN_INSTR: IDENT QUALIF ASSIGN EXPR { found("ASSIGN_INSTR", $1); }
 /* identifier, qualifier (QUALIF), assignment operator (ASSIGN),
    and expression (EXPR) */

QUALIF: %empty
   | '[' EXPR_LIST ']' QUALIF
   | '.' IDENT QUALIF
 /* Either empty,
   or a left square bracket, expression list (EXPR_LIST), right square bracket, and qualifier,
   or a dot, identifier, and qualifier
 */

EXPR_LIST: EXPR
   | EXPR ',' EXPR_LIST
 /* Nonempty list of expressions (EXPR) separated with commas */

EXPR: NUMBER
   | IDENT QUALIF
   | EXPR '+' EXPR
   | EXPR '-' EXPR
   | EXPR '*' EXPR
   | EXPR '/' EXPR
   | '-' EXPR %prec NEG
   | '(' EXPR ')'
 /* One of the following:
    number,
    identifier with a qualifier,
    two expressions separated with a:
    - plus,
    - minus,
    - star,
    - slash,
    minus sign followed by expression (use NEG precedence),
    or expression in parentheses */

FOR_INSTR: KW_FOR IDENT ASSIGN CONST_VAR TO_DOWNTO CONST_VAR KW_DO BLOCK_INSTR { found("FOR_INSTR", ""); }
 /* keyword FOR, identifier, assignment operator (ASSIGN),
    constant or variable (CONST_VAR), keyword TO or DOWNTO (TO_DOWNTO),
    constant or variable (CONST_VAR), keyword DO,
    and instruction block (BLOCK_INSTR) */

TO_DOWNTO: KW_TO
   | KW_DOWNTO
 /* Either keyword TO or keyword DOWNTO */

CONST_VAR: INTEGER_CONST
   | IDENT
 /*Either an integer constant, or identifier */

BLOCK_INSTR: BLOCK
   | INSTRUCTION
 /* Either a block (BLOCK), or a single instruction (INSTRUCTION) */ 

IF_INSTR: KW_IF LOG_EXPR KW_THEN BLOCK_INSTR ELSE_PART { found("IF_INSTR", ""); }
 /* keyword IF, logical expression LOG_EXPR, keyword THEN,
    instruction block BLOCK_INSTR, and else part (ELSE_PART) */

LOG_EXPR: EXPR '<' EXPR
   | EXPR '>' EXPR
   | EXPR LE EXPR
   | EXPR '=' EXPR
   | '(' EXPR ')' 
 /* Either two expressions EXPR linked with <, >, LE, =,
   or a logical expression in parentheses */

ELSE_PART: %empty
   | KW_ELSE BLOCK_INSTR
 /* Either empty,
    or keyword ELSE, and instruction block (BLOCK_INSTR) */


%%


int main( void )
{
	int ret;
	printf( "Author: Hubert Lewandowski\n" );
	printf( "yytext              Token type      Token value as string\n\n" );
	ret = yyparse();
	return ret;
}

void yyerror( const char *txt )
{
	printf( "Syntax error %s\n", txt );
}

void found( const char *nonterminal, const char *value )
{ /* info on found syntax structures (nonterminal) */
	printf( "===== FOUND: %s %s%s%s=====\n", nonterminal, 
			(*value) ? "'" : "", value, (*value) ? "' " : "" );
}

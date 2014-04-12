%{
#include <stdio.h>
#include <stdlib.h>
#include "ast.h"
%}
%union {
    int intval;
    char *strval;
    struct CNode *cnode;
}
%error-verbose
%token IDENTIFIER "identifier" INT_CONST "integer constant" CHAR_CONST "character constant" STR_CONST "string constant" USER_TYPE "typedef name"
%token KW_VOID "void" KW_CHAR "char" KW_INT "int" KW_STRUCT "struct" KW_UNION "union" KW_IF "if" KW_ELSE "else" KW_WHILE "while" KW_TYPEDEF "typedef"
%token KW_FOR "for" KW_CONT "continue" KW_BREAK "break" KW_RET "ret" KW_SIZEOF "sizeof"
%token OPT_OR "||" OPT_AND "&&" OPT_EQ "==" OPT_NE "!=" OPT_LE "<=" OPT_GE ">=" OPT_SHL "<<" OPT_SHR ">>" OPT_INC "++" OPT_DEC "--" OPT_PTR "->"
%token ASS_MUL "*=" ASS_DIV "/=" ASS_MOD "%=" ASS_ADD "+=" ASS_SUB "-=" ASS_SHL "<<=" ASS_SHR ">>=" ASS_AND "&=" ASS_XOR "^=" ASS_OR "|="
%token UNKNOWN "stray character"
%token END 0 "end of file"
%type<intval> INT_CONST
%type<strval> IDENTIFIER STR_CONST CHAR_CONST USER_TYPE
%type<intval> additive_operator assignment_operator equality_operator multiplicative_operator relational_operator shift_operator struct_or_union unary_operator
%type<cnode> additive_expression and_expression arguments array_initializer assignment_expression cast_expression comp_decls compound_statement comp_stmts constant_expression declaration declarator declarators equality_expression exclusive_or_expression expression expression_statement function_definition identifier inclusive_or_expression init_declarator init_declarators initializer iteration_statement jump_statement logical_and_expression logical_or_expression multiplicative_expression optional_exp parameters plain_declaration direct_declarator postfix postfix_expression primary_expression prog_list program relational_expression selection_statement shift_expression statement struct_field struct_fields type_name type_specifier unary_expression abstract_declarator direct_abstract_declarator direct_abstract_declarator_opt abstract_declarator_opt user_type
%start program
%%
program
    : prog_list { ast_root = cnode_create_ast(cnode_list_wrap(PROG, $1)); }

prog_list
    : declaration
    | function_definition
    | prog_list declaration         { $$ = cnode_list_append($1, $2); }
    | prog_list function_definition { $$ = cnode_list_append($1, $2); }

declaration
    : KW_TYPEDEF type_specifier { enter_typedef(); } declarators ';' {
        $$ = cnode_add_loc(cnode_create_typedef(
                            $2,
                            cnode_add_loc(cnode_list_wrap(DECLRS, $4), @4)), @$);
        clear_state();
    }
    | type_specifier ';' {
        $$ = cnode_add_loc(cnode_create_decl(
                            $1,
                            cnode_list_wrap(INIT_DECLRS, cnode_create_nop())), @$);
        clear_state();
    }
    | type_specifier init_declarators  ';' {
        $$ = cnode_add_loc(cnode_create_decl(
                            $1,
                            cnode_add_loc(cnode_list_wrap(INIT_DECLRS, $2), @2)), @$);
        clear_state();
    }

function_definition
    : type_specifier declarator compound_statement {
        $$ = cnode_add_loc(cnode_create_func($1, $2, $3), @$);
        clear_state();
    }

parameters
    : { $$ = NULL; }
    | plain_declaration
    | parameters ',' plain_declaration { $$ = cnode_list_append($1, $3); }

declarators
    : declarator
    | declarators ',' declarator { $$ = cnode_list_append($1, $3); }

init_declarators
    : init_declarator
    | init_declarators ',' init_declarator { $$ = cnode_list_append($1, $3); }

init_declarator
    : declarator { $$ = cnode_add_loc(cnode_create_init_declr($1, cnode_create_nop()), @$); }
    | declarator '=' initializer { $$ = cnode_add_loc(cnode_create_init_declr($1, $3), @$); }

initializer
    : assignment_expression { $$ = cnode_add_loc(cnode_create_initr(INITR_NORM, $1), @$); }
    | '{' array_initializer '}' { $$ = cnode_add_loc(cnode_create_initr(INITR_ARR, $2), @$); }

array_initializer
    : initializer
    | array_initializer ',' initializer {
        $$ = cnode_list_append(cnode_add_loc($1, @1), $3); }

type_specifier
    : KW_VOID { $$ = cnode_add_loc(cnode_create_type_spec(KW_VOID, 0), @$); force_id(); }
    | KW_CHAR { $$ = cnode_add_loc(cnode_create_type_spec(KW_CHAR, 0), @$); force_id(); }
    | KW_INT { $$ = cnode_add_loc(cnode_create_type_spec(KW_INT, 0), @$); force_id(); }
    | struct_or_union identifier '{' struct_fields '}' {
        $$ = cnode_add_loc(cnode_create_type_spec($1, 2, $2, cnode_add_loc(cnode_list_wrap(FIELDS, $4), @4)), @$);
        force_id();
    }
    | struct_or_union '{' struct_fields '}'            {
        $$ = cnode_add_loc(cnode_create_type_spec($1, 2, cnode_create_nop(), cnode_add_loc(cnode_list_wrap(FIELDS, $3), @3)), @$);
        force_id();
    }
    | struct_or_union identifier {
        $$ = cnode_add_loc(cnode_create_type_spec($1, 2, $2, cnode_create_nop()), @$);
        force_id();
    }
    | user_type { $$ = cnode_add_loc(cnode_create_type_spec(USER_TYPE, 1, $1), @$); force_id(); }

user_type
    : USER_TYPE { $$ = cnode_add_loc(cnode_create_identifier($1), @$); }

struct_fields
    : struct_field
    | struct_fields struct_field { $$ = cnode_list_append($1, $2); }
struct_field
    : type_specifier declarators ';' {
        $$ = cnode_add_loc(
                cnode_create_struct_field(
                    $1,
                    cnode_add_loc(cnode_list_wrap(DECLRS, $2), @2)), @$);
        clear_state();
    }

struct_or_union
    : KW_STRUCT { $$ = KW_STRUCT; force_id(); }
    | KW_UNION { $$ = KW_UNION; force_id(); }

plain_declaration
    : type_specifier declarator {
        $$ = cnode_add_loc(cnode_create_plain_decl($1, $2), @$);
        clear_state();
    }

direct_declarator
    : identifier { push($1->rec.strval); }
    | '(' declarator ')' { $$ = $2; }
    | direct_declarator '(' parameters ')' {
        $$ = cnode_add_loc(cnode_create_declr(
                            DECLR_FUNC, 2, $1,
                            cnode_add_loc(cnode_list_wrap(PARAMS, $3), @3)), @$);
    }
    | direct_declarator '[' constant_expression ']' {
        $$ = cnode_add_loc(cnode_create_declr(DECLR_ARR, 2, $1, $3), @$);
    }

declarator
    : direct_declarator
    | '*' declarator {
        $$ = cnode_add_loc(cnode_create_declr('*', 1, $2), @$); }

statement
    : expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement

expression_statement
    : ';'               { $$ = cnode_add_loc(cnode_create_stmt(STMT_EXP, 1, cnode_create_nop()), @$); }
    | expression ';'    { $$ = cnode_add_loc(cnode_create_stmt(STMT_EXP, 1, $1), @$); }

compound_statement
    : { clear_state(); enter_block(); } '{' comp_decls comp_stmts '}' {
        $$ = cnode_add_loc(
                cnode_create_stmt(STMT_COMP, 2, cnode_add_loc(cnode_list_wrap(COMP_DECLS, $3), @3),
                                                cnode_add_loc(cnode_list_wrap(COMP_STMTS, $4), @4)), @$);
        exit_block();
    }

comp_decls
    : { $$ = cnode_create_nop(); }
    | comp_decls declaration { $$ = cnode_list_append($1, $2); }

comp_stmts
    : { $$ = cnode_create_nop(); }
    | comp_stmts statement { $$ = cnode_list_append($1, $2); }

selection_statement
    : KW_IF '(' expression ')' statement {
        $$ = cnode_add_loc(
                cnode_create_stmt(STMT_IF, 3, $3, $5, cnode_create_nop()), @$);
    }
    | KW_IF '(' expression ')' statement KW_ELSE statement {
        $$ = cnode_add_loc(
                cnode_create_stmt(STMT_IF, 3, $3, $5, $7), @$);
    }

iteration_statement
    : KW_WHILE '(' expression ')' statement {
        $$ = cnode_add_loc(cnode_create_stmt(STMT_WHILE, 2, $3, $5), @$);
    }
    | KW_FOR '(' optional_exp ';' optional_exp ';' optional_exp ')' statement {
        $$ = cnode_add_loc(cnode_create_stmt(STMT_FOR, 4, $3, $5, $7, $9), @$);
    }

optional_exp
    : { $$ = cnode_create_nop(); }
    | expression

jump_statement
    : KW_CONT ';'   { $$ = cnode_add_loc(cnode_create_stmt(STMT_CONT, 0), @$); }
    | KW_BREAK ';'  { $$ = cnode_add_loc(cnode_create_stmt(STMT_BREAK, 0), @$); }
    | KW_RET optional_exp ';' { $$ = cnode_add_loc(cnode_create_stmt(STMT_RET, 1, $2), @$); }

expression
    : assignment_expression
    | expression ',' assignment_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp(',', 2, $1, $3), @2); }

assignment_expression
    : logical_or_expression
    | unary_expression assignment_operator assignment_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp($2, 2, $1, $3), @2); }

assignment_operator
    : '='           { $$ = '='; }
    | ASS_MUL       { $$ = ASS_MUL; }
    | ASS_DIV       { $$ = ASS_DIV; }
    | ASS_MOD       { $$ = ASS_MOD; }
    | ASS_ADD       { $$ = ASS_ADD; }
    | ASS_SUB       { $$ = ASS_SUB; }
    | ASS_SHL       { $$ = ASS_SHL; }
    | ASS_SHR       { $$ = ASS_SHR; }
    | ASS_AND       { $$ = ASS_AND; }
    | ASS_XOR       { $$ = ASS_XOR; }
    | ASS_OR        { $$ = ASS_OR; }

constant_expression: logical_or_expression
logical_or_expression
    : logical_and_expression
    | logical_or_expression OPT_OR logical_and_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp(OPT_OR, 2, $1, $3), @2); }

logical_and_expression
    : inclusive_or_expression
    | logical_and_expression OPT_AND inclusive_or_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp(OPT_AND, 2, $1, $3), @2); }

inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp('|', 2, $1, $3), @2); }

exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp('^', 2, $1, $3), @2); }

and_expression
    : equality_expression
    | and_expression '&' equality_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp('&', 2, $1, $3), @2); }

equality_expression
    : relational_expression
    | equality_expression equality_operator relational_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp($2, 2, $1, $3), @2); }

equality_operator
    : OPT_EQ { $$ = OPT_EQ; }
    | OPT_NE { $$ = OPT_NE; }

relational_expression
    : shift_expression
    | relational_expression relational_operator shift_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp($2, 2, $1, $3), @2); }

relational_operator
    : '<' { $$ = '<'; }
    | '>' { $$ = '>'; }
    | OPT_LE { $$ = OPT_LE; }
    | OPT_GE { $$ = OPT_GE; }

shift_expression
    : additive_expression
    | shift_expression shift_operator additive_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp($2, 2, $1, $3), @2); }

shift_operator
    : OPT_SHL { $$ = OPT_SHL; }
    | OPT_SHR { $$ = OPT_SHR; }

additive_expression
    : multiplicative_expression
    | additive_expression additive_operator multiplicative_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp($2, 2, $1, $3), @2); }

additive_operator
    : '+' { $$ = '+'; }
    | '-' { $$ = '-'; }

multiplicative_expression
    : cast_expression
    | multiplicative_expression multiplicative_operator cast_expression {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp($2, 2, $1, $3), @2); }

multiplicative_operator
    : '*' { $$ = '*'; }
    | '/' { $$ = '/'; }
    | '%' { $$ = '%'; }

cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression {
        $$ = cnode_add_loc(cnode_create_exp(EXP_CAST, 2, $2, $4), @$); }

type_name
    : type_specifier abstract_declarator_opt {
        $$ = cnode_add_loc(cnode_create_declr(0, 2, $1, $2), @$);
        clear_state();
    }

abstract_declarator_opt
    : { $$ = cnode_create_nop(); }
    | abstract_declarator

abstract_declarator
    : '*' { $$ = cnode_add_loc(cnode_create_declr('*', 1, cnode_create_nop()), @$); }
    | '*' abstract_declarator { $$ = cnode_add_loc(cnode_create_declr('*', 1, $2), @$); }
    | direct_abstract_declarator

direct_abstract_declarator_opt
    : { $$ = cnode_create_nop(); }
    | direct_abstract_declarator

direct_abstract_declarator
    : '(' abstract_declarator ')' { $$ = $2; }
    | direct_abstract_declarator '(' parameters ')' {
        $$ = cnode_add_loc(cnode_create_declr(
                            DECLR_FUNC, 2, $1,
                            cnode_add_loc(cnode_list_wrap(PARAMS, $3), @3)), @$);
    }
    | '(' parameters ')' {
        $$ = cnode_add_loc(cnode_create_declr(
                            DECLR_FUNC, 2, cnode_create_nop(),
                            cnode_add_loc(cnode_list_wrap(PARAMS, $2), @2)), @$);
    }
    | direct_abstract_declarator_opt '[' constant_expression ']' {
        $$ = cnode_add_loc(cnode_create_declr(DECLR_ARR, 2, $1, $3), @$);
    }

unary_expression
    : postfix_expression
    | OPT_INC unary_expression { $$ = cnode_add_loc(cnode_create_exp(OPT_INC, 1, $2), @$); }
    | OPT_DEC unary_expression { $$ = cnode_add_loc(cnode_create_exp(OPT_DEC, 1, $2), @$); }
    | unary_operator cast_expression { $$ = cnode_add_loc(cnode_create_exp($1, 1, $2), @$); }
    | KW_SIZEOF unary_expression { $$ = cnode_add_loc(cnode_create_exp(KW_SIZEOF, 1, $2), @$); }
    | KW_SIZEOF '(' type_name ')' { $$ = cnode_add_loc(cnode_create_exp(KW_SIZEOF, 1, $3), @$); }

unary_operator
    : '&' { $$ = '&'; }
    | '*' { $$ = '*'; }
    | '+' { $$ = '+'; }
    | '-' { $$ = '-'; }
    | '~' { $$ = '~'; }
    | '!' { $$ = '!'; }

postfix_expression
    : primary_expression
    | postfix_expression postfix {
        @$ = @2;
        $$ = cnode_add_loc(cnode_create_exp(EXP_POSTFIX, 2, $1, $2), @2); }

postfix
    : '[' expression ']' { $$ = cnode_add_loc(cnode_create_exp(POSTFIX_ARR, 1, $2), @$); }
    | '(' arguments ')' {
        $$ = cnode_add_loc(cnode_create_exp(
                            POSTFIX_CALL, 1,
                            cnode_add_loc(cnode_list_wrap(ARGS, $2), @2)), @$); }
    | '.' identifier { $$ = cnode_add_loc(cnode_create_exp(POSTFIX_DOT, 1, $2), @$); }
    | OPT_PTR identifier { $$ = cnode_add_loc(cnode_create_exp(POSTFIX_PTR, 1, $2), @$); }
    | OPT_INC { $$ = cnode_add_loc(cnode_create_exp(OPT_INC, 0), @$); }
    | OPT_DEC { $$ = cnode_add_loc(cnode_create_exp(OPT_DEC, 0), @$); }

arguments
    : { $$ = NULL; }
    | assignment_expression
    | arguments ',' assignment_expression { $$ = cnode_list_append($1, $3); }

primary_expression
    : identifier
    | INT_CONST   { $$ = cnode_add_loc(cnode_create_int_const($1), @$); }
    | CHAR_CONST { $$ = cnode_add_loc(cnode_create_char_const($1), @$); }
    | STR_CONST  { $$ = cnode_add_loc(cnode_create_str_const($1), @$); }
    | '(' expression ')' { $$ = cnode_add_loc($2, @$); }

identifier
    : IDENTIFIER { $$ = cnode_add_loc(cnode_create_identifier($1), @$); }
%%

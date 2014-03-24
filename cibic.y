%{
#include <stdio.h>
#include "ast.h"
%}
%union {
    int intval;
    char *strval;
    struct CNode *cnode;
}

%token IDENTIFIER INT_CONST CHAR_CONST STR_CONST
%token KW_VOID KW_CHAR KW_INT KW_STRUCT KW_UNION KW_IF KW_ELSE KW_WHILE
%token KW_FOR KW_CONT KW_BREAK KW_RET KW_SIZEOF
%token OPT_OR OPT_AND OPT_EQ OPT_NE OPT_LE OPT_GE OPT_SHL OPT_SHR OPT_INC OPT_DEC OPT_PTR
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB ASS_SHL ASS_SHR ASS_AND ASS_XOR ASS_OR
%token UNKNOWN
%type<intval> INT_CONST CHAR_CONST
%type<strval> IDENTIFIER STR_CONST
%type<intval> assignment_operator equality_operator relational_operator shift_operator additive_operator multiplicative_operator unary_operator struct_or_union
%type<cnode> expression assignment_expression constant_expression logical_or_expression logical_and_expression inclusive_or_expression exclusive_or_expression and_expression equality_expression relational_expression shift_expression additive_expression multiplicative_expression cast_expression type_name unary_expression postfix_expression identifier primary_expression arguments postfix type_specifier program declaration function_definition parameters declarators init_declarators init_declarator initializer array_initializer struct_fields struct_field plain_declaration  declarator_array plain_declarator expression_statement compound_statement statement comp_decls comp_stmts selection_statement iteration_statement jump_statement optional_exp declarator prog_list
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
    : type_specifier ';' {
        $$ = cnode_create_decl($1, cnode_list_wrap(INIT_DECLRS, cnode_create_nop()));
    }
    | type_specifier init_declarators ';' { 
        $$ = cnode_create_decl($1, cnode_list_wrap(INIT_DECLRS, $2)); 
    }

function_definition
    : type_specifier plain_declarator '(' parameters ')' compound_statement {
        $$ = cnode_create_func($1, $2, cnode_list_wrap(PARAMS, $4), $6);
    }
    | type_specifier plain_declarator '(' ')' compound_statement {
        $$ = cnode_create_func($1, $2, cnode_list_wrap(PARAMS, cnode_create_nop()), $5);
    }

parameters
    : plain_declaration
    | parameters ',' plain_declaration { $$ = cnode_list_append($1, $3); }

declarators
    : declarator
    | declarators ',' declarator { $$ = cnode_list_append($1, $3); }

init_declarators
    : init_declarator
    | init_declarators ',' init_declarator { $$ = cnode_list_append($1, $3); }

init_declarator
    : declarator { $$ = cnode_create_init_declr($1, cnode_create_nop()); }
    | declarator '=' initializer { $$ = cnode_create_init_declr($1, $3); }

initializer
    : assignment_expression { $$ = cnode_create_initr(INITR_NORM, $1); }
    | '{' array_initializer '}' { $$ = cnode_create_initr(INITR_ARR, $2); }

array_initializer
    : initializer
    | array_initializer ',' initializer { $$ = cnode_list_append($1, $3); }

type_specifier
    : KW_VOID { $$ = cnode_create_type_spec(KW_VOID, 0); }
    | KW_CHAR { $$ = cnode_create_type_spec(KW_CHAR, 0); }
    | KW_INT { $$ = cnode_create_type_spec(KW_INT, 0); }
    | struct_or_union identifier '{' struct_fields '}' { $$ = cnode_create_type_spec($1, 2, $2, $4); }
    | struct_or_union '{' struct_fields '}'            { $$ = cnode_create_type_spec($1, 2, cnode_create_nop(), $3); }
    | struct_or_union identifier                { $$ = cnode_create_type_spec($1, 2, $2, cnode_create_nop()); }

struct_fields
    : struct_field
    | struct_fields struct_field { $$ = cnode_list_append($1, $2); }
struct_field
    : type_specifier declarators ';' { 
        $$ = cnode_create_struct_field($1, cnode_list_wrap(DECLRS, $2));
    }

struct_or_union
    : KW_STRUCT { $$ = KW_STRUCT; }
    | KW_UNION { $$ = KW_UNION; }

plain_declaration
    : type_specifier declarator { $$ = cnode_create_plain_decl($1, $2); }

declarator
    : plain_declarator '(' ')' { 
        $$ = cnode_create_declr(DECLR_FUNC, 2, $1, cnode_list_wrap(PARAMS, cnode_create_nop()));
    }
    | plain_declarator '(' parameters ')' {
        $$ = cnode_create_declr(DECLR_FUNC, 2, $1, cnode_list_wrap(PARAMS, $3));
    }
    | declarator_array

declarator_array
    : plain_declarator
    | declarator_array '[' constant_expression ']' { $$ = cnode_create_declr(DECLR_ARR, 2, $1, $3); }

plain_declarator
    : identifier
    | '*' plain_declarator { $$ = cnode_create_declr('*', 1, $2); }

statement
    : expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement

expression_statement
    : ';'               { $$ = cnode_create_stmt(STMT_EXP, 1, cnode_create_nop()); }
    | expression ';'    { $$ = cnode_create_stmt(STMT_EXP, 1, $1); }

compound_statement
    : '{' comp_decls comp_stmts '}' { 
        $$ = cnode_create_stmt(STMT_COMP, 2, cnode_list_wrap(COMP_DECLS, $2), 
                                             cnode_list_wrap(COMP_STMTS, $3)); 
    }

comp_decls
    : { $$ = cnode_create_nop(); }
    | comp_decls declaration { $$ = cnode_list_append($1, $2); }

comp_stmts
    : { $$ = cnode_create_nop(); }
    | comp_stmts statement { $$ = cnode_list_append($1, $2); }

selection_statement
    : KW_IF '(' expression ')' statement { 
        $$ = cnode_create_stmt(STMT_IF, 3, $3, $5, cnode_create_nop()); 
    }
    | KW_IF '(' expression ')' statement KW_ELSE statement { 
        $$ = cnode_create_stmt(STMT_IF, 3, $3, $5, $7);
    }

iteration_statement
    : KW_WHILE '(' expression ')' statement {
        $$ = cnode_create_stmt(STMT_WHILE, 2, $3, $5);
    }
    | KW_FOR '(' optional_exp ';' optional_exp ';' optional_exp ')' statement {
        $$ = cnode_create_stmt(STMT_FOR, 4, $3, $5, $7, $9);
    }

optional_exp
    : { $$ = cnode_create_nop(); }
    | expression

jump_statement
    : KW_CONT ';'   { $$ = cnode_create_stmt(STMT_CONT, 0); }
    | KW_BREAK ';'  { $$ = cnode_create_stmt(STMT_BREAK, 0); }
    | KW_RET optional_exp ';' { $$ = cnode_create_stmt(STMT_RET, 1, $2); }

expression
    : assignment_expression
    | expression ',' assignment_expression { $$ = cnode_create_exp(',', 2, $1, $3); }

assignment_expression
    : logical_or_expression
    | unary_expression assignment_operator assignment_expression { $$ = cnode_create_exp($2, 2, $1, $3); }

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
        $$ = cnode_create_exp(OPT_OR, 2, $1, $3);
    }

logical_and_expression
    : inclusive_or_expression
    | logical_and_expression OPT_AND inclusive_or_expression {
        $$ = cnode_create_exp(OPT_AND, 2, $1, $3);
    }

inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression {
        $$ = cnode_create_exp('|', 2, $1, $3);
    }

exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression {
        $$ = cnode_create_exp('^', 2, $1, $3);
    }

and_expression
    : equality_expression
    | and_expression '&' equality_expression {
        $$ = cnode_create_exp('&', 2, $1, $3);
    }

equality_expression
    : relational_expression
    | equality_expression equality_operator relational_expression {
        $$ = cnode_create_exp($2, 2, $1, $3);
    }

equality_operator
    : OPT_EQ { $$ = OPT_EQ; }
    | OPT_NE { $$ = OPT_NE; }

relational_expression
    : shift_expression
    | relational_expression relational_operator shift_expression {
        $$ = cnode_create_exp($2, 2, $1, $3);
    }

relational_operator
    : '<' { $$ = '<'; }
    | '>' { $$ = '>'; }
    | OPT_LE { $$ = OPT_LE; }
    | OPT_GE { $$ = OPT_GE; }

shift_expression
    : additive_expression
    | shift_expression shift_operator additive_expression {
        $$ = cnode_create_exp($2, 2, $1, $3);
    }

shift_operator
    : OPT_SHL { $$ = OPT_SHL; }
    | OPT_SHR { $$ = OPT_SHR; }

additive_expression
    : multiplicative_expression
    | additive_expression additive_operator multiplicative_expression {
        $$ = cnode_create_exp($2, 2, $1, $3);
    }

additive_operator
    : '+' { $$ = '+'; }
    | '-' { $$ = '-'; }

multiplicative_expression
    : cast_expression
    | multiplicative_expression multiplicative_operator cast_expression {
        $$ = cnode_create_exp($2, 2, $1, $3);
    }

multiplicative_operator
    : '*' { $$ = '*'; }
    | '/' { $$ = '/'; }
    | '%' { $$ = '%'; }

cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression {
        $$ = cnode_create_exp(EXP_CAST, 2, $2, $4);
    }

type_name
    : type_specifier
    | type_name '*' { $$ = cnode_create_exp('*', 1, $1); }

unary_expression
    : postfix_expression
    | OPT_INC unary_expression { $$ = cnode_create_exp(OPT_INC, 1, $2); }
    | OPT_DEC unary_expression { $$ = cnode_create_exp(OPT_DEC, 1, $2); }
    | unary_operator cast_expression { $$ = cnode_create_exp($1, 1, $2); }
    | KW_SIZEOF unary_expression { $$ = cnode_create_exp(KW_SIZEOF, 1, $2); }
    | KW_SIZEOF '(' type_name ')' { $$ = cnode_create_exp(KW_SIZEOF, 1, $3); }

unary_operator
    : '&' { $$ = '&'; }
    | '*' { $$ = '*'; }
    | '+' { $$ = '+'; }
    | '-' { $$ = '-'; }
    | '~' { $$ = '~'; }
    | '!' { $$ = '!'; }

postfix_expression
    : primary_expression
    | postfix_expression postfix { $$ = cnode_create_exp(EXP_POSTFIX, 2, $1, $2); }

postfix
    : '[' expression ']' { $$ = cnode_create_exp(POSTFIX_ARR, 1, $2); }
    | '(' arguments ')' { 
        $$ = cnode_create_exp(POSTFIX_CALL, 1, cnode_list_wrap(ARGS, $2)); 
    }
    | '(' ')' { 
        $$ = cnode_create_exp(POSTFIX_CALL, 1, cnode_list_wrap(ARGS, cnode_create_nop()));
    }
    | '.' identifier { $$ = cnode_create_exp(POSTFIX_DOT, 1, $2); }
    | OPT_PTR identifier { $$ = cnode_create_exp(POSTFIX_PTR, 1, $2); }
    | OPT_INC { $$ = cnode_create_exp(OPT_INC, 0); }
    | OPT_DEC { $$ = cnode_create_exp(OPT_DEC, 0); }

arguments
    : assignment_expression
    | arguments ',' assignment_expression { $$ = cnode_list_append($1, $3); }

primary_expression
    : identifier
    | INT_CONST   { $$ = cnode_create_int_const($1); }
    | CHAR_CONST { $$ = cnode_create_char_const($1); }
    | STR_CONST  { $$ = cnode_create_str_const($1); }
    | '(' expression ')' { $$ = $2; }

identifier
    : IDENTIFIER { $$ = cnode_create_identifier($1); }
%%

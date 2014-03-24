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
%type<intval> assignment_operator equality_operator relational_operator shift_operator additive_operator multiplicative_operator unary_operator
%type<cnode> expression assignment_expression constant_expression 
%type<cnode> logical_or_expression logical_and_expression inclusive_or_expression exclusive_or_expression and_expression equality_expression relational_expression shift_expression additive_expression multiplicative_expression cast_expression type_name
%type<cnode>unary_expression postfix_expression identifier primary_expression arguments postfix type_specifier
%%
debug_top
    : expression { ast_root = $1; }
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
    | logical_or_expression OPT_OR logical_and_expression { $$ = cnode_create_exp(OPT_OR, 2, $1, $3); }

logical_and_expression
    : inclusive_or_expression
    | logical_and_expression OPT_AND inclusive_or_expression { $$ = cnode_create_exp(OPT_AND, 2, $1, $3); }

inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression { $$ = cnode_create_exp('|', 2, $1, $3); }

exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression { $$ = cnode_create_exp('^', 2, $1, $3); }

and_expression
    : equality_expression
    | and_expression '&' equality_expression { $$ = cnode_create_exp('&', 2, $1, $3); }

equality_expression
    : relational_expression 
    | equality_expression equality_operator relational_expression { $$ = cnode_create_exp($2, 2, $1, $3); }

equality_operator
    : OPT_EQ { $$ = OPT_EQ; }
    | OPT_NE { $$ = OPT_NE; }

relational_expression
    : shift_expression
    | relational_expression relational_operator shift_expression { $$ = cnode_create_exp($2, 2, $1, $3); }

relational_operator
    : '<' { $$ = '<'; }
    | '>' { $$ = '>'; }
    | OPT_LE { $$ = OPT_LE; }
    | OPT_GE { $$ = OPT_GE; }

shift_expression
    : additive_expression
    | shift_expression shift_operator additive_expression { $$ = cnode_create_exp($2, 2, $1, $3); }

shift_operator
    : OPT_SHL { $$ = OPT_SHL; }
    | OPT_SHR { $$ = OPT_SHR; }

additive_expression
    : multiplicative_expression
    | additive_expression additive_operator multiplicative_expression { $$ = cnode_create_exp($2, 2, $1, $3); }

additive_operator
    : '+' { $$ = '+'; }
    | '-' { $$ = '-'; }

multiplicative_expression
    : cast_expression
    | multiplicative_expression multiplicative_operator cast_expression { $$ = cnode_create_exp($2, 2, $1, $3); }

multiplicative_operator
    : '*' { $$ = '*'; }
    | '/' { $$ = '/'; }
    | '%' { $$ = '%'; }

cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression { $$ = cnode_create_exp(EXP_CAST, 2, $2, $4); }

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
    | '(' arguments ')' { $$ = cnode_create_exp(POSTFIX_CALL, 1, $2); }
    | '(' ')' { $$ = cnode_create_exp(POSTFIX_CALL, 1, NULL); }
    | '.' identifier { $$ = cnode_create_exp(POSTFIX_DOT, 1, $2); }
    | OPT_PTR identifier { $$ = cnode_create_exp(POSTFIX_PTR, 1, $2); }
    | OPT_INC { $$ = cnode_create_exp(OPT_INC, 0); }
    | OPT_DEC { $$ = cnode_create_exp(OPT_DEC, 0); }

arguments
    : assignment_expression
    | arguments ',' assignment_expression { $$ = cnode_append($1, $3); }

primary_expression
    : identifier
    | INT_CONST   { $$ = cnode_create_int_const($1); }
    | CHAR_CONST { $$ = cnode_create_char_const($1); }
    | STR_CONST  { $$ = cnode_create_str_const($1); }
    | '(' expression ')' { $$ = $2; }

identifier
    : IDENTIFIER { $$ = cnode_create_identifier($1); }

type_specifier
    : KW_VOID { $$ = cnode_create_type_spec(KW_VOID, 0); }
    | KW_CHAR { $$ = cnode_create_type_spec(KW_CHAR, 0); }
    | KW_INT { $$ = cnode_create_type_spec(KW_INT, 0); } 
    /* to be continue */


%%
int yywrap() { 
    return 1; 
}

int yyerror(char *s) {
}

extern FILE *yyin;
void test_ast() {
    yyparse();
    if (ast_root)
        cnode_debug_print(ast_root);
    else
        fprintf(stderr, "Syntax Error\n");
}

int main() {
    int ret;
    yyin = fopen("in", "r");
    test_ast();
    return 0;
}

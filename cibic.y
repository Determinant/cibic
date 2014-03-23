%{
#include <stdio.h>
#include "ast.h"
%}
%token IDENTIFIER INT_CONST CHAR_CONST STR_CONST
%token KW_VOID KW_CHAR KW_INT KW_STRUCT KW_UNION KW_IF KW_ELSE KW_WHILE
%token KW_FOR KW_CONT KW_BREAK KW_RET KW_SIZEOF
%token OPT_OR OPT_AND OPT_EQ OPT_NE OPT_LE OPT_GE OPT_SHL OPT_SHR OPT_INC OPT_DEC OPT_PTR
%token ASS_MUL ASS_DIV ASS_MOD ASS_ADD ASS_SUB ASS_SHL ASS_SHR ASS_AND ASS_XOR ASS_OR 
%token UNKNOWN
%union {
    int intval;
    char *strval;
}
%%
program
    : body { 
        printf("\n")}
body
    : IDENTIFIER
%%
int yywrap() { 
    return 1; 
}

int yyerror(char *s) {
}

extern FILE *yyin;
int main() {
    int ret;
    //yyin = fopen("in", "r");
    while (ret = yylex()) 
    {
        printf("%d\n", ret);    
        if (ret == IDENTIFIER)
            printf("id: %s\n", yylval.strval);
        else if (ret == INT_CONST)
            printf("int: %d\n", yylval.intval);
        else if (ret == STR_CONST)
            printf("str: %s\n", yylval.strval);
    }
    return 0;
}

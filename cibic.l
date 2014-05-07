%{
#include "cibic.tab.h"
#include "const.h"
#include "semantics.h"
int yycolumn = 1;
char linebuff[MAX_LINEBUFF], *lptr = linebuff;

#define YY_USER_ACTION \
    do { \
        yylloc.first_line = yylloc.last_line = yylineno; \
        yylloc.first_column = yycolumn; \
        yylloc.last_column = yycolumn + yyleng - 1; \
        yycolumn += yyleng; \
        memmove(lptr, yytext, yyleng);  \
        lptr += yyleng; \
    } while (0);

#define NEW_LINE_USER_ACTION \
    do { \
        yycolumn = 1; \
        lptr = linebuff; \
    } while (0)
%}

letter [a-zA-Z_$]
digit [0-9]
string ((\\.|[^\n\"\\])*)
char ([^\n'\\]|\\.|\\[0-7]+|\\[xX][0-9a-fA-F]+)

%s IN_BLOCK_COMMENT IN_INLINE_COMMENT IN_DIRECTIVE
%option yylineno
%%


<INITIAL>{
"/*"    BEGIN(IN_BLOCK_COMMENT);
}
<IN_BLOCK_COMMENT>{
"*/"    BEGIN(INITIAL);
[^*\n]+   // eat comment in chunks
"*"       // eat the lone star
\n      { NEW_LINE_USER_ACTION; }
}

<INITIAL>{
"//"    BEGIN(IN_INLINE_COMMENT);
}
<IN_INLINE_COMMENT>{
\n      { NEW_LINE_USER_ACTION; BEGIN(INITIAL); }
[^\n]+
}

<INITIAL>{
"#"    BEGIN(IN_DIRECTIVE);
}
<IN_DIRECTIVE>{
\n      { NEW_LINE_USER_ACTION; BEGIN(INITIAL); }
[^\n]+
}

"void"                                      { return KW_VOID; }
"char"                                      { return KW_CHAR; }
"int"                                       { return KW_INT; }
"struct"                                    { return KW_STRUCT; }
"union"                                     { return KW_UNION; }
"if"                                        { return KW_IF; }
"else"                                      { return KW_ELSE; }
"while"                                     { return KW_WHILE; }
"for"                                       { return KW_FOR; }
"continue"                                  { return KW_CONT; }
"break"                                     { return KW_BREAK; }
"return"                                    { return KW_RET; }
"sizeof"                                    { return KW_SIZEOF; }
"typedef"                                   { return KW_TYPEDEF; }

{letter}({letter}|{digit})*                 {
    yylval.strval = strdup(yytext);
    return is_identifier(yytext) ? IDENTIFIER : USER_TYPE;
}

({digit}+)|(0[xX][0-9a-fA-F]+)              {
    if (*yytext == '0')
    {
        if (*(yytext + 1) == 'x' || *(yytext + 1) == 'X')
            sscanf(yytext, "%x", &yylval.intval);
        else    // FIXME: error report if it is not a valid octal
            sscanf(yytext, "%o", &yylval.intval);
    }
    else yylval.intval = atoi(yytext);
    return INT_CONST;
}

'{char}' {
    yylval.strval = strndup(yytext + 1, strlen(yytext) - 2);
    return CHAR_CONST;
}

'{char}? {
    yyerror("missing terminating ' character\n");
    exit(1);
}

\"{string}\" {
    yylval.strval = strndup(yytext + 1, strlen(yytext) - 2);
    return STR_CONST;
}

\"{string}? {
    yyerror("missing terminating \" character\n");
    exit(1);
}

"||"                                        { return OPT_OR; }
"&&"                                        { return OPT_AND; }
"=="                                        { return OPT_EQ; }
"!="                                        { return OPT_NE; }
"<="                                        { return OPT_LE; }
">="                                        { return OPT_GE; }
"<<"                                        { return OPT_SHL; }
">>"                                        { return OPT_SHR; }
"++"                                        { return OPT_INC; }
"--"                                        { return OPT_DEC; }
"->"                                        { return OPT_PTR; }

"*="                                        { return ASS_MUL; }
"/="                                        { return ASS_DIV; }
"%="                                        { return ASS_MOD; }
"+="                                        { return ASS_ADD; }
"-="                                        { return ASS_SUB; }
"<<="                                       { return ASS_SHL; }
">>="                                       { return ASS_SHR; }
"&="                                        { return ASS_AND; }
"^="                                        { return ASS_XOR; }
"|="                                        { return ASS_OR; }

[();,={}\[\]*|\^&<>+\-*//%~!.]              { return *yytext; }

[ \t\r]                                   /* skip whitespaces */
\n                                          { NEW_LINE_USER_ACTION; }
.                                           { return UNKNOWN; }
%%

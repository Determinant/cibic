%{
#include "cibic.tab.h"
%}

letter [a-zA-Z_$]
digit [0-9]
%s IN_BLOCK_COMMENT IN_INLINE_COMMENT IN_DIRECTIVE
%%

<INITIAL>{
"/*"    BEGIN(IN_BLOCK_COMMENT);
}
<IN_BLOCK_COMMENT>{
"*/"    BEGIN(INITIAL);
[^*\n]+   // eat comment in chunks
"*"       // eat the lone star
\n
}

<INITIAL>{
"//"    BEGIN(IN_INLINE_COMMENT);
}
<IN_INLINE_COMMENT>{
\n      BEGIN(INITIAL);
[^\n]+ 
}

<INITIAL>{
"#"    BEGIN(IN_INLINE_COMMENT);
}
<IN_DIRECTIVE>{
\n      BEGIN(INITIAL);
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

{letter}({letter}|{digit})*                 { 
    yylval.strval = strdup(yytext); 
    return IDENTIFIER; 
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

'([^\'\\]|\\.|\\[0-7][0-7]?[0-7]?|\\[xX][0-9a-fA-F]+)' { 
    yylval.strval = strdup(yytext);
    return CHAR_CONST; 
}

\"[^\n\"]*\" {
    yylval.strval = strndup(yytext + 1, strlen(yytext) - 2);
    return STR_CONST; 
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

[ \t\n\r]                                   /* skip whitespaces */
.                                           { return UNKNOWN; }
%%

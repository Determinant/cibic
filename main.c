#include <stdio.h>
#include <getopt.h>
#include "cibic.tab.h"
#include "ast.h"

extern int yyparse();
extern FILE *yyin;
char *fname;

int yywrap() {
    return 1;
}

int yyerror(char *s) {
}

void print_ast() {
    yyparse();
    if (fname)
        printf("AST for file: \"%s\"\n", fname);
    else
        printf("AST for stdin\n");

    if (ast_root)
    {
        cnode_debug_print(ast_root, 1);
    }
    else
        fprintf(stdout, "Syntax Error\n");
}

void print_help() {
}

static struct option lopts[] = {
    { "ast", no_argument, 0, 'a'},
    { "help", no_argument, 0, 'h'},
    {0, 0, 0, 0}
};

enum {
    PRINT_AST,
    PRINT_HELP
} mode = PRINT_HELP;

int main(int argc, char **argv) {
    int option_index = 0, i;
    while (1)
    {
        int c = getopt_long(argc, argv, "ah", lopts, &option_index);
        if (c == -1)
            break;
        switch (c)
        {
            case 0: break;
            case 'a': mode = PRINT_AST; break;
            case 'h': mode = PRINT_HELP; break;
        }
    }
    if (optind == argc - 1)
    {
        fname = argv[argc - 1];
        yyin = fopen(fname, "r");
        if (!yyin)
        {
            fprintf(stderr, "Error while opening file.");
            return 1;
        }
    }
    else if (optind == argc)
    {
        fname = NULL;
        yyin = stdin;
    }
    else
    {
        fprintf(stderr, "Only one source file is exepected.\n");
        return 1;
    }
    switch (mode)
    {
        case PRINT_AST: print_ast(); break;
        case PRINT_HELP: print_help(); break;
    }
    return 0;
}

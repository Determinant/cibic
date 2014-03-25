#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include "cibic.tab.h"
#include "ast.h"

extern char linebuff[];
extern char *lptr;
extern int yyparse();
extern FILE *yyin;
char *fname;

int yywrap() {
    return 1;
}

void print_error(char *err_msg, int row, int col) {
    *lptr = '\0';
    fprintf(stderr, "%d:%d: %s\n%s\n", 
            row, col, err_msg, linebuff);
    while (--col) putchar(' ');
    puts("^");
}

int yyerror(char *err_msg) {
    print_error(err_msg, 
            yylloc.first_line, yylloc.first_column);
    return 0;
}

void print_ast() {
    if (fname)
        fprintf(stderr, "AST for file: \"%s\"\n", fname);
    else
        fprintf(stderr, "AST for stdin\n");
    yyparse();
    if (ast_root)
    {
        cnode_debug_print(ast_root, 1);
    }
    else exit(1);
}

void print_help() {
    fprintf(stderr,
            "CBIC: C Implemented Bare and Ingenuous Compiler\n\n"
	        "Copyright (C) 2014 Ted Yin\n"
	        "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n"
	        "There is NO WARRANTY, to the extent permitted by law.\n"
	        "Usage: [options] filename\n"
            "Options:\n" 
            "\t --ast \t\t Print AST Construction\n"
            "\t --help \t Show this info\n"
        );
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
    int option_index = 0;
    while (1)
    {
        int c = getopt_long(argc, argv, "ah", lopts, &option_index);
        if (c == -1)
            break;
        switch (c)
        {
            case 0: break;
            case 'a': mode = PRINT_AST; break;
            case 'h': print_help(); return 0;
        }
    }
    if (optind == argc - 1)
    {
        fname = argv[argc - 1];
        yyin = fopen(fname, "r");
        if (!yyin)
        {
            fprintf(stderr, "Error while opening file.\n");
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

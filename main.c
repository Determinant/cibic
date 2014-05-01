#include <stdio.h>
#include <getopt.h>
#include <stdlib.h>
#include "cibic.tab.h"
#include "ast.h"
#include "ssa.h"
#include "semantics.h"

extern char linebuff[];
extern char *lptr;
extern int yyparse();
extern FILE *yyin;
char *fname;

int yywrap() {
    return 1;
}

void print_error(char *err_msg, char *lb, int row, int col, int warn) {
    *lptr = '\0';
    fprintf(stderr, "%d:%d: %s: %s\n",
            row, col, warn ? "warning" : "error", err_msg);
    if (lb)
    {
        fprintf(stderr, "%s\n", lb);
        while (--col) fprintf(stderr, "%c", ' ');
        fprintf(stderr, "^\n");
    }
    if (!warn) exit(1);
}

int yyerror(char *err_msg) {
    print_error(err_msg, linebuff,
            yylloc.first_line, yylloc.first_column, 0);
    return 0;
}

void print_ast() {
    if (fname)
        fprintf(stderr, "AST for file: \"%s\"\n", fname);
    else
        fprintf(stderr, "AST for stdin\n");
    cibic_init();
    yyparse();
    cnode_debug_print(ast_root, 1);
}

void print_sem() {
    cibic_init();
    yyparse();
    semantics_check(ast_root);
/*    cnode_debug_print(ast_root, 1); */
}

void lib_generate() {
    FILE *f = fopen("lib.s", "r");     
    static char buff[1024];
    if (f)
    {
        size_t size;
        while ((size = fread(buff, 1, 1024, f)))
            fwrite(buff, 1, size, stdout);
    }
}

void print_ssa() {
    cibic_init();
    yyparse();
    semantics_check(ast_root);
    ssa_generate();
    lib_generate();
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
    { "sem", no_argument, 0, 'm'},
    {0, 0, 0, 0}
};

enum {
    PRINT_AST,
    PRINT_HELP,
    PRINT_SEM,
    PRINT_SSA
} mode = PRINT_SSA;

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
            case 'm': mode = PRINT_SEM; break;
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
        case PRINT_SEM: print_sem(); break;
        default: print_ssa();
    }
    return 0;
}

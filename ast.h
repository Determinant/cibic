#ifndef AST_H
#define AST_H
#include <stdarg.h>
#include "const.h"
#include "semantics.h"
#include "cibic.tab.h"

typedef struct CNode {
    enum {
        /* Top Level */
        PROG,
        FUNC_DEF,
        DECL, /* declaration */
        DECLR, /* declarator */
        DECLRS,
        INIT_DECLR,
        INIT_DECLRS,
        INITR, /* initializer */
        TYPE_SPEC,
        FIELD, /* struct-or-union field */
        FIELDS,
        PLAIN_DECL,
        DECLS,
        FUNCS,
        TYPEDEF,

        /* Statments */
        STMT,

        /* Expressions  */
        EXP,
        ID, /* identifier */
        INT, /* INT_CONST */
        CHAR,
        STR,
        NOP,

        COMP_STMTS,
        COMP_DECLS,
        ARGS,
        PARAMS
    } type;
    union {
        int intval;
        int subtype;
        char *strval;
    } rec;
    struct {
        CType_t type;
        CVar_t var;
        CVList_t autos;
        long const_val;
        int is_const;
        int offset; /* offset from var */
    } ext;
    struct CNode *chd, *next;
    /* For error reporting */
    struct {
        int row, col;
    } loc;
} CNode;

CNode *cnode_add_loc(CNode *node, YYLTYPE loc);
CNode *cnode_create_ast(CNode *wrapped);
CNode *cnode_create_nop(void);
CNode *cnode_create_general(int type, int subtype, int pnum, va_list ap);
CNode *cnode_list_append(CNode *list, CNode *tail);
CNode *cnode_list_wrap(int type, CNode *list);

CNode *cnode_create_exp(int exp_type, int pnum, ...);
CNode *cnode_create_type_spec(int spec_type, int pnum, ...);
CNode *cnode_create_declr(int declr_type, int pnum, ...);
CNode *cnode_create_stmt(int stmt_type, int pnum, ...);
CNode *cnode_create_initr(int initr_type, CNode *body);

CNode *cnode_create_decl(CNode *type, CNode *init_declrs);
CNode *cnode_create_func(CNode *type, CNode *declr, CNode *stmt);
CNode *cnode_create_init_declr(CNode *declr, CNode *initr);
CNode *cnode_create_struct_field(CNode *type_spec, CNode *declrs);
CNode *cnode_create_plain_decl(CNode *type_spec, CNode *declr);
CNode *cnode_create_typedef(CNode *type, CNode *declrs);

CNode *cnode_create_identifier(char *val);
CNode *cnode_create_int_const(int val);
CNode *cnode_create_char_const(char *val);
CNode *cnode_create_str_const(char *val);

void cnode_debug_print(CNode *ast, int fancy);

extern CNode *ast_root;
extern CNode *null;

#endif

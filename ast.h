#ifndef AST_H
#define AST_H

#define EXP_POSTFIX     1024
#define POSTFIX_ARR     1025
#define POSTFIX_CALL    1026
#define POSTFIX_DOT     1027
#define POSTFIX_PTR     1028
#define EXP_CAST        1029
#define INITR_NORM      1030
#define INITR_ARR       1031
#define DECLR_FUNC      1032
#define DECLR_ARR       1033
#define STMT_EXP        1034
#define STMT_COMP       1035
#define STMT_IF         1036
#define STMT_WHILE      1037
#define STMT_FOR        1038
#define STMT_CONT       1039
#define STMT_BREAK      1040
#define STMT_RET        1041

typedef struct CNode {
    enum {
        /* Top Level */
        PROG,
        FUNC_DEF,
        DECL, /* declaration */
        DECLR, /* declarator */
        INIT_DECLR, 
        INITR, /* initializer */
        TYPE_SPEC,
        FIELD, /* struct-or-union field */
        PLAIN_DECL,

        /* Statments */
        STMT,

        /* Expressions  */
        EXP,
        TYPE_NAME,
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
    struct CNode *chd, *next;
    /* For error reporting */
    struct Location {
        int row, col;
    } loc;
} CNode;

void cnode_init();
CNode *cnode_create_nop();
CNode *cnode_create_general(int type, int subtype, int pnum, va_list ap); 
CNode *cnode_append(CNode *node, CNode *tail);

CNode *cnode_create_exp(int exp_type, int pnum, ...);
CNode *cnode_create_type_spec(int spec_type, int pnum, ...);
CNode *cnode_create_declr(int declr_type, int pnum, ...);
CNode *cnode_create_stmt(int stmt_type, int pnum, ...);
CNode *cnode_create_initr(int initr_type, CNode *body);

CNode *cnode_create_decl(CNode *type, CNode *init_declrs);
CNode *cnode_create_func(CNode *type, CNode *plain_decl, CNode *params, CNode *stmt);
CNode *cnode_create_init_declr(CNode *declr, CNode *initr);
CNode *cnode_create_struct_field(CNode *type_spec, CNode *declrs);
CNode *cnode_create_plain_decl(CNode *type_spec, CNode *declr);
CNode *cnode_create_comp_decls(CNode *decls);
CNode *cnode_create_comp_stmts(CNode *stmts);
CNode *cnode_create_args(CNode *arg_list);
CNode *cnode_create_params(CNode *plist);

CNode *cnode_create_identifier(char *val);
CNode *cnode_create_int_const(int val);
CNode *cnode_create_char_const(int val);
CNode *cnode_create_str_const(char *val);

void cnode_debug_print(CNode *ast);

extern CNode *ast_root;
extern CNode *null;

#endif

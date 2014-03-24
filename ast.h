#ifndef AST_H
#define AST_H

#define EXP_POSTFIX     1024
#define POSTFIX_ARR     1025
#define POSTFIX_CALL    1026
#define POSTFIX_DOT     1027
#define POSTFIX_PTR     1028
#define EXP_CAST        1029

typedef struct CNode {
    enum {
        /* Top Level */
        PROG,  FUNC_DEF, PARAMS, 
        DECL, /* declaration */
        DECLR, /* declarator */
        DECLRS, 
        INIT_DECLRS, INIT_DECLR, 
        INITR, /* initializer */
        TYPE_SPEC,
        STRUCT, UNION, 
        PLAIN_DECL, PLAIN_DECLR,

        /* Statments */
        EXP_STMT, /* expression-statment */
        COMP_STMT, IF_STMT, /* selection-statment */
        WHILE_STMT, FOR_STMT,
        CONT_STMT , BREAK_STMT, RET_STMT, /* 'continue', 'break', 'return' */

        /* Expressions (expressions use their token ID to denote their types */
        EXP,
        TYPE_NAME,
        ID, /* identifier */
        INT, /* INT_CONST */
        CHAR,
        STR
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

CNode *cnode_create_exp(int exp_type, int pnum, ...);
CNode *cnode_create_type_spec(int spec_type, int pnum, ...);
CNode *cnode_append(CNode *node, CNode *tail);
CNode *cnode_create_identifier(char *val);
CNode *cnode_create_int_const(int val);
CNode *cnode_create_char_const(int val);
CNode *cnode_create_str_const(char *val);
CNode *cnode_debug_print(CNode *ast);

extern CNode *ast_root;

#endif

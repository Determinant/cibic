typedef struct {
    enum {
        /* Top Level */
        PROG = 1024,  FUNC_DEF, PARAMS, 
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
        EXP 
    } type;
    union {
        int intval;
        char *strvar;
    } rec;
    struct CNode *chd, *next;
    /* For error reporting */
    struct Location {
        int row, col;
    } loc;
} CNode;

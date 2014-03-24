#include <stdlib.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include "ast.h"
#include "cibic.tab.h"
#define NEW_CNODE ((CNode *)malloc(sizeof(CNode)))

CNode *ast_root;

void cnode_init() {
}

CNode *cnode_create_nop() {
    CNode *nop = NEW_CNODE;
    nop->type = NOP;
    nop->next = nop->chd = NULL;
    return nop;
}

CNode *cnode_create_general(int type, int subtype, int pnum, va_list ap) {
    int i;
    CNode *exp = NEW_CNODE;
    exp->type = type;
    exp->rec.subtype = subtype;
    exp->next = NULL;
    for (i = 0; i < pnum; i++)
    {
        CNode *subexp = va_arg(ap, CNode*);
        if (subexp->next)
        {
            puts("asdf");
        }
        assert(subexp->next == NULL);
        subexp->next = exp->chd;
        exp->chd = subexp;
    }
    return exp;
}

CNode *cnode_append(CNode *node, CNode *tail) {
    if (node->type == NOP) 
    {
        free(node);
        return tail;
    }
    tail->next = node; 
    return tail;
}

CNode *cnode_create_identifier(char *val) {
    CNode *exp = NEW_CNODE;
    exp->type = ID;
    exp->chd = exp->next = NULL;
    exp->rec.strval = val;
    return exp;
}

CNode *cnode_create_int_const(int val) {
    /* TODO: overflow checking */
    CNode *exp = NEW_CNODE;
    exp->type = INT;
    exp->chd = exp->next = NULL;
    exp->rec.intval = val;
    return exp;
}

CNode *cnode_create_char_const(int val) {
    /* TODO: overflow checking */
    CNode *exp = NEW_CNODE;
    exp->type = CHAR;
    exp->chd = exp->next = NULL;
    exp->rec.intval = val;
    return exp;
}

CNode *cnode_create_str_const(char *val) {
    CNode *exp = NEW_CNODE;
    exp->type = STR;
    exp->chd = exp->next = NULL;
    exp->rec.strval = val;
    return exp;
}

CNode *cnode_create_exp(int exp_type, int pnum, ...) {
    CNode *ret;
    va_list ap;
    va_start(ap, pnum);
    ret = cnode_create_general(EXP, exp_type, pnum, ap);
    va_end(ap);
    return ret;
}

CNode *cnode_create_type_spec(int spec_type, int pnum, ...) {
    CNode *ret;
    va_list ap;
    va_start(ap, pnum);
    ret = cnode_create_general(TYPE_SPEC, spec_type, pnum, ap);
    va_end(ap);
    return ret;
}

CNode *cnode_create_declr(int declr_type, int pnum, ...) {
    CNode *ret;
    va_list ap;
    va_start(ap, pnum);
    ret = cnode_create_general(DECLR, declr_type, pnum, ap);
    va_end(ap);
    return ret;
}

CNode *cnode_create_stmt(int stmt_type, int pnum, ...) {
    CNode *ret;
    va_list ap;
    va_start(ap, pnum);
    ret = cnode_create_general(STMT, stmt_type, pnum, ap);
    va_end(ap);
    return ret;
}

CNode *cnode_create_initr(int initr_type, CNode *body) {
    CNode *initr = NEW_CNODE;
    initr->type = INITR;
    initr->rec.subtype = initr_type;
    initr->chd = body;
    return initr;
}

CNode *cnode_create_decl(CNode *type, CNode *init_declrs) {
    CNode *decl = NEW_CNODE;
    decl->type = DECL;
    decl->chd = type;
    type->next = init_declrs;
    return decl;
}

CNode *cnode_create_func(CNode *type, CNode *plain_decl, CNode *params, CNode *stmt) {
    CNode *func = NEW_CNODE;
    func->type = FUNC_DEF;
    func->chd = stmt;
    func->next = NULL;
    stmt->next = params;
    params->next = plain_decl;
    plain_decl->next = type;
    return func;
}

CNode *cnode_create_init_declr(CNode *declr, CNode *initr) {
    CNode *init_declr = NEW_CNODE;
    init_declr->type = INIT_DECLR;
    init_declr->chd = initr;
    initr->next = declr;
    return init_declr;
}

CNode *cnode_create_struct_field(CNode *type_spec, CNode *declrs) {
    CNode *field = NEW_CNODE;
    field->type = FIELD;
    field->chd = declrs;
    declrs->next = type_spec;
    return field;
}

CNode *cnode_create_plain_decl(CNode *type_spec, CNode *declr) {
    CNode *pdecl = NEW_CNODE;
    pdecl->type = PLAIN_DECL;
    pdecl->chd = declr;
    declr->next = type_spec;
    return pdecl;
}

CNode *cnode_create_comp_decls(CNode *decls) {
    CNode *comp_decls = NEW_CNODE;
    comp_decls->type = COMP_DECLS;
    comp_decls->chd = decls;
    return comp_decls;
}

CNode *cnode_create_comp_stmts(CNode *stmts) {
    CNode *comp_stmts = NEW_CNODE;
    comp_stmts->type = COMP_STMTS;
    comp_stmts->chd = stmts;
    comp_stmts->next = NULL;
    return comp_stmts;
}

CNode *cnode_create_args(CNode *arg_list) {
    CNode *args = NEW_CNODE;
    args->type = ARGS;
    args->chd = arg_list;
    args->next = NULL;
    return args;
}

CNode *cnode_create_params(CNode *plist) {
    CNode *params = NEW_CNODE;
    params->type = PARAMS;
    params->chd = plist;
    params->next = NULL;
    return params;
}

char *cnode_debug_type_repr(CNode *ast) {
    static buffer[1024];
    char *type = NULL;
    switch (ast->type)
    {
        case PROG:      type = "prog"; break;
        case FUNC_DEF:  type = "func"; break;
        case DECL:      type = "decl"; break;
        case DECLR:     type = "declr"; break;
        case INIT_DECLR:  type = "init_declr"; break;
        case PLAIN_DECL:  type = "p_decl"; break;
        case TYPE_NAME:  type = "type_name"; break;
        case COMP_STMTS: type = "stmts"; break;
        case COMP_DECLS: type = "decls"; break;
        case ARGS:  type = "args"; break;
        case PARAMS: type = "params"; break;
        case ID:  type = "id"; break;
        case INT:  type = "int"; break;
        case CHAR:  type = "char"; break;
        case STR:  type = "str"; break;
        case NOP: type = "nop"; break;
    }
    if (ast->type == EXP)
    {
        switch (ast->rec.subtype)
        {
            case ',': type = ","; break;
            case '=': type = "="; break;
            case ASS_MUL: type = "*="; break;
            case ASS_DIV: type = "/="; break;
            case ASS_MOD: type = "%="; break;
            case ASS_ADD: type = "+="; break;
            case ASS_SUB: type = "-="; break;
            case ASS_SHL: type = "<<="; break;
            case ASS_SHR: type = ">>="; break;
            case ASS_AND: type = "&="; break;
            case ASS_XOR: type = "^="; break;
            case ASS_OR: type = "|="; break;
            case OPT_OR: type = "||"; break;
            case OPT_AND: type = "&&"; break;
            case '|': type = "|"; break;
            case '^': type = "^"; break;
            case '&': type = "&"; break;
            case OPT_EQ: type = "=="; break;
            case OPT_NE: type = "!="; break;
            case '<': type = "<"; break;
            case '>': type = ">"; break;
            case OPT_LE: type = "<="; break;
            case OPT_GE: type = ">="; break;
            case OPT_SHL: type = "<<"; break;
            case OPT_SHR: type = ">>"; break;
            case '+': type = "+"; break;
            case '-': type = "-"; break;
            case '*': type = "*"; break;
            case '/': type = "/"; break;
            case '%': type = "%"; break;
            case EXP_CAST: type = "cast"; break;
            case OPT_INC: type = "++"; break;
            case OPT_DEC: type = "--"; break;
            case '~': type = "~"; break;
            case '!': type = "!"; break;
            case KW_SIZEOF: type = "sizeof"; break;
            case EXP_POSTFIX: type = "pofix"; break;
            case POSTFIX_ARR: type = "arr"; break;
            case POSTFIX_CALL: type = "call"; break;
            case POSTFIX_DOT: type = "dot"; break;
            case POSTFIX_PTR: type = "ptr"; break;
            default: assert(0);
        }
    }
    else if (ast->type == TYPE_SPEC)
    {
        switch (ast->rec.subtype)
        {
            case KW_VOID: type = "void"; break;
            case KW_CHAR: type = "char"; break;
            case KW_INT: type = "int"; break;
            case KW_STRUCT: type = "struct"; break;
            case KW_UNION: type = "union"; break;
            default: assert(0);
        }
    }
    else if (ast->type == DECLR)
    {
        switch (ast->rec.subtype)
        {
            case DECLR_FUNC: type = "func"; break;
            case DECLR_ARR: type = "arr"; break;
            default: assert(0);
        }
    }
    else if (ast->type == STMT)
    {
        switch (ast->rec.subtype)
        {
            case STMT_EXP: type = "exp"; break;
            case STMT_COMP: type = "blk"; break;
            case STMT_IF: type = "if"; break;
            case STMT_WHILE: type = "while"; break;
            case STMT_FOR: type = "for"; break;
            case STMT_CONT: type = "cont"; break;
            case STMT_BREAK: type = "break"; break;
            case STMT_RET: type = "ret"; break;
            default: assert(0);
        }
    }
    else if (ast->type == INITR)
    {
        switch (ast->rec.subtype)
        {
            case INITR_NORM: type = "initr_n"; break;
            case INITR_ARR: type = "initr_a"; break;
            default: assert(0);
        }
    }
    else assert(type);
    sprintf(buffer, "%s", type);
    return buffer;
}

void cnode_debug_print_(CNode *ast) {
    printf("(%s", cnode_debug_type_repr(ast));
    for (ast = ast->chd; ast; ast = ast->next)
    {
        printf(" ");
        cnode_debug_print_(ast);
    }
    printf(")");
}

void cnode_debug_print(CNode *ast) {
    cnode_debug_print_(ast);
    puts("");
}

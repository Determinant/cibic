#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "ast.h"
#include "cibic.tab.h"
#define NEW_CNODE ((CNode *)malloc(sizeof(CNode)))

CNode *ast_root;

void cnode_reverse_chd(CNode *node) {
    static CNode *chdn[MAX_CHDN];
    CNode *p;
    int n = 0;
    for (p = node->chd; p; p = p->next)
        cnode_reverse_chd(p);
    for (p = node->chd; p; p = p->next)
        chdn[n++] = p;
    if (n)
    {
        node->chd = chdn[--n];
        for (; n; n--)
            chdn[n]->next = chdn[n - 1];
        chdn[0]->next = NULL;
    }
}

CNode *cnode_create_ast(CNode *wrapped) {
    cnode_reverse_chd(wrapped);
    return wrapped;
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
    exp->next = exp->chd = NULL;
    for (i = 0; i < pnum; i++)
    {
        CNode *subexp = va_arg(ap, CNode*);
#ifdef CNODE_DEBUG
        assert(subexp->next == NULL);
#endif
        subexp->next = exp->chd;
        exp->chd = subexp;
    }
    return exp;
}

CNode *cnode_list_append(CNode *list, CNode *tail) {
    if (list->type == NOP) 
    {
        free(list);
        return tail;
    }
    tail->next = list; 
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

CNode *cnode_create_char_const(char *val) {
    /* TODO: overflow checking */
    CNode *exp = NEW_CNODE;
    exp->type = CHAR;
    exp->chd = exp->next = NULL;
    exp->rec.strval = val;
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
    initr->next = NULL;
    return initr;
}

CNode *cnode_create_decl(CNode *type, CNode *init_declrs) {
    CNode *decl = NEW_CNODE;
#ifdef CNODE_DEBUG
    assert(type->next == NULL);
    assert(init_declrs->next == NULL);
#endif
    decl->type = DECL;
    decl->next = NULL;
    decl->chd = init_declrs;
    init_declrs->next = type;
    return decl;
}

CNode *cnode_create_func(CNode *type, CNode *plain_decl, CNode *params, CNode *stmt) {
    CNode *func = NEW_CNODE;
#ifdef CNODE_DEBUG
    assert(type->next == NULL);
    assert(plain_decl->next == NULL);
    assert(params->next == NULL);
    assert(stmt->next == NULL);
#endif
    func->type = FUNC_DEF;
    func->next = NULL;
    func->chd = stmt;
    stmt->next = params;
    params->next = plain_decl;
    plain_decl->next = type;
    return func;
}

CNode *cnode_create_init_declr(CNode *declr, CNode *initr) {
    CNode *init_declr = NEW_CNODE;
#ifdef CNODE_DEBUG
    assert(declr->next == NULL);
    assert(initr->next == NULL);
#endif
    init_declr->type = INIT_DECLR;
    init_declr->next = NULL;
    init_declr->chd = initr;
    initr->next = declr;
    return init_declr;
}

CNode *cnode_create_struct_field(CNode *type_spec, CNode *declrs) {
    CNode *field = NEW_CNODE;
#ifdef CNODE_DEBUG
    assert(type_spec->next == NULL);
    assert(declrs->next == NULL);
#endif
    field->type = FIELD;
    field->next = NULL;
    field->chd = declrs;
    declrs->next = type_spec;
    return field;
}

CNode *cnode_create_plain_decl(CNode *type_spec, CNode *declr) {
    CNode *pdecl = NEW_CNODE;
#ifdef CNODE_DEBUG
    assert(type_spec->next == NULL);
    assert(declr->next == NULL);
#endif
    pdecl->type = PLAIN_DECL;
    pdecl->next = NULL;
    pdecl->chd = declr;
    declr->next = type_spec;
    return pdecl;
}

CNode *cnode_list_wrap(int type, CNode *list) {
    CNode *wlist = NEW_CNODE;
    wlist->type = type;
    wlist->next = NULL;
    wlist->chd = list;
    return wlist;
}

char *cnode_debug_type_repr(CNode *ast) {
    static char buffer[1024], abuff[1024];
    char *type, *aptr = abuff;
    switch (ast->type)
    {
        case PROG:      type = "prog"; break;
        case FUNC_DEF:  type = "func"; break;
        case DECLS:     type = "prg_decls"; break;
        case FUNCS:     type = "prg_funcs"; break;
        case DECL:      type = "decl"; break;
        case INIT_DECLR:  type = "init_declr"; break;
        case PLAIN_DECL:  type = "p_decl"; break;
        case TYPE_NAME:  type = "type_name"; break;
        case COMP_STMTS: type = "blk_stmts"; break;
        case COMP_DECLS: type = "blk_decls"; break;
        case DECLRS: type = "declrs"; break;
        case INIT_DECLRS: type = "i_declrs"; break;
        case ARGS:  type = "args"; break;
        case PARAMS: type = "params"; break;
        case ID:
            type = "id"; 
            aptr += sprintf(abuff, "%s", ast->rec.strval);
            break;
        case INT:
            type = "int"; 
            aptr += sprintf(abuff, "%d", ast->rec.intval);
            break;
        case CHAR:
            type = "char";
            aptr += sprintf(abuff, "%s", ast->rec.strval);
            break;
        case STR:
            type = "str";
            aptr += sprintf(abuff, "\"%s\"", ast->rec.strval);
            break;
        case FIELD: type = "field"; break;
        case FIELDS: type = "fields"; break;
        case NOP: type = "nop"; break;
        case EXP: 
        case INITR: 
        case TYPE_SPEC: 
        case STMT:
        case DECLR:
            type = NULL; break;
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
            case '*': type = "*"; break;
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
    else 
    {
        if (type == NULL)
            puts("");
        assert(type);
    }
    if (aptr == abuff)
        sprintf(buffer, "%s(%d,%d)", type,
                ast->loc.row, ast->loc.col);
    else
    {
        *aptr = '\0';
        sprintf(buffer, "%s:%s(%d,%d)", type, abuff, 
                ast->loc.row, ast->loc.col);
    }
    return buffer;
}

void cnode_debug_print_plain(CNode *ast) {
    fprintf(stderr, "(%s", cnode_debug_type_repr(ast));
    for (ast = ast->chd; ast; ast = ast->next)
    {
        fprintf(stderr, " ");
        cnode_debug_print_plain(ast);
    }
    fprintf(stderr, ")");
}

void cnode_debug_print_fancy(CNode *ast, int lvl) {
    static int show[1024];
    int i;
    show[lvl] = 1;
    for (i = 0; i < lvl - 1; i++)
        fprintf(stderr, "%c    ", show[i] ? '|' : ' ');
    if (lvl)
        fprintf(stderr, "|____");
    fprintf(stderr, "[%s]\n", cnode_debug_type_repr(ast));
    for (ast = ast->chd; ast; ast = ast->next)
    {
        if (!ast->next) show[lvl] = 0;
        cnode_debug_print_fancy(ast, lvl + 1);
    }
}

void cnode_debug_print(CNode *ast, int fancy) {
    if (fancy)
        cnode_debug_print_fancy(ast, 0);
    else
        cnode_debug_print_plain(ast);
    puts("");
}


CNode *cnode_add_loc(CNode *node, YYLTYPE loc) {
    node->loc.row = loc.first_line;
    node->loc.col = loc.first_column;
    return node;
}

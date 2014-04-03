#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "semantics.h"
#include "ast.h"
#define NEW(type) ((type *)malloc(sizeof(type)))
#define CHECK_TYPE(p, _type) assert(p->type == _type)

#ifdef CIBIC_DEBUG
CTable_t ctable_create(Hashfunc_t hfunc, Printfunc_t pfunc) {
    CTable_t ct = NEW(CTable);
    memset(ct->head, 0, sizeof(CTNode*) * MAX_TABLE_SIZE);
    ct->hfunc = hfunc;
    ct->pfunc = pfunc;
    return ct;
}
#else
CTable_t ctable_create(Hashfunc_t hfunc) {
    CTable_t ct = NEW(CTable);
    memset(ct->head, 0, sizeof(CTNode*) * MAX_TABLE_SIZE);
    ct->hfunc = hfunc;
    return ct;
}
#endif

void *ctable_lookup(CTable_t ct, const char *key) {
    unsigned int hv = ct->hfunc(key) % MAX_TABLE_SIZE;
    CTNode *p = ct->head[hv];
    for (; p; p = p->next)
        if (!strcmp(p->key, key))
            return p->val;
    return NULL; /* not found */
}

int ctable_insert(CTable_t ct, const char *key, void *val, int lvl) {
    unsigned int hv = ct->hfunc(key) % MAX_TABLE_SIZE;
    CTNode *p = ct->head[hv];
    CTNode *np;
    for (; p && p->lvl == lvl; p = p->next)
        if (!strcmp(p->key, key))
            return 0; /* conflict */
    np = NEW(CTNode);
    np->key = key;
    np->val = val;
    np->lvl = lvl;
    np->next = ct->head[hv];
    ct->head[hv] = np;
    return 1;
}

void ctable_clip(CTable_t ct, const char *key, int max_lvl) {
    unsigned int hv = ct->hfunc(key) % MAX_TABLE_SIZE;
    CTNode *p = ct->head[hv], *np;
    for (; p && p->lvl > max_lvl; p = np)
    {
        np = p->next;
        free(p);
    }
    ct->head[hv] = p;
}

CScope_t cscope_create() {
    CScope_t p = NEW(CScope);
    p->lvl = -1;
    p->top = NULL;
#ifdef CIBIC_DEBUG
    p->tvar = ctable_create(bkdr_hash, ctable_cvar_print);
    p->ttype = ctable_create(bkdr_hash, ctable_ctype_print);
#else
    p->tvar = ctable_create(bkdr_hash);
    p->ttype = ctable_create(bkdr_hash);
#endif
    cscope_enter(p);
    return p;
}

int cscope_push_var(CScope_t cs, CVar_t var) {
#ifdef CIBIC_DEBUG
    assert(cs->top);
#endif
    if (ctable_insert(cs->tvar, var->name, var, cs->lvl))
    {
        var->next = cs->top->vhead;
        cs->top->vhead = var;
        return 1;
    }
    else return 0; /* naming conflict */
}

int cscope_push_type(CScope_t cs, CType_t type) {
#ifdef CIBIC_DEBUG
    assert(cs->top);
#endif
    if (ctable_insert(cs->ttype, type->name, type, cs->lvl))
    {
        type->next = cs->top->thead;
        cs->top->thead = type;
        return 1;
    }
    else return 0; /* naming conflict */
}

void cscope_enter(CScope_t cs) {
    CSNode *np = NEW(CSNode);
    np->next = cs->top;
    np->vhead = NULL;
    np->thead = NULL;
    cs->top = np;
    cs->lvl++;
}

void cscope_exit(CScope_t cs) {
    CSNode *top_o = cs->top;
    CVar_t vp;
    CType_t tp;
    cs->lvl--;
    cs->top = top_o->next;
    for (vp = top_o->vhead; vp; vp = vp->next)
        ctable_clip(cs->tvar, vp->name, cs->lvl);
    for (tp = top_o->thead; tp; tp = tp->next)
        ctable_clip(cs->ttype, tp->name, cs->lvl);
    free(top_o);
}

void ctable_debug_print(CTable_t ct) {
    int i;
    fprintf(stderr, "*** CTable ***\n");
    for (i = 0; i < MAX_TABLE_SIZE; i++)
        if (ct->head[i])
        {
            CTNode *p;
            fprintf(stderr, "[%04d]", i);
            for (p = ct->head[i]; p; p = p->next)
                fprintf(stderr, "->[%s:%d]", ct->pfunc(p->val), p->lvl);
            fprintf(stderr, "\n");
        }
    fprintf(stderr, "*** CTable ***\n");
}

void cscope_debug_print(CScope_t cs) {
    int lvl = cs->lvl;
    CSNode *p;
    fprintf(stderr, "\n****** CScope ******\n");
    for (p = cs->top; p; p = p->next)
    {
        CVar_t vp;
        CType_t tp;
        fprintf(stderr, "Level %d:\n", lvl--);
        fprintf(stderr, "Vars: ");
        for (vp = p->vhead; vp; vp = vp->next)
            fprintf(stderr, "%s ", vp->name);
        fprintf(stderr, "\nTypes: ");
        for (tp = p->thead; tp; tp = tp->next)
            fprintf(stderr, "%s ", tp->name);
        fprintf(stderr, "\n\n");
    }
    fprintf(stderr, "Var Table:\n");
    ctable_debug_print(cs->tvar);
    fprintf(stderr, "Type Table:\n");
    ctable_debug_print(cs->ttype);
    fprintf(stderr, "****** CScope ******\n\n");
}

CVar_t cscope_lookup_var(CScope_t cs, const char *name) {
    return ctable_lookup(cs->tvar, name);
}

CType_t cscope_lookup_type(CScope_t cs, const char *name) {
    return ctable_lookup(cs->ttype, name);
}

unsigned int bkdr_hash(const char *str) {
    unsigned int seed = 131;
    unsigned int hv = 0;
    while (*str)
        hv = hv * seed + (unsigned)(*str++);
    return hv;
}

const char *ctable_cvar_print(void *var) {
    static char buff[MAX_DEBUG_PRINT_BUFF];
    sprintf(buff, "%s", ((CVar_t )var)->name);
    return buff;
}

const char *ctable_ctype_print(void *type) {
    static char buff[MAX_DEBUG_PRINT_BUFF];
    sprintf(buff, "%s", ((CType_t )type)->name);
    return buff;
}

CVar_t cvar_create(const char *name, CType_t type) {
    CVar_t cv = NEW(CVar);
    cv->name = name;
    cv->type = type;
    return cv;
}

CType_t ctype_create(const char *name, int type) {
    CType_t ct = NEW(CType);
    ct->name = name;
    ct->type = type;
    switch (type)
    {
        case CINT: ct->size = INT_SIZE; break;
        case CCHAR: ct->size = CHAR_SIZE; break;
        case CVOID: ct->size = 0; break;
    }
    return ct;
}

void ctype_print(CType_t);

void cvar_print(CVar_t cv) {
    fprintf(stderr, "[var:%s]->", cv->name);
    ctype_print(cv->type);
}

void ctype_print(CType_t ct) {
    switch (ct->type)
    {
        case CINT: fprintf(stderr, "[int]"); break;
        case CCHAR: fprintf(stderr, "[char]"); break;
        case CVOID: fprintf(stderr, "[void]"); break;
        case CSTRUCT: case CUNION:
            {
                CTable_t f = ct->rec.fields;
                int i;
                CTNode *fn; 
                fprintf(stderr, "[%s:(name:%s|fields:", ct->type == CSTRUCT ? "struct" : "union"
                                                , ct->name);
                if (f)
                {
                    int first = 1;
                    for (i = 0; i < MAX_TABLE_SIZE; i++)
                        for (fn = f->head[i]; fn; fn = fn->next)
                        {
                            fprintf(stderr, "%s", first ? (first = 0, "") : ",");
                            fprintf(stderr, "%s:", fn->key);
                            cvar_print((CVar_t)fn->val);
                        }
                }
                fprintf(stderr, ")]");
            }
            break;
        case CARR:
            {
                fprintf(stderr, "[arr:(%d)]->", ct->rec.arr.len);
                ctype_print(ct->rec.arr.elem);
            }
            break;
        case CPTR:
            {
                fprintf(stderr, "[ptr]->");
                ctype_print(ct->rec.ref);
            }
            break;
        case CFUNC:
            {
                CVar_t p;
                fprintf(stderr, "[func:(name:%s|params:", ct->name);
                for (p = ct->rec.func.params; p; p = p->next)
                {
                    cvar_print(p);
                    if (p->next) fprintf(stderr, ",");
                }
                fprintf(stderr, "|local:");
                for (p = ct->rec.func.local; p; p = p->next)
                {
                    cvar_print(p);
                    if (p->next) fprintf(stderr, ",");
                }
                fprintf(stderr, ")]->");
                ctype_print(ct->rec.func.ret);
            }
            break;
    }
}

CTable_t semantics_fields(CNode *, CScope_t scope);
CType_t semantics_type_spec(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, TYPE_SPEC);
    CType_t type;
    switch (p->rec.subtype)
    {
        case KW_VOID: type = ctype_create("", CVOID); break;
        case KW_CHAR: type = ctype_create("", CCHAR); break;
        case KW_INT: type = ctype_create("", CINT); break;
        case KW_STRUCT: case KW_UNION:
            {
                CNode *id = p->chd,
                      *fields = p->chd->next;
                type = ctype_create(id->type == NOP ? "" : id->rec.strval,
                                    p->rec.subtype == KW_STRUCT ? CSTRUCT : CUNION);
                if (fields->type == NOP)
                {
                    type = cscope_lookup_type(scope, id->rec.strval);
                    if (!type) puts("type not exist");
                }
                else
                {
                    type->rec.fields = semantics_fields(fields, scope);
                    if (id->type != NOP)
                        if (!cscope_push_type(scope, type))
                            puts("fuck type");
                }
            }
            break;
        default: assert(0);
    }
    return type;
}

CVar_t semantics_declr(CNode *, CType_t );
CVar_t semantics_p_decl(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, PLAIN_DECL);
    return semantics_declr(p->chd->next,
                            semantics_type_spec(p->chd, scope));
}

CVar_t semantics_params(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, PARAMS);
    p = p->chd;
    if (p->type == NOP) return NULL; /* void arguments */
    CVar_t params = semantics_p_decl(p, scope);
    cscope_push_var(scope, params);
    for (p = p->next; p; p = p->next)
    {
        CVar_t var = semantics_p_decl(p, scope);
        if (scope)  /* params inside a function definition */
            if (!cscope_push_var(scope, var))
                puts("fuck params");
        var->next = params;
        params = var;
    }
    return params;
}

CVar_t semantics_p_declr(CNode *p, CType_t type_spec) {
    /* deal with pointer prefix */
    CNode *t;
    CType_t tt, ptype;
    const char *name;
    if (p->type == ID) 
    {
        ptype = type_spec;              /* filled by type spec */
        name = p->rec.strval;
    }
    else
    {
        ptype = ctype_create("", CPTR); /* pointer */
        for (t = p, tt = ptype;; t = t->chd)
        {
            if (t->chd->type == ID)
            {
                tt->rec.ref = type_spec; /* filled by type spec */
                name = t->chd->rec.strval;
                break;
            }
            tt->rec.ref = ctype_create("", CPTR);
            tt = tt->rec.ref;
        }
    }
    return cvar_create(name, ptype);
}

CVar_t semantics_declr(CNode *p, CType_t type_spec) {
    CType_t type;
    const char *name;
    if (p->type == ID || p->rec.subtype == '*')
        return semantics_p_declr(p, type_spec);
    switch (p->rec.subtype)
    {
        case DECLR_FUNC: 
            {
                CVar_t p_declr = semantics_p_declr(p->chd, type_spec);
                type = ctype_create("", CFUNC); /* function declr */
                type->rec.func.params = semantics_params(p->chd->next, NULL);
                /* incomplete type */
                type->rec.func.local = NULL;
                type->rec.func.ret = p_declr->type;
                name = p_declr->name;
                free(p_declr);
            }
            break;
        case DECLR_ARR:
            {
                CNode *t;
                CType_t tt;
                type = ctype_create("", CARR); /* array declr */
                for (t = p, tt = type;; t = t->chd)
                {
                    /* TODO: range checking */
                    tt->rec.arr.len = t->chd->next->rec.intval;     /* array length */
                    if (t->chd->type == ID || t->chd->rec.subtype == '*')
                    {
                        CVar_t p_declr = semantics_p_declr(t->chd, type_spec);
                        tt->rec.arr.elem = p_declr->type;
                        name = p_declr->name;
                        free(p_declr);
                        break;
                    }
                    tt->rec.arr.elem = ctype_create("", CARR);
                    tt = tt->rec.arr.elem;
                }
            }
            break;
        default: assert(0);
    }
    return cvar_create(name, type);
}

CTable_t semantics_fields(CNode *p, CScope_t scope) {
#ifdef CIBIC_DEBUG
    CTable_t ct = ctable_create(bkdr_hash, ctable_cvar_print);
#else
    CTable_t ct = ctable_create(bkdr_hash);
#endif
    for (p = p->chd; p; p = p->next)
    {
        CNode *declr = p->chd->next->chd;
        for (; declr; declr = declr->next)
        {
            CVar_t var = semantics_declr(declr, 
                                        semantics_type_spec(p->chd, scope));
            /* TODO: conflicts report */
            if (!ctable_insert(ct, var->name, var, 0))
                puts("fuck fields");
        }
    }
    return ct;
}

CVar_t semantics_decl(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, DECL);
    CNode *init = p->chd->next;
    CType_t type = semantics_type_spec(p->chd, scope);
    CVar_t res = NULL;
    switch (type->type)
    {
        case CSTRUCT: 
        case CUNION:
            cscope_push_type(scope, type); break;
        case CINT:
        case CCHAR:
        case CVOID:
            /* TODO: useless typename warning */
            ;
            break;
        default: assert(0);
    }
    if (init->type != NOP)
    {
        CNode *p;
        for (p = init->chd; p; p = p->next)
        {
            /* TODO: initializer checking */
            CVar_t var = semantics_declr(p->chd, type);
            if (!cscope_push_var(scope, var))
                puts("fuck decl");
            var->next = res;
            res = var;
        }
    }
    return res;
}

CVar_t semantics_comp(CNode *, CScope_t);
CVar_t semantics_stmt(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, STMT);
    switch (p->rec.subtype)
    {
        case STMT_EXP: 
            /* TODO: expression handle */
            break;
        case STMT_COMP:
            return semantics_comp(p, scope);
        case STMT_IF:
            /* TODO: `if' statement */
            ;
            break;
        case STMT_FOR:
            /* TODO: `for' statement */
            ;
            break;
        case STMT_WHILE:
            /* TODO: `while' statement */
            ;
            break;
        case STMT_CONT:
            /* TODO: `continue' statement */
            ;
            break;
        case STMT_BREAK:
            /* TODO: `break' statement */
            ;
            break;
        case STMT_RET:
            /* TODO: `return' statement */
            ;
            break;
        default: assert(0);
    }
    return NULL;
}

CVar_t semantics_comp(CNode *p, CScope_t scope) {
    CNode *decls = p->chd,
          *stmts = p->chd->next, *i;
    CVar_t res = NULL;
    cscope_enter(scope);
    if (decls->chd->type != NOP)
        for (i = decls->chd; i; i = i->next)
        {
            CVar_t vlist = semantics_decl(i, scope);
            if (vlist)  /* collect local vars */
            {
                CVar_t p;
                for (p = vlist; p->next; p = p->next);
                p->next = res;
                res = vlist;
            }
        }
    if (stmts->chd->type != NOP)
        for (i = stmts->chd; i; i = i->next)
        {
            CVar_t vlist = semantics_stmt(i, scope);
            if (vlist)  /* collect nested local vars */
            {
                CVar_t p;
                for (p = vlist; p->next; p = p->next);
                p->next = res;
                res = vlist;
            }
        }
    cscope_exit(scope);
    return res;
}

CType_t semantics_func(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, FUNC_DEF);
    CNode *chd = p->chd->next;
    CType_t func = ctype_create(chd->rec.strval, CFUNC);
    chd = chd->next;
    func->rec.func.ret = semantics_type_spec(p->chd, scope);   /* check return type */
    cscope_enter(scope);
    func->rec.func.params = semantics_params(chd, scope);       /* check params */
    func->rec.func.local = semantics_comp(chd->next, scope); /* check comp */
    cscope_exit(scope);
    ctype_print(func);
    fprintf(stderr, "\n");
    return func;
}

void semantics_check_(CNode *p, CScope_t scope) {
    p = p->chd;
    switch (p->type)
    {
        case FUNC_DEF: semantics_func(p, scope); break;
        default: ;
    }
}

void semantics_check(CNode *ast) {
    CScope_t scope = cscope_create();
    /* add top-level basic types */
    cscope_push_type(scope, ctype_create("int", CINT));
    cscope_push_type(scope, ctype_create("char", CCHAR));
    cscope_push_type(scope, ctype_create("void", CVOID));
    semantics_check_(ast, scope);
}

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "semantics.h"
#include "ast.h"
#define NEW(type) ((type *)malloc(sizeof(type)))
#define CHECK_TYPE(p, _type) assert(p->type == _type)
#define ERROR(ast) print_error(err_buff, NULL, (ast)->loc.row, (ast)->loc.col, 0)
#define WARNING(ast) print_error(err_buff, NULL, (ast)->loc.row, (ast)->loc.col, 1)

extern void print_error(char *, char *, int, int, int);
extern char *load_line(int);
static char err_buff[MAX_ERROR_BUFF];
static CType_t basic_type_int; 
static CType_t basic_type_char;
static CType_t basic_type_void;

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
void ctable_destory(CTable_t ct) {
    int i;
    for (i = 0; i < MAX_TABLE_SIZE; i++)
    {
        CTNode *p, *np;
        for (p = ct->head[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
    }
}

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
    p->func = NULL;
    p->inside_loop = 0;
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
        CSVar *csvar = NEW(CSVar);
        csvar->var = var;
        csvar->next = cs->top->vhead;
        cs->top->vhead = csvar;
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
        CSType *cstype = NEW(CSType);
        cstype->type = type;
        cstype->next = cs->top->thead;
        cs->top->thead = cstype;
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
    CSVar *vp;
    CSType *tp;
    cs->lvl--;
    cs->top = top_o->next;
    for (vp = top_o->vhead; vp; vp = vp->next)
        ctable_clip(cs->tvar, vp->var->name, cs->lvl);
    for (tp = top_o->thead; tp; tp = tp->next)
        ctable_clip(cs->ttype, tp->type->name, cs->lvl);
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
        CSVar *vp;
        CSType *tp;
        fprintf(stderr, "Level %d:\n", lvl--);
        fprintf(stderr, "Vars: ");
        for (vp = p->vhead; vp; vp = vp->next)
            fprintf(stderr, "%s ", vp->var->name);
        fprintf(stderr, "\nTypes: ");
        for (tp = p->thead; tp; tp = tp->next)
            fprintf(stderr, "%s ", tp->type->name);
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
    sprintf(buff, "%s@%lx", ((CVar_t )var)->name, (size_t)var);
    return buff;
}

const char *ctable_ctype_print(void *type) {
    static char buff[MAX_DEBUG_PRINT_BUFF];
    sprintf(buff, "%s@%lx", ((CType_t )type)->name, (size_t)type);
    return buff;
}

CVar_t cvar_create(const char *name, CType_t type, CNode *ast) {
    CVar_t cv = NEW(CVar);
    cv->name = name;
    cv->type = type;
    cv->ast = ast;
    return cv;
}

CType_t ctype_create(const char *name, int type, CNode *ast) {
    CType_t ct = NEW(CType);
    ct->name = name;
    ct->type = type;
    ct->ast = ast;
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
    fprintf(stderr, "[var@%lx:%s]->", (size_t)cv, cv->name);
    ctype_print(cv->type);
}

void ctype_print(CType_t ct) {
    switch (ct->type)
    {
        case CINT: 
            fprintf(stderr, "[int]"); break;
        case CCHAR:
            fprintf(stderr, "[char]"); break;
        case CVOID:
            fprintf(stderr, "[void]"); break;
        case CSTRUCT:
        case CUNION:
            {
                CTable_t f = ct->rec.fields;
                int i;
                CTNode *fn; 
                fprintf(stderr, "[%s@%lx:(name:%s|fields:",
                        ct->type == CSTRUCT ? "struct" : "union",
                        (size_t)ct,
                        ct->name);
                if (f)
                {
                    int first = 1;
                    for (i = 0; i < MAX_TABLE_SIZE; i++)
                        for (fn = f->head[i]; fn; fn = fn->next)
                        {
                            fprintf(stderr, "%s", first ? (first = 0, "") : ",");
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

static CType_t struct_type_merge(CType_t new, CScope_t scope) {
    /* Note: we shall try to lookup first instead of pushing !! */
    CType_t old = cscope_lookup_type(scope, new->name);
    if (!old) /* create it if it does not exist */
    {
        cscope_push_type(scope, new);
        return new;
    } /* otherwise we have it */
    if (old->type != new->type) /* not a struct or union */
    {
        sprintf(err_buff, "conflicting types of '%s'", new->name);
        ERROR(new->ast);
    }
    /* otherwise it is a struct or union */
    if (!new->rec.fields) /* use the old definition */
        return old;
    /* otherwise there's a completion definition */
    if (cscope_push_type(scope, new)) /* try to push the defintion */
        return new;
    /* conflict appears */
    if (old->rec.fields) /* if the old one is complete */
    {
        sprintf(err_buff, "redefinition of '%s'", new->name);
        ERROR(new->ast);
    }
    /* otherwise incomplete, thus complete the type */
    old->next = new->next;
    old->rec.fields = new->rec.fields;
    old->ast = new->ast;
    free(new);
    return old;
}

int is_same_type(CType_t typea, CType_t typeb, int arr2ptr) {
    if (typea == typeb) return 1;
    if (arr2ptr)
        do {
            int ta = typea->type, tb = typeb->type;
            if (ta == CPTR) typea = typea->rec.ref;
            else if (ta == CARR) typea = typea->rec.arr.elem;
            else break;
            if (tb == CPTR) typeb = typeb->rec.ref;
            else if (tb == CARR) typeb = typeb->rec.arr.elem;
            else break;
            return is_same_type(typea, typeb, 0);
        } while (0);
    if (typea->type != typeb->type) return 0;
    switch (typea->type)
    {
        case CSTRUCT: case CUNION:
            return typea == typeb;
        case CARR:
            if (typea->rec.arr.len != typeb->rec.arr.len)
                return 0;
            return is_same_type(typea->rec.arr.elem,
                    typeb->rec.arr.elem, 0);
        case CPTR:
            return is_same_type(typea->rec.ref,
                    typeb->rec.ref, 0);
        case CFUNC:
            {
                CVar_t pa, pb;
                for (pa = typea->rec.func.params, 
                        pb = typeb->rec.func.params; pa && pb;
                        pa = pa->next, pb = pb->next)
                    if (!is_same_type(pa->type, pb->type, 0))
                        return 0;
                if (pa || pb) 
                    return 0; /* different number of parameters */
                return is_same_type(typea->rec.func.ret,
                        typeb->rec.func.ret, 0);
            }
        case CINT: case CCHAR: case CVOID:
            ;
            break;
    }
    return 1;
}

static CVar_t var_merge(CVar_t new, CScope_t scope) {
    CVar_t old;
    if (cscope_push_var(scope, new))
        return new;
    else
        old = cscope_lookup_var(scope, new->name);
    if (!is_same_type(old->type, new->type, 0) || scope->lvl > 0)
    {
        sprintf(err_buff, "conflicting types of '%s'", new->name);
        ERROR(new->ast);
    }
    free(new);
    return old;
}

CTable_t semantics_fields(CNode *, CScope_t scope);
CType_t semantics_type_spec(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, TYPE_SPEC);
    CType_t type;
    switch (p->rec.subtype)
    {
        case KW_VOID:
            type = ctype_create("", CVOID, p); break;
        case KW_CHAR:
            type = ctype_create("", CCHAR, p); break;
        case KW_INT:
            type = ctype_create("", CINT, p); break;
        case KW_STRUCT: case KW_UNION:
            {
                CNode *id = p->chd,
                      *fields = p->chd->next;
                type = ctype_create(id->type == NOP ? "" : id->rec.strval,
                        p->rec.subtype == KW_STRUCT ? CSTRUCT : CUNION,
                        p);
                if (fields->type == NOP)
                    type->rec.fields = NULL; /* incomplete type */
                else
                    type->rec.fields = semantics_fields(fields, scope);

                if (id->type != NOP)
                    type = struct_type_merge(type, scope);
            }
            break;
        default: assert(0);
    }
    return type;
}

int type_is_complete(CType_t type) {
    switch(type->type)
    {
        case CINT: case CCHAR: case CPTR: case CARR:
            return 1;
        case CSTRUCT: case CUNION:
            return type->rec.fields != NULL;
        default: ; /* TODO: CFUNC CVOID */
    }
    return 1;
}

CVar_t semantics_declr(CNode *, CType_t, CScope_t);
CVar_t semantics_p_decl(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, PLAIN_DECL);
    CVar_t var = semantics_declr(p->chd->next,
            semantics_type_spec(p->chd, scope),
            scope);
    if (!type_is_complete(var->type))
    {
        sprintf(err_buff, "parameter '%s' has incomplete type", var->name);
        ERROR(var->ast);
    }
    return var;
}

CType_t semantics_type_name(CNode *p, CScope_t scope) {
    CNode *t, *ast;
    CType_t tt, type;
    if (p->type == TYPE_SPEC) 
    {
        type = semantics_type_spec(p, scope);
        ast = p;
    }
    else
    {
        type = ctype_create("", CPTR, p); /* pointer */
        for (t = p, tt = type;; t = t->chd)
        {
            if (t->chd->type == TYPE_SPEC)
            {
                tt->rec.ref = semantics_type_spec(t->chd, scope);
                ast = t;
                break;
            }
            tt->rec.ref = ctype_create("", CPTR, t);
            tt = tt->rec.ref;
        }
    }
    type->ast = ast;
    return type;
}

CVar_t semantics_params(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, PARAMS);
    p = p->chd;
    if (p->type == NOP) return NULL; /* void arguments */
    CVar_t params = semantics_p_decl(p, scope), tail = params;
#ifdef CIBIC_DEBUG
    CTable_t tparams = ctable_create(bkdr_hash, ctable_cvar_print);
#else
    CTable_t tparams = ctable_create(bkdr_hash);
#endif
    ctable_insert(tparams, params->name, params, 0);
    for (p = p->next; p; p = p->next)
    {
        CVar_t var = semantics_p_decl(p, scope);
        if (scope)  /* params inside a function definition */
            if (!ctable_insert(tparams, var->name, var, 0))
            {
                sprintf(err_buff, "redefinition of parameter '%s'", var->name);
                ERROR(var->ast);
            }
        tail->next = var;
        tail = var;
    }
    ctable_destory(tparams);
    return params;
}

#define CHECK_CVOID(name, ast) \
    if (type_spec->type == CVOID) \
do { \
    sprintf(err_buff, "variable or field '%s' declared void", name); \
    ERROR(ast); \
} while (0)

CVar_t semantics_p_declr(CNode *p, CType_t type_spec) {
    /* deal with pointer prefix */
    CNode *t, *ast;
    CType_t tt, ptype;
    const char *name;
    if (p->type == ID) 
    {
        ptype = type_spec;              /* filled by type spec */
        name = p->rec.strval;
        ast = p;
        CHECK_CVOID(name, ast);
    }
    else
    {
        ptype = ctype_create("", CPTR, p); /* pointer */
        for (t = p, tt = ptype;; t = t->chd)
        {
            if (t->chd->type == ID)
            {
                tt->rec.ref = type_spec; /* filled by type spec */
                name = t->chd->rec.strval;
                ast = t;
                break;
            }
            tt->rec.ref = ctype_create("", CPTR, t);
            tt = tt->rec.ref;
        }
    }
    return cvar_create(name, ptype, ast);
}

CVar_t semantics_declr(CNode *p, CType_t type_spec, CScope_t scope) {
    CType_t type;
    const char *name;
    CNode *ast;
    if (p->type == ID || p->rec.subtype == '*')
        return semantics_p_declr(p, type_spec);
    switch (p->rec.subtype)
    {
        case DECLR_FUNC: 
            {
                CVar_t p_declr = semantics_p_declr(p->chd, type_spec);
                type = ctype_create("", CFUNC, p); /* function declr */
                cscope_enter(scope);
                type->rec.func.params = semantics_params(p->chd->next, scope);
                cscope_exit(scope);
                /* incomplete type */
                type->rec.func.local = NULL;
                type->rec.func.ret = p_declr->type;
                type->rec.func.body = NULL;         /* not a definition */
                name = p_declr->name;
                ast = p_declr->ast;
                free(p_declr);
            }
            break;
        case DECLR_ARR:
            {
                CNode *t;
                CType_t tt;
                type = ctype_create("", CARR, p); /* array declr */
                for (t = p, tt = type;; t = t->chd)
                {
                    /* TODO: range checking */
                    tt->rec.arr.len = t->chd->next->rec.intval;     /* array length */
                    if (t->chd->type == ID || t->chd->rec.subtype == '*')
                    {
                        CVar_t p_declr = semantics_p_declr(t->chd, type_spec);
                        tt->rec.arr.elem = p_declr->type;
                        name = p_declr->name;
                        ast = p_declr->ast;
                        free(p_declr);
                        break;
                    }
                    tt->rec.arr.elem = ctype_create("", CARR, t);
                    tt = tt->rec.arr.elem;
                }
            }
            break;
        default: assert(0);
    }
    return cvar_create(name, type, ast);
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
                    semantics_type_spec(p->chd, scope),
                    scope);
            /* incomplete type checking */
            if (!type_is_complete(var->type))
            {
                sprintf(err_buff, "field '%s' has incomplete type", var->name);
                ERROR(var->ast);
            }
            if (!ctable_insert(ct, var->name, var, 0))
            {
                sprintf(err_buff, "duplicate member '%s'", var->name);
                ERROR(var->ast);
            }
        }
    }
    return ct;
}

CVar_t semantics_decl(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, DECL);
    CNode *init = p->chd->next;
    CType_t type = semantics_type_spec(p->chd, scope);
    CVar_t res = NULL;
    int useful = 0;
    if ((type->type == CSTRUCT || type->type == CUNION) && 
            (*type->name) != '\0')
    {
        cscope_push_type(scope, type);
        useful = 1;
    }
    if (init->chd->type != NOP)
    {
        CNode *p;
        for (p = init->chd; p; p = p->next)
        {
            /* TODO: initializer checking */
            CVar_t var = semantics_declr(p->chd, type, scope);
            if (scope->lvl && !type_is_complete(var->type))
            {
                sprintf(err_buff, "storage size of '%s' isn’t known", var->name);
                ERROR(var->ast);
            }
            var = var_merge(var, scope);
            var->next = res;
            res = var;
        }
        useful = 1;
    }
    if (!useful)
    {
        /* useless typename warning */
        sprintf(err_buff, "useless declaration");
        WARNING(type->ast);
    }
    return res;
}

#define NOT_IGNORE_VOID(et, ast) \
    if (et->type == CVOID) \
do \
{ \
    sprintf(err_buff, "void value not ignored as it ought to be"); \
    ERROR(ast); \
} while (0)

#define INCOMP_TYPE(ast) \
    do { \
        sprintf(err_buff, "incompatible types when assigning"); \
        ERROR(ast); \
    } while (0)

void exp_check_aseq_(CType_t lhs, CType_t rhs, CNode *ast) {
    NOT_IGNORE_VOID(lhs, ast);
    NOT_IGNORE_VOID(rhs, ast);
    switch (lhs->type)
    {
        case CSTRUCT: case CUNION:
            if (!is_same_type(lhs, rhs, 0))
                INCOMP_TYPE(ast);
            break;
        case CARR: case CFUNC: /* constant */
            INCOMP_TYPE(ast);
            break;
        case CINT: case CCHAR:
            switch (rhs->type)
            {
                case CINT: case CCHAR:
                    ; break; /* ok */
                case CPTR: case CARR:
                    sprintf(err_buff, "assignment makes integer from pointer without a cast");
                    WARNING(ast);
                    break;
                default: INCOMP_TYPE(ast);
            }
            break;
        case CPTR:
            switch (rhs->type)
            {
                case CPTR: case CARR:
                    if (!is_same_type(lhs->rec.ref, rhs->rec.ref, 1))
                    {
                        sprintf(err_buff, "assignment from incompatible pointer type");
                        WARNING(ast);
                    }
                    break;
                case CINT: case CCHAR:
                    sprintf(err_buff, "assignment makes pointer from integer without a cast");
                    WARNING(ast);
                    break;
                default: INCOMP_TYPE(ast);
            }
            break;
        default: assert(0);
    }
}

ExpType exp_check_aseq(ExpType lhs, ExpType rhs, CNode *ast) {
    exp_check_aseq_(lhs.type, rhs.type, ast);
    return lhs;
}

#define IS_INT(tt) ((tt) == CINT || (tt) == CCHAR)
#define IS_ARITH(tt) IS_INT(tt)
#define IS_SCALAR(tt) (IS_ARITH(tt) || (tt) == CPTR || (tt) == CARR)

ExpType exp_check_arith(ExpType op1, ExpType op2, CNode *ast) {
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, ast);
    NOT_IGNORE_VOID(op2.type, ast);
    res.lval = 0;
    res.type = basic_type_int;
    if (!(IS_ARITH(t1) && IS_ARITH(t2)))
    {
        sprintf(err_buff, "invalid operands to binary operator");
        ERROR(ast);
    }
    return res;
}

ExpType exp_check_bitwise(ExpType op1, ExpType op2, CNode *ast) {
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, ast);
    NOT_IGNORE_VOID(op2.type, ast);
    res.lval = 0;
    res.type = basic_type_int;
    if (!(IS_INT(t1) && IS_INT(t2)))
    {
        sprintf(err_buff, "invalid operands to binary operator");
        ERROR(ast);
    }
    return res;
}

ExpType exp_check_add(ExpType op1, ExpType op2, CNode *ast, int sub) {
    int t1 = op1.type->type,
        t2 = op2.type->type;
    NOT_IGNORE_VOID(op1.type, ast);
    NOT_IGNORE_VOID(op2.type, ast);
    if (!sub && t2 == CPTR) 
    {
        /* place the pointer type in the first place */
        int t = t1;
        ExpType te = op1;

        t1 = t2;
        t2 = t;

        op1 = op2;
        op2 = te;

        CNode *n1 = ast->chd;
        CNode *n2 = n1->next;
        n2->next = n1;
        n1->next = NULL;
        ast->chd = n2;
    }
    if (!((t1 == CINT || t1 == CCHAR || t1 == CPTR) &&
                (t2 == CINT || t2 == CCHAR)))
    {
        sprintf(err_buff, "invalid operands to binary operator");
        ERROR(ast);
    }
    return op1; /* int or pointer */
}

ExpType exp_check_int(ExpType op1, CNode *ast) {
    if (!IS_INT(op1.type->type))
    {
        sprintf(err_buff, "wrong type argument to unary operator");
        ERROR(ast);
    }
    op1.lval = 0;
    return op1;
}

ExpType exp_check_scalar(ExpType op1, CNode *ast) {
    if (!IS_SCALAR(op1.type->type))
    {
        sprintf(err_buff, "wrong type argument to unary operator");
        ERROR(ast);
    }
    op1.lval = 0;
    return op1;
}

ExpType exp_check_deref(ExpType op1, CNode *ast) {
    if (op1.type->type != CPTR)
    {
        sprintf(err_buff, "invalid type argument of unary '*'");
        ERROR(ast);
    }
    op1.lval = 1;   /* deref changes exp to lval */
    if (!type_is_complete(op1.type = op1.type->rec.ref))
    {
        sprintf(err_buff, "dereferencing pointer to incomplete type");
        ERROR(ast);
    }
    return op1;
}

ExpType exp_check_ref(ExpType op1, CNode *ast) {
    ExpType res;
    if (!op1.lval)
    {
        sprintf(err_buff, "lvalue required as unary '&' operand");
        ERROR(ast);
    }
    res.lval = 0;
    res.type = ctype_create("", CPTR, ast);
    res.type->rec.ref = op1.type;
    return res;
}

ExpType exp_check_sizeof(ExpType op1) {
    op1.lval = 0;
    op1.type = basic_type_int;
    return op1;
}

ExpType exp_check_inc(ExpType op1, CNode *ast) {
    if (!IS_SCALAR(op1.type->type))
    {
        sprintf(err_buff, "wrong type argument to increment/decrement");
        ERROR(ast);
    }
    if (!op1.lval)
    {
        sprintf(err_buff, "lvalue required as increment/decrement operand");
        ERROR(ast);
    }
    return op1;
}

ExpType semantics_exp(CNode *, CScope_t);

ExpType exp_check_logical(ExpType op1, ExpType op2, CNode *ast) {
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, ast);
    NOT_IGNORE_VOID(op2.type, ast);
    res.lval = 0;
    res.type = basic_type_int;
    if (!(IS_SCALAR(t1) && IS_SCALAR(t2)))
    {
        sprintf(err_buff, "invalid operands to binary operator");
        ERROR(ast);
    }
    return res;
}

ExpType exp_check_ass(ExpType lhs, ExpType rhs, CNode *p) {
    NOT_IGNORE_VOID(lhs.type, p);
    NOT_IGNORE_VOID(rhs.type, p);
    if (!lhs.lval)
    {
        sprintf(err_buff, "lvalue required as left operand of assignment");
        ERROR(p);
    }
    switch (p->rec.subtype)
    {
        case '=' : return exp_check_aseq(lhs, rhs, p);
        case ASS_MUL: return exp_check_aseq(lhs, exp_check_arith(lhs, rhs, p), p);
        case ASS_DIV: return exp_check_aseq(lhs, exp_check_arith(lhs, rhs, p), p); 
        case ASS_MOD: return exp_check_aseq(lhs, exp_check_arith(lhs, rhs, p), p); 

        case ASS_ADD: return exp_check_aseq(lhs, exp_check_add(lhs, rhs, p, 0), p);
        case ASS_SUB: return exp_check_aseq(lhs, exp_check_add(lhs, rhs, p, 1), p);

        case ASS_SHL: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p), p);
        case ASS_SHR: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p), p);
        case ASS_AND: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p), p);
        case ASS_XOR: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p), p);
        case ASS_OR: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p), p);
        default: assert(0);
    }
}

ExpType exp_check_equality(ExpType op1, ExpType op2, CNode *ast) {
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, ast);
    NOT_IGNORE_VOID(op2.type, ast);
    res.lval = 0;
    res.type = basic_type_int;
    if (IS_ARITH(t1) && IS_ARITH(t2))
        return res;
    if (!(IS_SCALAR(t1) && IS_SCALAR(t2)))
    {
        sprintf(err_buff, "invalid operands to binary operator");
        ERROR(ast);
    }
    if (t1 == CPTR && t2 == CPTR)
    {
        if (!is_same_type(op1.type->rec.ref, op2.type->rec.ref, 0))
        {
            sprintf(err_buff, "comparison of distinct pointer types lacks a cast");
            WARNING(ast);
        }
    }
    else if (t1 == CPTR || t2 == CPTR)
    {
        sprintf(err_buff, "comparison between pointer and integer");
        WARNING(ast);
    }
    return res;
}

ExpType exp_check_postfix(CNode *p, CScope_t scope) {
    CNode *post = p->chd->next;
    ExpType op1 = semantics_exp(p->chd, scope), op2;
    int t1 = op1.type->type, t2;
    switch (post->rec.subtype)
    {
        case POSTFIX_ARR:
            if (!(t1 == CARR || t1 == CPTR))
            {
                sprintf(err_buff, "subscripted value is neither array nor pointer");
                ERROR(p);
            }
            op2 = semantics_exp(post, scope);
            t2 = op2.type->type;
            if (!IS_INT(t2))
            {
                sprintf(err_buff, "array subscript is not an integer");
                ERROR(p);
            }
            op1.type = op1.type->rec.arr.elem;
            op1.lval = 1;
            break;
        case POSTFIX_CALL:
            if (t1 != CFUNC)
            {
                sprintf(err_buff, "called object is not a function");
                ERROR(p);
            }
            {
                CNode *arg = post->chd->chd;
                CType_t func = p->chd->ext.type;
                CVar_t param = func->rec.func.params;
                for (; arg && param;
                        arg = arg->next, param = param->next) 
                {
                    semantics_exp(arg, scope);
                    exp_check_aseq_(param->type, arg->ext.type, arg);
                }
                if (arg || param)
                {
                    sprintf(err_buff, "too many/few arguments to the function");
                    ERROR(p);
                }
                op1.type = func->rec.func.ret;
                op1.lval = 0;
                break;
            }
        case POSTFIX_DOT:
            if (!(t1 == CSTRUCT || t1 == CUNION))
            {
                sprintf(err_buff, "request for the member in something not a structure or union");
                ERROR(p);
            }
            {
                CVar_t fv = ctable_lookup(op1.type->rec.fields, post->chd->rec.strval);
                if (!fv)
                {
                    sprintf(err_buff, "struct/union has no member named '%s'", post->chd->rec.strval);
                    ERROR(p);
                }
                op1.type = fv->type;
                op1.lval = 1;
            }
            break;
        case POSTFIX_PTR:
            if (t1 != CPTR)
            {
                sprintf(err_buff, "invalid type argument of '->'");
                ERROR(p);
            }
            {
                CType_t tref = op1.type->rec.ref;
                if (!(tref->type == CSTRUCT || tref->type == CUNION))
                {
                    sprintf(err_buff, "request for the member in something not a structure or union");
                    ERROR(p);
                }
                if (!tref->rec.fields)
                {
                    sprintf(err_buff, "dereferencing pointer to incomplete type");
                    ERROR(p);
                }
                CVar_t fv = ctable_lookup(tref->rec.fields, post->chd->rec.strval);
                if (!fv)
                {
                    sprintf(err_buff, "struct/union has no member named '%s'", post->chd->rec.strval);
                    ERROR(p);
                }
                op1.type = fv->type;
                op1.lval = 1;
            }
            break;
        case OPT_INC: case OPT_DEC:
            exp_check_inc(op1, p);
            break;
        default: assert(0);
    }
    return op1;
}

ExpType semantics_exp(CNode *p, CScope_t scope) {
    ExpType res;
    switch (p->type)
    {
        case ID:
            if (!(p->ext.var = cscope_lookup_var(scope, p->rec.strval)))
            {
                sprintf(err_buff, "'%s' undeclared", p->rec.strval);
                ERROR(p);
            }
            res.type = p->ext.var->type;
            res.lval = !(res.type->type == CARR || res.type->type == CFUNC);
            break;
        case INT:
            res.type = basic_type_int;
            res.lval = 0;
            break;
        case CHAR:
            res.type = basic_type_char;
            res.lval = 0;
            break;
        case STR:
            {
                CType_t type = ctype_create("", CPTR, NULL);
                type->rec.ref = basic_type_char;
                res.type = type;
            }
        case EXP:
            {
                ExpType op1;
                ExpType op2;
                if (!(p->rec.subtype == EXP_CAST || p->rec.subtype == EXP_POSTFIX))
                {
                    op1 = semantics_exp(p->chd, scope);
                    if (p->chd->next)
                        op2 = semantics_exp(p->chd->next, scope);
                }
                switch (p->rec.subtype)
                {
                    /* following cases are binary expressions */
                    case ',': 
                        res = op2;
                        res.lval = 0;
                        break;
                    case '=' : 
                    case ASS_MUL:
                    case ASS_DIV:
                    case ASS_MOD:
                    case ASS_ADD:
                    case ASS_SUB:
                    case ASS_SHL:
                    case ASS_SHR:
                    case ASS_AND:
                    case ASS_XOR:
                    case ASS_OR:
                        res = exp_check_ass(op1, op2, p);
                        break;
                    case OPT_OR:
                    case OPT_AND:
                        res = exp_check_logical(op1, op2, p);
                        break;
                    case OPT_SHL:
                    case OPT_SHR:
                    case '|':
                    case '^':
                        res = exp_check_bitwise(op1, op2, p);
                        break;
                    case OPT_EQ:
                    case OPT_NE:
                    case '<':
                    case '>' :
                    case OPT_LE:
                    case OPT_GE:
                        res = exp_check_equality(op1, op2, p);
                        break;
                    case '/': case '%':
                        res = exp_check_arith(op1, op2, p);
                        break;
                    case EXP_CAST:
                        res.type = semantics_type_name(p->chd, scope);
                        res.lval = 0;
                        break;
                    case '&':
                        if (p->chd->next)
                            res = exp_check_bitwise(op1, op2, p);
                        else
                            res = exp_check_ref(op1, p);
                        break;
                    case '*': 
                        if (p->chd->next)
                            res = exp_check_arith(op1, op2, p);
                        else
                            res = exp_check_deref(op1, p);
                        break;
                    case '+':
                        if (p->chd->next)
                            res = exp_check_add(op1, op2, p, 0);
                        else
                        {
                            res = op1;
                            res.lval = 0;
                        }
                        break;
                    case '-':
                        if (p->chd->next)
                            res = exp_check_add(op1, op2, p, 1);
                        else
                        {
                            res = op1;
                            res.lval = 0;
                        }
                        break;
                    case '~':
                        res = exp_check_int(op1, p);
                        break;
                    case '!':
                        res = exp_check_scalar(op1, p);
                        break;
                    case OPT_INC: case OPT_DEC:
                        res = exp_check_inc(op1, p);
                        break;
                    case KW_SIZEOF:
                        res = exp_check_sizeof(op1);
                        break;
                    case EXP_POSTFIX:
                        res = exp_check_postfix(p, scope);
                        break;
                    default: assert(0);
                }
            }
            break;
        case NOP: ; break;
        default: assert(0);
    }
    p->ext.type = res.type;
    return res;
}

CVar_t semantics_stmt(CNode *p, CScope_t scope);

CVar_t semantics_if(CNode *p, CScope_t scope) {
    ExpType exp = semantics_exp(p->chd, scope);
    CNode *body1 = p->chd->next,
          *body2 = body1->next;
    CVar_t res;
    if (!IS_SCALAR(exp.type->type))
    {
        sprintf(err_buff, "a scalar is required in 'if' condition");
        ERROR(p->chd);
    }
    cscope_enter(scope);
    res = semantics_stmt(body1, scope);
    cscope_exit(scope);
    if (body2->type != NOP)
    {
        cscope_enter(scope);
        res->next = semantics_stmt(p->chd->next->next, scope);
        cscope_exit(scope);
    }
    return res;
}

CVar_t semantics_for(CNode *p, CScope_t scope) {
    ExpType exp = semantics_exp(p->chd->next, scope);
    semantics_exp(p->chd, scope);
    semantics_exp(p->chd->next->next, scope);
    CVar_t res;
    if (p->chd->next->type != NOP && !IS_SCALAR(exp.type->type))
    {
        sprintf(err_buff, "a scalar is required in 'for' condition");
        ERROR(p->chd->next);
    }
    cscope_enter(scope);
    scope->inside_loop++;
    res = semantics_stmt(p->chd->next->next->next, scope);
    scope->inside_loop--;
    cscope_exit(scope);
    return res;
}

CVar_t semantics_while(CNode *p, CScope_t scope) {
    ExpType exp = semantics_exp(p->chd, scope);
    CVar_t res;
    if (!IS_SCALAR(exp.type->type))
    {
        sprintf(err_buff, "a scalar is required in 'while' condition");
        ERROR(p->chd);
    }
    cscope_enter(scope);
    scope->inside_loop++;
    res = semantics_stmt(p->chd->next, scope);
    scope->inside_loop--;
    cscope_exit(scope);
    return res;
}

CVar_t semantics_check_loop(CNode *p, CScope_t scope, const char *stmt_name) {
    if (!scope->inside_loop)
    {
        sprintf(err_buff, "%s statement not within a loop", stmt_name);
        ERROR(p);
    }
    return NULL;
}
CVar_t semantics_return(CNode *p, CScope_t scope) {
    assert(scope->func);
    CType_t rt = scope->func->rec.func.ret;
    if (p->chd->type != NOP)
    {
        if (rt->type == CVOID)
        {
            sprintf(err_buff, "'return' with a value, in function returning void");
            WARNING(p->chd);
        }
        semantics_exp(p->chd, scope);
        exp_check_aseq_(rt, p->chd->ext.type, p->chd);
    }
    return NULL;
}

CVar_t semantics_comp(CNode *, CScope_t);
CVar_t semantics_stmt(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, STMT);
    switch (p->rec.subtype)
    {
        case STMT_EXP: 
            semantics_exp(p->chd, scope);
            break;
        case STMT_COMP:
            {
                CVar_t res;
                cscope_enter(scope);
                res = semantics_comp(p, scope);
                cscope_exit(scope);
                return res;
            }
        case STMT_IF:
            return semantics_if(p, scope);
        case STMT_FOR:
            return semantics_for(p, scope);
        case STMT_WHILE:
            return semantics_while(p, scope);
        case STMT_CONT:
            return semantics_check_loop(p, scope, "continue");
        case STMT_BREAK:
            return semantics_check_loop(p, scope, "break");
        case STMT_RET:
            return semantics_return(p, scope);
        default: assert(0);
    }
    return NULL;
}

CVar_t semantics_comp(CNode *p, CScope_t scope) {
    CNode *decls = p->chd,
          *stmts = p->chd->next, *i;
    CVar_t res = NULL;
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
    return res;
}

CVar_t semantics_func(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, FUNC_DEF);
    CVar_t head = semantics_p_declr(p->chd->next, semantics_type_spec(p->chd, scope));
    CType_t func = ctype_create(head->name, CFUNC, p), funco;
    CVar_t res = cvar_create(head->name, func, p), old = NULL;
    CNode *chd = p->chd->next->next;

    func->rec.func.ret = head->type;
    /* check return type */
    if (!type_is_complete(head->type))
    {
        sprintf(err_buff, "return type is an incomplete type");
        ERROR(p->chd);
    }

    scope->func = func;
    func->rec.func.params = semantics_params(chd, scope);      /* check params */
    if (cscope_push_var(scope, res))
        old = res;
    cscope_enter(scope);                                       /* enter into function local scope */
    {
        CVar_t p;
        for (p = func->rec.func.params; p; p = p->next)
            cscope_push_var(scope, p);
    }
    func->rec.func.local = semantics_comp(chd->next, scope);   /* check comp */
    func->rec.func.body = chd->next;
    cscope_exit(scope);                                        /* exit from local scope */

    if (!old)
    {
        old = cscope_lookup_var(scope, res->name);
        funco = old->type;
        if (funco->type != CFUNC)
        {
            sprintf(err_buff, "conflicting types of '%s'", res->name);
            ERROR(res->ast);
        }
        else if (funco->rec.func.body)
        {
            sprintf(err_buff, "redefintion of function '%s'", res->name);
            ERROR(res->ast);
        }
        else if (!is_same_type(funco, res->type, 0))
        {
            sprintf(err_buff, "function defintion does not match the prototype");
            ERROR(res->ast);
        }
        funco->rec.func.local = res->type->rec.func.local;
        funco->rec.func.body = res->type->rec.func.body;
        free(res);
    }
    return old;
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
    basic_type_int = ctype_create("int", CINT, NULL);
    basic_type_char = ctype_create("char", CCHAR, NULL);
    basic_type_void = ctype_create("void", CVOID, NULL);
    /* add top-level basic types */
    cscope_push_type(scope, basic_type_int);
    cscope_push_type(scope, basic_type_char);
    cscope_push_type(scope, basic_type_void);
    /* check all definitions and declarations */
    for (ast = ast->chd; ast; ast = ast->next)
    {
        switch (ast->type)
        {
            case FUNC_DEF: 
                semantics_func(ast, scope); break;
            case DECL: 
                semantics_decl(ast, scope); break;
            default: assert(0);
        }
    }
    cscope_debug_print(scope);
    {
        CTNode *p;
        int i;
        for (i = 0; i < MAX_TABLE_SIZE; i++)
            for (p = scope->tvar->head[i]; p; p = p->next)
            {
                cvar_print((CVar_t)p->val);
                fprintf(stderr, "\n");
            }
        for (i = 0; i < MAX_TABLE_SIZE; i++)
            for (p = scope->ttype->head[i]; p; p = p->next)
            {
                ctype_print((CType_t)p->val);
                fprintf(stderr, "\n");
            }
    }
}

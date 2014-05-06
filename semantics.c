#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "semantics.h"
#include "ast.h"
#define NEW(type) ((type *)malloc(sizeof(type)))
#define CHECK_TYPE(p, _type) assert(p->type == _type)
#define ERROR(x) do { error_print x; } while (0)
#define WARNING(x) do { warning_print x; } while (0)

#define NOT_IGNORE_VOID(et, ast) \
    if (et->type == CVOID) \
        do { \
            ERROR((ast, "void value not ignored as it ought to be")); \
        } while (0)

#define INCOMP_TYPE(ast) \
    do { \
        ERROR((ast, "incompatible types when assigning")); \
    } while (0)

/* pointer to function conversion (std 6.3.2/4) */
/* also convert array to pointer */
#define POINTER_CONV(t, p) \
    do { \
        if ((t)->type == CFUNC) \
        { \
            CType_t f = ctype_create("", CPTR, p); \
            f->rec.ref = t; \
            t = f; \
        } \
        else if ((t)->type == CARR) \
        { \
            CType_t a = ctype_create("", CPTR, p); \
            a->rec.ref = t->rec.arr.elem; \
            free(t); \
            t = a; \
        } \
    } while (0)

#define CHECK_CVOID(name, ast) \
    if (typespec->type == CVOID) \
        do { \
            ERROR((ast, "variable or field '%s' declared void", name)); \
        } while (0)

#define IS_INT(tt) ((tt) == CINT || (tt) == CCHAR)
#define IS_PTR(tt) ((tt) == CPTR || (tt) == CARR)
#define IS_SCALAR(tt) (!((tt) == CUNION || (tt) == CSTRUCT))
#define IS_ARITH(tt) IS_INT(tt)

extern void print_error(char *, char *, int, int, int);
extern char *load_line(int);
static char err_buff[MAX_ERROR_BUFF];
static CType_t basic_type_int; 
static CType_t basic_type_char;
static CType_t basic_type_void;
static CType_t builtin_printf;
static CType_t builtin_scanf;
static CType_t builtin_malloc;
static CType_t builtin_memcpy;
static CType_t builtin_print_int;
static CType_t builtin_print_char;
static CType_t builtin_print_string;

CTList_t funcs;
CVList_t gvars;
CSList_t cstrs;

static void error_print(CNode *ast, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsprintf(err_buff, fmt, args);
    print_error(err_buff, NULL, ast->loc.row, ast->loc.col, 0);
    va_end(args);
}

static void warning_print(CNode *ast, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vsprintf(err_buff, fmt, args);
    print_error(err_buff, NULL, ast->loc.row, ast->loc.col, 1);
    va_end(args);
}

const char *csymbol_getname(CSymbol_t sym) {
    switch (sym->kind)
    {
        case CVAR: return sym->rec.var->name;
        case CTYPE: return sym->rec.type->name;
        case CDEF: return sym->rec.def->name;
    }
    return NULL;
}

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
    CTNode *p = ct->head[hv], *np;
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

CScope_t cscope_create(void) {
    CScope_t p = NEW(CScope);
    p->lvl = -1;
    p->top = NULL;
    p->func = NULL;
    p->inside_loop = 0;
    p->ids = ctable_create(bkdr_hash, csymbol_print);
    p->tags = ctable_create(bkdr_hash, csymbol_print);
    cscope_enter(p);
    return p;
}

static int cscope_push(CScope_t cs, CSymbol_t sym, int nspace) {
    CTable_t ct = nspace == NS_ID ? cs->ids : cs->tags;
#ifdef CIBIC_DEBUG
    assert(cs->top);
#endif
    if (ctable_insert(ct, csymbol_getname(sym), sym, cs->lvl))
    {
        CSElem *e = NEW(CSElem);
        e->sym = sym;
        e->next = cs->top->symlist;
        cs->top->symlist = e;
        return 1;
    }
    else return 0; /* naming conflict */
}

int cscope_push_var(CScope_t cs, CVar_t var, int nspace) {
    CSymbol_t p = NEW(CSymbol);
    p->kind = CVAR;
    p->rec.var = var;
    if (!cscope_push(cs, p, nspace))
    {
        free(p);
        return 0;
    }
    return 1;
}

int cscope_push_type(CScope_t cs, CType_t type, int nspace) {
    CSymbol_t p = NEW(CSymbol);
    p->kind = CTYPE;
    p->rec.type = type;
    if (!cscope_push(cs, p, nspace))
    {
        free(p);
        return 0;
    }
    return 1;
}

int cscope_push_def(CScope_t cs, CDef_t def, int nspace) {
    CSymbol_t p = NEW(CSymbol);
    p->kind = CDEF;
    p->rec.def = def;
    if (!cscope_push(cs, p, nspace))
    {
        free(p);
        return 0;
    }
    return 1;
}

void cscope_enter(CScope_t cs) {
    CSNode *np = NEW(CSNode);
    np->next = cs->top;
    np->symlist = NULL;
    cs->top = np;
    cs->lvl++;
}

void cscope_exit(CScope_t cs) {
    CSNode *otop = cs->top;
    CSElem *p, *np;
    cs->lvl--;
    cs->top = otop->next;
    for (p = otop->symlist; p; p = np)
    {
        const char *name = csymbol_getname(p->sym);
        ctable_clip(cs->ids, name, cs->lvl);
        ctable_clip(cs->tags, name, cs->lvl);
        np = p->next;   
        free(p->sym);   /* free CSymbol */
        free(p);        /* free CSElem */
    }
    free(otop);
}

CSymbol_t cscope_lookup(CScope_t cs, const char *name, int nspace) {
    if (nspace == NS_ID)
        return ctable_lookup(cs->ids, name);
    else
        return ctable_lookup(cs->tags, name);
    return NULL;
}

unsigned int bkdr_hash(const char *str) {
    unsigned int seed = 131;
    unsigned int hv = 0;
    while (*str)
        hv = hv * seed + (unsigned)(*str++);
    return hv;
}

CVar_t cvar_create(char *name, CType_t type, CNode *ast) {
    CVar_t cv = NEW(CVar);
    cv->name = name;
    cv->type = type;
    cv->ast = ast;
    cv->initr = NULL;
    cv->defsite = NULL;
    cv->loc = 0;
    cv->weight = 0;
    cv->reload = 0;
    return cv;
}

CType_t ctype_create(char *name, int type, CNode *ast) {
    CType_t ct = NEW(CType);
    ct->name = name;
    ct->type = type;
    ct->ast = ast;
    ct->size = -1;
    return ct;
}

int align_shift(int x) {
    return ((4 - (x & 3)) & 3);
}

int calc_size(CType_t type) {
    int size = type->size;
    if (size != -1) return size;
    /* TODO: correct alignment */
    switch (type->type)
    {
        case CINT: size = INT_SIZE; break;
        case CCHAR: size = CHAR_SIZE; break;
        case CPTR: size = PTR_SIZE; break;
        case CARR: 
            size = type->rec.arr.len * calc_size(type->rec.arr.elem);
            break;
        case CSTRUCT:
            {
                size = 0;
                CVar_t p = type->rec.st.flist;
                if (!p) return -1;
                for (; p; p = p->next)
                {
                    /* add padding to align to word boundary */
                    if (p->type->type != CCHAR)
                        size += align_shift(size);
                    p->start = size;
                    size += calc_size(p->type);
                }
            }
            break;
        case CUNION:
            {
                size = 0;
                CVar_t p = type->rec.st.flist;
                if (!p) return -1;
                for (; p; p = p->next)
                {
                    int t = calc_size(p->type);
                    if (t > size) size = t;
                    p->start = 0;
                }
            }
            break;
        case CVOID: return -1;
        case CFUNC: return 1;
    }
    return (type->size = size);
}

static CType_t struct_type_merge(CType_t new, CScope_t scope) {
    /* Note: we shall try to lookup first instead of pushing !! */
    CSymbol_t lu = cscope_lookup(scope, new->name, NS_TAG);
    CType_t old;
    if (!lu) /* create it if it does not exist */
    {
        cscope_push_type(scope, new, NS_TAG);
        return new;
    } /* otherwise we have it */
    old = lu->rec.type;
    /* it must be a struct or union */
    if (!new->rec.st.fields) /* use the old definition */
        return old;
    /* otherwise it's a complete definition */
    if (cscope_push_type(scope, new, NS_TAG))
        return new;      /* try to push the defintion */
    /* conflict appears */
    if (old->rec.st.fields) /* if the old one is complete */
        ERROR((new->ast, "redefinition of '%s'", new->name));
    /* otherwise incomplete, thus complete the type */
    old->rec.st = new->rec.st;
    old->ast = new->ast;
    free(new);
    return old;
}

static void type_merge(CType_t old, CType_t new) {
    /* assume old and new are the same type */
    assert(old->type == new->type);
    switch (old->type)
    {
        case CINT: case CCHAR: case CPTR:
        case CUNION: case CSTRUCT:
            break;
        case CFUNC:
            if (new->rec.func.params)
                old->rec.func.params = new->rec.func.params;
            if (new->rec.func.body)
            {
                old->rec.func.local = new->rec.func.local;
                old->rec.func.body = new->rec.func.body;
            }
            break;
        default: assert(0);
    }
}

int is_same_type(CType_t typea, CType_t typeb) {
    if (typea == typeb) return 1;
    if (typea->type != typeb->type) return 0;
    switch (typea->type)
    {
        case CSTRUCT: case CUNION:
            return typea == typeb;
        case CARR:
            if (typea->rec.arr.len != typeb->rec.arr.len)
                return 0;
            return is_same_type(typea->rec.arr.elem, typeb->rec.arr.elem);
        case CPTR:
            return is_same_type(typea->rec.ref, typeb->rec.ref);
        case CFUNC:
            {
                CVar_t pa = typea->rec.func.params,
                       pb = typeb->rec.func.params;
                if ((pa || typea->rec.func.body) &&
                    (pb || typeb->rec.func.body))
                {
                    for (;pa && pb; pa = pa->next, pb = pb->next)
                        if (!is_same_type(pa->type, pb->type))
                            return 0;
                    if (pa || pb) 
                        return 0; /* different number of parameters */
                }
                return is_same_type(typea->rec.func.ret, typeb->rec.func.ret);
            }
        case CINT: case CCHAR: case CVOID: break;
    }
    return 1;
}

static CVar_t var_merge(CVar_t new, CScope_t scope) {
    CVar_t old;
    if (cscope_push_var(scope, new, NS_ID))
        return new;
    else
        old = cscope_lookup(scope, new->name, NS_ID)->rec.var;
    if (!is_same_type(old->type, new->type))
        ERROR((new->ast, "conflicting types of '%s'", new->name));
    else if (scope->lvl)
        ERROR((new->ast, "redeclaration of '%s' with no linkage", new->name));
    type_merge(old->type, new->type);
    free(new);
    return old;
}

void semantics_fields(CNode *, CType_t, CScope_t scope);
CType_t semantics_typespec(CNode *p, CScope_t scope) {
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
                    type->rec.st.fields = NULL; /* incomplete type */
                else
                    semantics_fields(fields, type, scope);
                if (id->type != NOP)
                    type = struct_type_merge(type, scope);
            }
            break;
        case USER_TYPE:
            {
                CHECK_TYPE(p->chd, ID);
                CSymbol_t lu = cscope_lookup(scope, p->chd->rec.strval, NS_ID);
                assert(lu && lu->kind == CDEF); /* parser guarantees this */
                type = lu->rec.def->type;
            }
            break;
        default: assert(0);
    }
    return type;
}

int type_is_complete(CType_t type) {
    switch(type->type)
    {
        case CINT: case CCHAR:
            /* basic types are always complete */
        case CPTR:
            /* pointer may point to an incomplete type */
        case CARR:
            /* syntax of incomplete arrays is not allowed in `cibic.y` */
            return 1;
        case CSTRUCT: case CUNION:
            /* fields are guaranteed to be complete if exists, due to
             * `semantics_fields` */
            return type->rec.st.fields != NULL;
        case CVOID:
            /* void type is never complete */
            return 0;
        case CFUNC:
            /* function body is not required here, it is checked in the last
             * phase */
            return 1;
        default: assert(0);
    }
    return 1;
}

CVar_t semantics_declr(CNode *, CType_t, CScope_t, int);
CVar_t semantics_pdecl(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, PLAIN_DECL);
    CVar_t var = semantics_declr(p->chd->next,
                                semantics_typespec(p->chd, scope),
                                scope, 0);
    return var;
}

CVar_t semantics_params(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, PARAMS);
    static CVar dummy;
    CVar_t params = &dummy, tail = params;
    CTable_t ct;
    if (!(p = p->chd)) return NULL; /* no parameters */
    ct = ctable_create(bkdr_hash, ctable_cvar_print);
    for (; p; p = p->next)
    {
        CVar_t var = semantics_pdecl(p, scope);
        POINTER_CONV(var->type, p);
        if (scope)  /* params inside a function definition */
            if (!ctable_insert(ct, var->name, var, 0))
                ERROR((var->ast, "redefinition of parameter '%s'", var->name));
        tail->next = var;
        tail = var;
    }
    ctable_destory(ct);
    tail->next = NULL;
    return params->next;
}

ExpType semantics_exp(CNode *, CScope_t);
CVar_t semantics_declr(CNode *p, CType_t typespec, CScope_t scope, int flag) {
    CVar_t type;
    if (p->type == ID) 
    {
        if (!(flag & FLAG_FUNC_CHK)) CHECK_CVOID(p->rec.strval, p);
        return cvar_create(p->rec.strval, typespec, p);
    }
    if (p->type == NOP) /* type name */
        return cvar_create(NULL, typespec, p);
    switch (p->rec.subtype)
    {
        case DECLR_FUNC: 
            {
                CType_t func = ctype_create("", CFUNC, p); /* function declr */
                if (flag & FLAG_FUNC_DEF)                  /* function def */
                    func->rec.func.params = semantics_params(p->chd->next, scope);
                else /* function declaration */
                {
                    cscope_enter(scope);
                    func->rec.func.params = semantics_params(p->chd->next, scope);
                    cscope_exit(scope);
                    /* incomplete type */
                    func->rec.func.local = NULL;
                    func->rec.func.body = NULL;    /* not a definition */
                }
                func->rec.func.ret = typespec; /* might be an incomplete type */
                type = semantics_declr(p->chd, func, scope, flag | FLAG_FUNC_CHK);
                if (typespec->type == CARR)
                    ERROR((p, "'%s' declared as function returning an array",
                                type->name));
                if (typespec->type == CFUNC)
                    ERROR((p, "'%s' declared as function returing a function",
                                type->name));
            }
            break;
        case DECLR_ARR:
            {
                CType_t arr = ctype_create("", CARR, p);    /* array declr */
                CNode *rch = p->chd->next;
                ExpType tl = semantics_exp(rch, scope);
                if (calc_size(typespec) == -1)
                    ERROR((p, "array type has incomplete element type"));
                if (!rch->ext.is_const)
                    ERROR((p, "size of array must be a constant"));
                if (!IS_INT(tl.type->type))
                    ERROR((p, "size of array has non-integer type"));
                arr->rec.arr.elem = typespec;
                arr->rec.arr.len = rch->ext.const_val;
                type = semantics_declr(p->chd, arr, scope, 0);
            }
            break;
        case '*':
            {
                CType_t ptr = ctype_create("", CPTR, p);    /* pointer */
                ptr->rec.ref = typespec;
                type = semantics_declr(p->chd, ptr, scope, 0);
            }
            break;
        default: assert(0);
    }
    return type;
}

void semantics_fields(CNode *p, CType_t type, CScope_t scope) {
    CTable_t ct = ctable_create(bkdr_hash, ctable_cvar_print);
    type->rec.st.fields = ct;
    type->rec.st.flist = NULL;
    for (p = p->chd; p; p = p->next)
    {
        CNode *declr = p->chd->next->chd;
        for (; declr; declr = declr->next)
        {
            CVar_t var = semantics_declr(declr, 
                                        semantics_typespec(p->chd, scope),
                                        scope, 0);
            if (var->type->type == CFUNC)
                ERROR((var->ast, "field '%s' declared as a function", var->name));
            /* types of fields are supposed to be complete */
            if (calc_size(var->type) == -1)
                ERROR((var->ast, "field '%s' has incomplete type", var->name));
            if (!ctable_insert(ct, var->name, var, 0))
                ERROR((p, "duplicate member '%s'", var->name));
            var->next = type->rec.st.flist;
            type->rec.st.flist = var;
        }
    }
}

static void exp_check_aseq_(CType_t lhs, CType_t rhs, CNode *ast) {
    NOT_IGNORE_VOID(lhs, ast);
    NOT_IGNORE_VOID(rhs, ast);
    switch (lhs->type)
    {
        case CSTRUCT: case CUNION:
            if (!is_same_type(lhs, rhs))
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
                    WARNING((ast, "assignment makes integer from pointer without a cast"));
                    break;
                default: INCOMP_TYPE(ast);
            }
            break;
        case CPTR:
            switch (rhs->type)
            {
                case CPTR: case CARR:
                    if (!is_same_type(lhs->rec.ref, rhs->rec.ref))
                        WARNING((ast, "assignment from incompatible pointer type"));
                    break;
                case CINT: case CCHAR:
                    WARNING((ast, "assignment makes pointer from integer without a cast"));
                    break;
                default: INCOMP_TYPE(ast);
            }
            break;
        default: assert(0);
    }
}

void semantics_initr(CNode *p, CScope_t scope, CType_t type) {
    switch (p->rec.subtype)
    {
        case INITR_NORM: 
            {
                ExpType et = semantics_exp(p->chd, scope);
                if (!scope->lvl && !p->chd->ext.is_const)  /* in global scope */
                    ERROR((p->chd, "initializer element is not constant"));
                exp_check_aseq_(type, et.type, p);
            }
            break;
        case INITR_ARR:
        {
            if (type->type == CARR)
                type = type->rec.arr.elem;
            else
                ERROR((p, "invalid initializer"));
            for (p = p->chd; p; p = p->next)
                semantics_initr(p, scope, type);
        }
        break;
    }
}

void semantics_typedef(CNode *p, CType_t type, CScope_t scope) {
    CNode *declr = p->chd->next;
    for (p = declr->chd; p; p = p->next)
    {
        CVar_t var = semantics_declr(p, type, scope, 0);
        CDef_t def = cdef_create(var->name, var->type, var->ast);
        if (!cscope_push_def(scope, def, NS_ID))
        {
            CSymbol_t lu = cscope_lookup(scope, def->name, NS_ID);
            if (lu->kind != CDEF)
                ERROR((def->ast, "'%s' redeclared as different kind of symbol", def->name));
            /* FIXME: `typedef int a()` is different from typedef `int a(int)` */
            if (!is_same_type(lu->rec.def->type, def->type))
                ERROR((def->ast, "conflicting types of '%s'", def->name));
        }
    }
}

CVar_t semantics_decl(CNode *p, CScope_t scope) {
    CNode *declr = p->chd->next;
    CType_t type = semantics_typespec(p->chd, scope);
    CVar_t res = NULL;
    int useful = 0;
    if ((type->type == CSTRUCT || type->type == CUNION) && 
        (*type->name) != '\0')
    {
        cscope_push_type(scope, type, NS_TAG);
        useful = 1;
    }
    if (p->type == TYPEDEF)
    {
        semantics_typedef(p, type, scope);
        return NULL;
    }
    CHECK_TYPE(p, DECL);
    if (declr->chd->type != NOP)
    {
        CNode *p;
        for (p = declr->chd; p; p = p->next)
        {
            CNode *initr = p->chd->next;
            CVar_t var = semantics_declr(p->chd, type, scope, 0);
            if (var->type->type == CFUNC)
            {
                CType_t func = var->type;
                CSymbol_t lu;
                func->name = var->name;
                if (initr->type == INITR)
                    ERROR((var->ast, "function '%s' is initialized like a variable", func->name));
                if (!cscope_push_type(scope, func, NS_ID))
                {
                    lu = cscope_lookup(scope, func->name, NS_ID);
                    if (lu->kind != CTYPE)
                        ERROR((func->ast, "'%s' redeclared as different kind of symbol", func->name));
                    if (!is_same_type(lu->rec.type, func))
                        ERROR((func->ast, "conflicting types of '%s'", func->name));
                    type_merge(lu->rec.type, func);
                }
            }
            else
            {
                if (scope->lvl && calc_size(var->type) == -1)
                    ERROR((var->ast, "storage size of '%s' isn’t known", var->name));
                var = var_merge(var, scope);
                var->next = res;
                res = var;
                /* check initializer */
                if (initr->type == INITR)
                {
                    var->initr = initr;
                    semantics_initr(initr, scope, var->type);
                }
            }
        }
        useful = 1;
    }
    if (!useful)
        WARNING((type->ast, "useless declaration"));
    return res;
}

ExpType exp_check_aseq(ExpType lhs, ExpType rhs, CNode *ast) {
    exp_check_aseq_(lhs.type, rhs.type, ast);
    lhs.lval = 0;
    return lhs;
}

ExpType semantics_cast(CNode *p, CScope_t scope) {
    CNode *chd = p->chd->next;
    ExpType op = semantics_exp(chd, scope);
    CVar_t var = semantics_declr(p->chd->chd->next,
                                semantics_typespec(p->chd->chd, scope),
                                scope, 0);
    CType_t type = var->type;
    free(var);
    if (!IS_SCALAR(type->type))
        ERROR((p, "conversion to non-scalar type requested"));
    if (!IS_SCALAR(op.type->type))
        ERROR((p, "aggregate value used where a scalar was expected"));
    if (type->type == CARR)
        ERROR((p, "cast specifies array type"));
    if (type->type == CFUNC)
        ERROR((p, "cast specifies function type"));
    type->ast = p;
    op.type = type;
    op.lval = 0;
    if ((p->ext.is_const &= !IS_INT(type->type)))
    {
        p->ext.const_val = chd->ext.const_val;
    }
    return op;
}

ExpType exp_check_arith(ExpType op1, ExpType op2, CNode *p, char kind) {
    CNode *lch = p->chd, 
          *rch = lch->next;
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, p);
    NOT_IGNORE_VOID(op2.type, p);
    res.lval = 0;
    res.type = basic_type_int;
    if ((p->ext.is_const = lch->ext.is_const && rch->ext.is_const))
    {
        long l = lch->ext.const_val,
            r = rch->ext.const_val,
            *a = &(p->ext.const_val);
        switch (kind)
        {
            case '*': *a = l * r; break;
            case '/': *a = l / r; break;
            case '%': *a = l % r; break;
        }
    }
    if (!(IS_ARITH(t1) && IS_ARITH(t2)))
        ERROR((p, "invalid operands to binary operator"));
    return res;
}

ExpType exp_check_bitwise(ExpType op1, ExpType op2, CNode *p, char kind) {
    CNode *lch = p->chd, 
          *rch = lch->next;
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, p);
    NOT_IGNORE_VOID(op2.type, p);
    res.lval = 0;
    res.type = basic_type_int;
    if ((p->ext.is_const = lch->ext.is_const && rch->ext.is_const))
    {
        long l = lch->ext.const_val,
            r = rch->ext.const_val,
            *a = &(p->ext.const_val);
        switch (kind)
        {
            case 'l': *a = l << r; break;
            case 'r': *a = l >> r; break;
            case '&': *a = l & r; break;
            case '|': *a = l | r; break;
            case '^': *a = l ^ r; break;
        }
    }
    if (!(IS_INT(t1) && IS_INT(t2)))
        ERROR((p, "invalid operands to binary operator"));
    return res;
}

ExpType exp_check_add(ExpType op1, ExpType op2, CNode *p, char kind) {
    CNode *lch = p->chd, 
          *rch = lch->next;
    int t1 = op1.type->type,
        t2 = op2.type->type;
    NOT_IGNORE_VOID(op1.type, p);
    NOT_IGNORE_VOID(op2.type, p);
    if (kind == '+' && IS_PTR(t2)) 
    {
        /* place the pointer type in the first place */
        int t = t1;
        ExpType te = op1;

        t1 = t2;
        t2 = t;

        op1 = op2;
        op2 = te;

        CNode *n1 = p->chd;
        CNode *n2 = n1->next;
        n2->next = n1;
        n1->next = NULL;
        p->chd = n2;
    }
    if (t1 == CPTR && calc_size(op1.type->rec.ref) == -1)
        ERROR((p, "invalid use of undefined type"));
    if (t2 == CPTR && calc_size(op2.type->rec.ref) == -1)
        ERROR((p, "invalid use of undefined type"));
    if (kind == '-')
    {
        if (IS_PTR(t2) && !IS_PTR(t1))
            ERROR((p, "invalid operands to binary operator"));
    }
    else
    {
        if (!((IS_INT(t1) || IS_PTR(t1)) && IS_INT(t2)))
            ERROR((p, "invalid operands to binary operator"));
    }
    if ((p->ext.is_const = lch->ext.is_const && rch->ext.is_const))
    {
        long r = rch->ext.const_val,
            *a = &(p->ext.const_val);
        if (t1 == CARR)
        {
            int l = p->chd->ext.offset;
            CType_t type;
            p->ext.var = p->chd->ext.var;
            if (t1 == CPTR) type = op1.type->rec.ref;
            else type = op1.type->rec.arr.elem;
            r *= calc_size(type);
            switch (kind)
            {
                case '+': p->ext.offset = l + r; break;
                case '-': p->ext.offset = l - r; break;
            }
        }
        else
        {
            int l = lch->ext.const_val;
            switch (kind)
            {
                case '+': *a = l + r; break;
                case '-': *a = l - r; break;
            }
        }
    }
    op1.lval = 0;
    return op1; /* int or pointer */
}

ExpType exp_check_int(ExpType op1, CNode *p) {
    if (!IS_INT(op1.type->type))
        ERROR((p, "wrong type argument to unary operator"));
    op1.lval = 0;
    return op1;
}

ExpType exp_check_scalar(ExpType op1, CNode *p) {
    if (!IS_SCALAR(op1.type->type))
        ERROR((p, "wrong type argument to unary operator"));
    op1.lval = 0;
    return op1;
}

ExpType exp_check_deref(ExpType op1, CNode *p) {
    if (!IS_PTR(op1.type->type))
        ERROR((p, "invalid type argument of unary '*'"));
    if (op1.type->rec.ref->type == CFUNC)
        return op1;
    op1.lval = 1;   /* deref changes exp to lval */
    if (calc_size(op1.type = op1.type->rec.ref) == -1)
        ERROR((p, "dereferencing pointer to incomplete type"));
    return op1;
}

ExpType exp_check_ref(ExpType op1, CNode *p) {
    ExpType res;
    CType_t t = op1.type;
    if (t->type == CARR || (t->type == CPTR && t->rec.ref->type == CFUNC))
        return op1;
    if (!op1.lval)
        ERROR((p, "lvalue required as unary '&' operand"));
    /* TODO: constant pointer folding */
    p->ext.is_const = 0;
    /* should not be constant */
    res.lval = 0;
    res.type = ctype_create("", CPTR, p);
    res.type->rec.ref = op1.type;
    return res;
}

ExpType exp_check_sizeof(CNode *p, CScope_t scope) {
    ExpType res, sub;
    if (p->chd->type == DECLR)
    {
        CVar_t abs_declr = semantics_declr(
                                p->chd->chd->next,
                                semantics_typespec(p->chd->chd, scope),
                                scope, 0);
        sub.type = abs_declr->type;
        free(abs_declr);
    }
    else
        sub = semantics_exp(p->chd, scope);
    res.lval = 0;
    res.type = basic_type_int;
    p->ext.const_val = calc_size(sub.type);
    p->ext.is_const = 1;
    return res;
}

ExpType exp_check_inc(ExpType op1, CNode *p) {
    if (!IS_SCALAR(op1.type->type))
        ERROR((p, "wrong type argument to increment/decrement"));
    if (!op1.lval)
        ERROR((p, "lvalue required as increment/decrement operand"));
    op1.lval = 0;
    return op1;
}

ExpType exp_check_logical(ExpType op1, ExpType op2, CNode *p, char kind) {
    CNode *lch = p->chd, 
          *rch = lch->next;
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, p);
    NOT_IGNORE_VOID(op2.type, p);
    res.lval = 0;
    res.type = basic_type_int;

    if ((p->ext.is_const = lch->ext.is_const && rch->ext.is_const))
    {
        long l = lch->ext.const_val,
            r = rch->ext.const_val,
            *a = &(p->ext.const_val);
        switch (kind)
        {
            case '&': *a = l && r; break;
            case '|': *a = l || r; break;
        }
    }

    if (!(IS_SCALAR(t1) && IS_SCALAR(t2)))
        ERROR((p, "invalid operands to binary operator"));
    return res;
}

ExpType exp_check_ass(ExpType lhs, ExpType rhs, CNode *p) {
    NOT_IGNORE_VOID(lhs.type, p);
    NOT_IGNORE_VOID(rhs.type, p);
    if (!lhs.lval)
        ERROR((p, "lvalue required as left operand of assignment"));
    switch (p->rec.subtype)
    {
        case '=' : return exp_check_aseq(lhs, rhs, p);
        case ASS_MUL: return exp_check_aseq(lhs, exp_check_arith(lhs, rhs, p, '*'), p);
        case ASS_DIV: return exp_check_aseq(lhs, exp_check_arith(lhs, rhs, p, '/'), p); 
        case ASS_MOD: return exp_check_aseq(lhs, exp_check_arith(lhs, rhs, p, '%'), p); 

        case ASS_ADD: return exp_check_aseq(lhs, exp_check_add(lhs, rhs, p, '+'), p);
        case ASS_SUB: return exp_check_aseq(lhs, exp_check_add(lhs, rhs, p, '-'), p);

        case ASS_SHL: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p, 'l'), p);
        case ASS_SHR: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p, 'r'), p);
        case ASS_AND: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p, '&'), p);
        case ASS_XOR: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p, '^'), p);
        case ASS_OR: return exp_check_aseq(lhs, exp_check_bitwise(lhs, rhs, p, '|'), p);
        default: assert(0);
    }
}

ExpType exp_check_equality(ExpType op1, ExpType op2, CNode *p, int kind) {
    CNode *lch = p->chd, 
          *rch = lch->next;
    int t1 = op1.type->type,
        t2 = op2.type->type;
    ExpType res;
    NOT_IGNORE_VOID(op1.type, p);
    NOT_IGNORE_VOID(op2.type, p);
    res.lval = 0;
    res.type = basic_type_int;
    if ((p->ext.is_const = lch->ext.is_const && rch->ext.is_const))
    {
        long l = lch->ext.const_val,
            r = rch->ext.const_val,
            *a = &(p->ext.const_val);
        switch (kind)
        {
            case OPT_EQ: *a = l == r; break;
            case OPT_NE: *a = l != r; break;
            case '>': *a = l > r; break;
            case '<': *a = l < r; break;
            case OPT_LE: *a = l <= r; break;
            case OPT_GE: *a = l >= r; break;
        }
    }
    if (IS_ARITH(t1) && IS_ARITH(t2))
        return res;
    if (!(IS_SCALAR(t1) && IS_SCALAR(t2)))
        ERROR((p, "invalid operands to binary operator"));
    if (IS_PTR(t1) && IS_PTR(t2))
    {
        if (!is_same_type(op1.type->rec.ref, op2.type->rec.ref))
            WARNING((p, "comparison of distinct pointer types lacks a cast"));
    }
    else if (IS_PTR(t1) || IS_PTR(t2))
        WARNING((p, "comparison between pointer and integer"));
    return res;
}

ExpType exp_check_postfix(CNode *p, CScope_t scope) {
    CNode *post = p->chd->next;
    ExpType op1 = semantics_exp(p->chd, scope), op2;
    int t1 = op1.type->type, t2;
    switch (post->rec.subtype)
    {
        case POSTFIX_ARR:
            if (!IS_PTR(t1))
                ERROR((p, "subscripted value is neither array nor pointer"));
            op2 = semantics_exp(post->chd, scope);
            t2 = op2.type->type;
            if (!IS_INT(t2))
                ERROR((p, "array subscript is not an integer"));
            p->ext.is_const = 0;
            if (t1 == CARR)
                op1.type = op1.type->rec.arr.elem;
            else
                op1.type = op1.type->rec.ref;
            op1.lval = 1;
            break;
        case POSTFIX_CALL:
            if (!(t1 == CPTR && op1.type->rec.ref->type == CFUNC))
                ERROR((p, "called object is not a function"));
            {
                CNode *arg = post->chd->chd, *t;
                CType_t func = p->chd->ext.type;
                CVar_t param;
                /* pointer to function */
                if (func->type == CPTR) func = func->rec.ref;
                for (t = arg; t; t = t->next)
                    semantics_exp(t, scope);
                if ((param = func->rec.func.params))
                {
                    for (; arg && param;
                            arg = arg->next, param = param->next) 
                        exp_check_aseq_(param->type, arg->ext.type, arg);
                    if (arg || param)
                        ERROR((p, "too many/few arguments to the function"));
                }
                op1.type = func->rec.func.ret;
                op1.lval = 0;
                break;
            }
        case POSTFIX_DOT:
            if (!(t1 == CSTRUCT || t1 == CUNION))
                ERROR((p, "request for the member in something not a structure or union"));
            {
                calc_size(op1.type);
                CVar_t fv = ctable_lookup(op1.type->rec.st.fields,
                                        post->chd->rec.strval);
                if (!fv)
                    ERROR((p, "struct/union has no member named '%s'", post->chd->rec.strval));
                p->ext.var = NULL;
                p->ext.offset = fv->start;
                op1.type = fv->type;
            }
            break;
        case POSTFIX_PTR:
            if (t1 != CPTR)
                ERROR((p, "invalid type argument of '->'"));
            {
                CType_t tref = op1.type->rec.ref;
                if (!(tref->type == CSTRUCT || tref->type == CUNION))
                    ERROR((p, "request for the member in something not a structure or union"));
                if (!tref->rec.st.fields)
                    ERROR((p, "dereferencing pointer to incomplete type"));
                calc_size(tref);
                CVar_t fv = ctable_lookup(tref->rec.st.fields,
                                        post->chd->rec.strval);
                if (!fv)
                    ERROR((p, "struct/union has no member named '%s'", post->chd->rec.strval));
                p->ext.var = fv;
                p->ext.offset = fv->start;
                op1.type = fv->type;
                op1.lval = 1;
            }
            break;
        case OPT_INC: case OPT_DEC:
            op1 = exp_check_inc(op1, p);
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
            {
                CSymbol_t lu = cscope_lookup(scope, p->rec.strval, NS_ID);
                if (!lu) ERROR((p, "'%s' undeclared", p->rec.strval));
                if (lu->kind == CVAR)
                {
                    p->ext.var = lu->rec.var;
                    res.type = p->ext.var->type;
                    res.lval = res.type->type != CARR;
                }
                else
                {
                    p->ext.type = lu->rec.type;
                    res.type = p->ext.type;
                    res.lval = res.type->type == CFUNC;
                    POINTER_CONV(res.type, p);
                    p->ext.var = cvar_create(p->ext.type->name, res.type, NULL);
                }
                p->ext.is_const = 0; /* res.type->type == CARR ||
                                    res.type->type == CFUNC; */
            }
            break;
        case INT:
            res.type = basic_type_int;
            res.lval = 0;
            p->ext.is_const = 1;
            p->ext.const_val = p->rec.intval;
            break;
        case CHAR:
            res.type = basic_type_char;
            res.lval = 0;
            p->ext.is_const = 1;
            {
                char *val = p->rec.strval;
                int intval;
                int len = strlen(val);
                if (*val == '\\')
                {
                    if (len == 2)
                        switch (val[1])
                        {
                            case 'a': intval = '\a'; break;
                            case 'b': intval = '\b'; break;
                            case 'f': intval = '\f'; break;
                            case 'n': intval = '\n'; break;
                            case 'r': intval = '\r'; break;
                            case 't': intval = '\t'; break;
                            case 'v': intval = '\v'; break;
                            case '\\': intval = '\\'; break;
                            case '\'': intval = '\''; break;
                            case '"': intval = '\"'; break;
                            case '\?': intval = '\?'; break;
                            case '0': intval = '\0'; break;
                            default:
                                ERROR((p, "unknown escape sequence"));
                        }
                    else
                    {
                        switch (val[1])
                        {
                            case '0':
                                sscanf(val + 2, "%o", &intval);
                                break;
                            case 'x':
                                sscanf(val + 2, "%x", &intval);
                                break;
                            default:
                                ERROR((p, "unknown escape sequence"));
                        }
                    }
                }
                else
                    intval = *val;
                p->ext.const_val = intval;
            }
            break;
        case STR:
            {
                CType_t type = ctype_create("", CPTR, NULL);
                CSList_t cstr = NEW(CSList);

                cstr->str = p->rec.strval;
                (cstr->prev = cstrs->prev)->next = cstr;
                (cstr->next = cstrs)->prev = cstr;
                cstr->id = cstr->prev->id + 1;
                /* cstrs = cstr; */

                type->rec.ref = basic_type_char;
                res.type = type;
                res.lval = 0;
                p->ext.is_const = 1;
                p->ext.const_val = (long)cstr;
            }
            break;
        case EXP:
            {
                ExpType op1;
                ExpType op2;
                switch (p->rec.subtype)
                {
                    case EXP_CAST:
                        res = semantics_cast(p, scope);
                        break;
                    case EXP_POSTFIX:
                        res = exp_check_postfix(p, scope);
                        break;
                    case KW_SIZEOF:
                        res = exp_check_sizeof(p, scope);
                        break;
                    default:
                        {
                            op1 = semantics_exp(p->chd, scope);
                            if (p->chd->next)
                                op2 = semantics_exp(p->chd->next, scope);
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
                                    res = exp_check_logical(op1, op2, p, '|');
                                    break;
                                case OPT_AND:
                                    res = exp_check_logical(op1, op2, p, '&');
                                    break;
                                case OPT_SHL:
                                    res = exp_check_bitwise(op1, op2, p, 'l');
                                    break;
                                case OPT_SHR:
                                    res = exp_check_bitwise(op1, op2, p, 'r');
                                    break;
                                case '|':
                                case '^':
                                    res = exp_check_bitwise(op1, op2, p, p->rec.subtype);
                                    break;
                                case OPT_EQ:
                                case OPT_NE:
                                case '<':
                                case '>' :
                                case OPT_LE:
                                case OPT_GE:
                                    res = exp_check_equality(op1, op2, p, p->rec.subtype);
                                    break;
                                case '/': 
                                case '%':
                                    res = exp_check_arith(op1, op2, p, p->rec.subtype);
                                    break;
                                case '&':
                                    if (p->chd->next)
                                        res = exp_check_bitwise(op1, op2, p, '&');
                                    else
                                        res = exp_check_ref(op1, p);
                                    break;
                                case '*': 
                                    if (p->chd->next)
                                        res = exp_check_arith(op1, op2, p, '*');
                                    else
                                        res = exp_check_deref(op1, p);
                                    break;
                                case '+':
                                    if (p->chd->next)
                                        res = exp_check_add(op1, op2, p, '+');
                                    else
                                    {
                                        res = op1;
                                        res.lval = 0;
                                    }
                                    break;
                                case '-':
                                    if (p->chd->next)
                                        res = exp_check_add(op1, op2, p, '-');
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
                                default: 
                                    printf("%d\n", p->rec.subtype);
                                    assert(0);
                            }
                        }
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
        ERROR((p->chd, "a scalar is required in 'if' condition"));
    cscope_enter(scope);
    res = semantics_stmt(body1, scope);
    cscope_exit(scope);
    if (body2->type != NOP)
    {
        CVar_t h, t;
        cscope_enter(scope);
        if ((h = semantics_stmt(p->chd->next->next, scope)))
        {
            for (t = h; t->next; t = t->next);
            t->next = res;
            res = h;
        }
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
        ERROR((p->chd->next, "a scalar is required in 'for' condition"));
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
        ERROR((p->chd, "a scalar is required in 'while' condition"));
    cscope_enter(scope);
    scope->inside_loop++;
    res = semantics_stmt(p->chd->next, scope);
    scope->inside_loop--;
    cscope_exit(scope);
    return res;
}

CVar_t semantics_check_loop(CNode *p, CScope_t scope, const char *stmt_name) {
    if (!scope->inside_loop)
        ERROR((p, "%s statement not within a loop", stmt_name));
    return NULL;
}
CVar_t semantics_return(CNode *p, CScope_t scope) {
    assert(scope->func);
    CType_t rt = scope->func->rec.func.ret;
    if (p->chd->type != NOP)
    {
        ExpType t = semantics_exp(p->chd, scope);
        if (rt->type == CVOID)
        {
            if (t.type->type != CVOID)
                WARNING((p->chd, "'return' with a value, in function returning void"));
        }
        else
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
    {
        CVList autos, *tail = &autos;
        autos.next = NULL;
        for (i = decls->chd; i; i = i->next)
        {
            CVar_t vlist = semantics_decl(i, scope);
            CVList_t sa = NULL, a;
            if (vlist)  /* collect local vars */
            {
                CVar_t v;
                for (v = vlist; v->next; v = v->next)
                {
                    a = NEW(CVList);
                    a->var = v;
                    a->next = sa;
                    sa = a;
                }
                a = NEW(CVList);
                a->var = v;
                a->next = sa;
                sa = a;
                v->next = res;
                res = vlist;
            }
            if (sa)
            {
                tail->next = sa;
                for (tail = sa; tail->next; tail = tail->next);
            }
        }
        p->ext.autos = autos.next;
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

CType_t semantics_func(CNode *p, CScope_t scope) {
    CHECK_TYPE(p, FUNC_DEF);
    CVar_t head;
    CType_t func, efunc = NULL, rt;
    cscope_enter(scope);                /* enter function local scope */
    head = semantics_declr(p->chd->next,
                                semantics_typespec(p->chd, scope),
                                scope, FLAG_FUNC_DEF);
    func = head->type;
    rt = func->rec.func.ret;
    if (rt->type != CVOID && calc_size(rt) == -1)
        ERROR((func->rec.func.ret->ast, "return type is an incomplete type"));

    func->rec.func.body = p->chd->next->next;
    func->name = head->name;
    scope->func = func;
    free(head);
    {   /* Note: here is a dirty hack to forcibly push function definition to
           the global scope, while all the types specified in parameters retain in local scope.
           The key point is to make sure semantics_params does not push any var */
        CSNode *ntop = scope->top;
        CVar_t var;
        scope->top = ntop->next;
        scope->lvl--;

        if (!cscope_push_type(scope, func, NS_ID))
        {
            CSymbol_t lu = cscope_lookup(scope, func->name, NS_ID);
            if (lu->kind != CTYPE)
                ERROR((func->ast, "'%s' redeclared as different kind of symbol", func->name));
            efunc = lu->rec.type;
            if (efunc->type != CFUNC)
                ERROR((func->ast, "conflicting types of '%s'", func->name));
            else if (efunc->rec.func.body)
                ERROR((func->ast, "redefintion of function '%s'", func->name));
            else if (!is_same_type(efunc, func))
                ERROR((func->ast, "function defintion does not match the prototype"));
            type_merge(efunc, func);
        }

        scope->top = ntop;
        scope->lvl++;

        for (var = func->rec.func.params; var; var = var->next)
        {
            cscope_push_var(scope, var, NS_ID);
            if (calc_size(var->type) == -1)
                ERROR((var->ast, "parameter '%s' has incomplete type", var->name));
        }
    }
    func->rec.func.local = semantics_comp(p->chd->next->next, scope);   /* check comp */
    cscope_exit(scope);                                        /* exit from local scope */
    if (efunc)
    {
        type_merge(efunc, func);
        free(func);
        func = efunc;
    }
    return func;
}

CType_t make_builtin_func(char *name, CType_t rt) {
    CType_t func = ctype_create(name, CFUNC, NULL);
    func->rec.func.params = NULL;
    func->rec.func.body = NULL;
    func->rec.func.local = NULL;
    func->rec.func.ret = rt;
    return func;
}

typedef struct DNode DNode;
CScope_t typedef_scope;
struct DNode{
    enum DefState kind;
    DNode *next;
} *typedef_stack;

void cibic_init(void) {
    typedef_scope = cscope_create();
    typedef_stack = NULL;
}

int is_identifier(const char *name) {
    CSymbol_t lu;
    if (typedef_stack)
    {
        /* struct tag */
        /* the parser is reading declarators */
        if (typedef_stack->kind == FORCE_ID) return 1;
        /* the parser is reading typedef */
        if (typedef_stack->kind == IN_TYPEDEF) return 1;
        /* no info about name, assume it to be an id by default */
    }
    lu = cscope_lookup(typedef_scope, name, NS_ID);
    if (!lu) return 1;
    return lu->kind == CVAR;
}

void push(char *name) {
    if (typedef_stack && typedef_stack->kind == IN_TYPEDEF)
        cscope_push_type(typedef_scope, ctype_create(name, 0, NULL), NS_ID);
    else
        cscope_push_var(typedef_scope, cvar_create(name, NULL, NULL), NS_ID);
}

CDef_t cdef_create(const char *name, CType_t type, CNode *ast) {
    CDef_t cd = NEW(CDef);
    cd->name = name;
    cd->type = type;
    cd->ast = ast;
    return cd;
}

void def_enter(enum DefState kind) {
    DNode *ntop = NEW(DNode);
    ntop->kind = kind;
    ntop->next = typedef_stack;
    typedef_stack = ntop;
}

void def_exit(void) {
    DNode *ntop = typedef_stack->next;
    free(typedef_stack);
    typedef_stack = ntop;
}

void block_enter(void) { cscope_enter(typedef_scope); }
void block_exit(void) { cscope_exit(typedef_scope); }

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
    CSElem *tp;
    fprintf(stderr, "\n****** CScope ******\n");
    for (p = cs->top; p; p = p->next)
    {
        fprintf(stderr, "Level %d:\n", lvl--);
        for (tp = p->symlist; tp; tp = tp->next)
            fprintf(stderr, "%s ", csymbol_print(tp->sym));
        fprintf(stderr, "\n\n");
    }
    fprintf(stderr, "IDs:\n");
    ctable_debug_print(cs->ids);
    fprintf(stderr, "Tags:\n");
    ctable_debug_print(cs->tags);
    fprintf(stderr, "****** CScope ******\n\n");
}

const char *ctable_cvar_print(void *var) {
    static char buff[MAX_DEBUG_PRINT_BUFF];
    sprintf(buff, "%s@%lx", ((CVar_t )var)->name, (size_t)var);
    return buff;
}

const char *csymbol_print(void *csym) {
    CSymbol_t p = (CSymbol_t)csym;
    static char buff[MAX_DEBUG_PRINT_BUFF];
    switch (p->kind)
    {
        case CVAR:
            sprintf(buff, "%s@%lx", p->rec.var->name, (size_t)p->rec.var);
            break;
        case CTYPE:
            sprintf(buff, "%s@%lx", p->rec.type->name, (size_t)p->rec.type);
            break;
        case CDEF:
            sprintf(buff, "%s@%lx", p->rec.def->name, (size_t)p->rec.def);
    }
    return buff;
}

void ctype_print_(CType_t, int lvl);
void print_tabs(int tnum) { while (tnum--) fprintf(stderr, "    "); }
void ctype_pre_(CType_t type, int lvl) {
    int t= type->type;
    if (t == CARR || t == CFUNC || t == CUNION || t == CSTRUCT)
    {
        fprintf(stderr, "\n");
        print_tabs(lvl);
    }
}

void cvar_print_(CVar_t cv, int lvl) {
    fprintf(stderr, "[var@%lx:%s :: ", (size_t)cv, cv->name);
    ctype_pre_(cv->type, ++lvl);
    ctype_print_(cv->type, lvl);
    fprintf(stderr, "]");
}

void cdef_print_(CDef_t cd, int lvl) {
    fprintf(stderr, "[def@%lx:%s :: ", (size_t)cd, cd->name);
    ctype_pre_(cd->type, ++lvl);
    ctype_print_(cd->type, lvl);
    fprintf(stderr, "]");
}

void ctype_print_(CType_t ct, int lvl) {
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
                CTable_t f = ct->rec.st.fields;
                int i;
                CTNode *fn; 
                lvl++;
                fprintf(stderr, "[%s@%lx:{name:%s}{size:%d}",
                        ct->type == CSTRUCT ? "struct" : "union",
                        (size_t)ct, ct->name, ct->size);

                fprintf(stderr, "{fields:");
                if (f)
                {
                    fprintf(stderr, "\n");
                    int first = 1;
                    for (i = 0; i < MAX_TABLE_SIZE; i++)
                        for (fn = f->head[i]; fn; fn = fn->next)
                        {
                            fprintf(stderr, "%s", first ? (first = 0, "") : ",\n");
                            print_tabs(lvl);
                            cvar_print_((CVar_t)fn->val, lvl);
                        }
                }
                fprintf(stderr, "}]");
            }
            break;
        case CARR:
            {
                CType_t type = ct->rec.arr.elem;
                fprintf(stderr, "[arr:{len:%d}{size:%d}]->",
                        ct->rec.arr.len, ct->size);
                ctype_pre_(type, ++lvl);
                ctype_print_(type, lvl);
            }
            break;
        case CPTR:
            {
                CType_t type = ct->rec.ref;
                fprintf(stderr, "[ptr]->");
                ctype_pre_(type, ++lvl);
                ctype_print_(type, lvl);
            }
            break;
        case CFUNC:
            {
                CType_t type = ct->rec.func.ret;
                CVar_t p;
                lvl++;
                fprintf(stderr, "[func:{name:%s}{size:%d}\n",
                        ct->name, ct->size);
                print_tabs(lvl);
                fprintf(stderr, "{params:");
                if (ct->rec.func.params)
                {
                    fprintf(stderr, "\n");
                    for (p = ct->rec.func.params; p; p = p->next)
                    {
                        print_tabs(lvl + 1);
                        cvar_print_(p, lvl + 1);
                        if (p->next) fprintf(stderr, ",\n");
                    }
                }
                /* print_tabs(lvl); */
                fprintf(stderr, "}\n");
                print_tabs(lvl);
                fprintf(stderr, "{local:");
                if (ct->rec.func.local)
                {
                    fprintf(stderr, "\n");
                    for (p = ct->rec.func.local; p; p = p->next)
                    {
                        print_tabs(lvl + 1);
                        cvar_print_(p, lvl + 1);
                        if (p->next) fprintf(stderr, ",\n");
                    }
                }
                fprintf(stderr, "}]->");
                ctype_pre_(type, lvl);
                ctype_print_(type, lvl);
            }
            break;
    }
}

void ctype_print(CType_t ct) { ctype_print_(ct, 0); }
void cvar_print(CVar_t cv) { cvar_print_(cv, 0); }
void cdef_print(CDef_t cd) { cdef_print_(cd, 0); }

void semantics_check(CNode *p) {
    CScope_t scope = cscope_create();
    basic_type_int = ctype_create("int", CINT, NULL);
    basic_type_char = ctype_create("char", CCHAR, NULL);
    basic_type_void = ctype_create("void", CVOID, NULL);
    builtin_printf = make_builtin_func("printf", basic_type_int);
    builtin_scanf = make_builtin_func("scanf", basic_type_int);
    builtin_print_int = make_builtin_func("__print_int", basic_type_int);
    builtin_print_char = make_builtin_func("__print_char", basic_type_int);
    builtin_print_string = make_builtin_func("__print_string", basic_type_int);
    builtin_memcpy = make_builtin_func("memcpy", basic_type_void);
    {
        CType_t vstar = ctype_create("", CPTR, NULL);
        vstar->rec.ref = basic_type_void;
        builtin_malloc = make_builtin_func("malloc", vstar);
    }
    /* add top-level basic types */
    cscope_push_type(scope, basic_type_int, NS_ID);
    cscope_push_type(scope, basic_type_char, NS_ID);
    cscope_push_type(scope, basic_type_void, NS_ID);
    cscope_push_type(scope, builtin_printf, NS_ID);
    cscope_push_type(scope, builtin_scanf, NS_ID);
    cscope_push_type(scope, builtin_malloc, NS_ID);
    cscope_push_type(scope, builtin_memcpy, NS_ID);
    cscope_push_type(scope, builtin_print_int, NS_ID);
    cscope_push_type(scope, builtin_print_char, NS_ID);
    cscope_push_type(scope, builtin_print_string, NS_ID);
    /* const string counter */
    cstrs = NEW(CSList);
    cstrs->id = -1;
    cstrs->prev = cstrs->next = cstrs;
    /* check all definitions and declarations */
    for (p = p->chd; p; p = p->next)
    {
        switch (p->type)
        {
            case FUNC_DEF: 
                semantics_func(p, scope); break;
            case DECL: case TYPEDEF:
                semantics_decl(p, scope); break;
            default: assert(0);
        }
    }
    {
        int i;
        CTNode *p;
        funcs = NULL;
        gvars = NULL;
        for (i = 0; i < MAX_TABLE_SIZE; i++)
            for (p = scope->ids->head[i]; p; p = p->next)
            {
                CSymbol_t tp = (CSymbol_t)(p->val);
                CType_t func = tp->rec.type;
                if (tp->kind == CTYPE &&
                    func->type == CFUNC &&
                    func->rec.func.body)
                {
                    CTList_t nf = NEW(CTList);
                    CVar_t p;
                    int size;
                    nf->type = func;
                    nf->next = funcs;
                    funcs = nf;
                    size = 0;
                    for (p = func->rec.func.params; p; p = p->next)
                    {
                        CType_t t = p->type;
                        /* force to a word alignment */
                        size += align_shift(size);
                        p->start = size;
                        if (t->type == CARR)
                            size += PTR_SIZE;
                        else
                            size += calc_size(t);
                    }
                    size = 0;
                    for (p = func->rec.func.local; p; p = p->next)
                    {
                        /* force to a word alignment */
                        size += align_shift(size);
                        p->start = size;
                        size += calc_size(p->type);
                    }
                    func->rec.func.local_size = size;
                }
                else if (tp->kind == CVAR)
                {
                    CVList_t nv = NEW(CVList);
                    nv->var = tp->rec.var;
                    nv->next = gvars;
                    nv->var->loc = 1;
                    gvars = nv;
                }
            }
    }
    /*    cscope_debug_print(scope);
    {
        CTNode *p;
        int i;
        for (i = 0; i < MAX_TABLE_SIZE; i++)
            for (p = scope->ids->head[i]; p; p = p->next)
            {
                CSymbol_t tp = (CSymbol_t)(p->val);
                switch (tp->kind)
                {
                    case CVAR: 
                        if (calc_size(tp->rec.var->type) == -1)
                            ERROR((tp->rec.var->ast, "storage size of ‘a’ isn’t known"));
                        cvar_print(tp->rec.var);
                        break;
                    case CTYPE: ctype_print(tp->rec.type); break;
                    case CDEF: cdef_print(tp->rec.def); break;
                }
                fprintf(stderr, "\n\n");
            }
    }
    cnode_debug_print(ast_root, 1); */
}

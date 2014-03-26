#include <stdlib.h>
#include <string.h>
#include "semantics.h"
#define NEW(type) ((type *)malloc(sizeof(type)))

CTable_t ctable_create(Hashfunc_t hfunc) {
    CTable_t ct = NEW(CTable);
    memset(ct->head, 0, sizeof(CTNode*) * MAX_TABLE_SIZE);
    ct->hfunc = hfunc;
    return ct;
}

void *ctable_lookup(CTable_t ct, const char *key) {
    unsigned int hv = (ct->hfunc(key)) % MAX_TABLE_SIZE;
    CTNode *p = ct->head[hv];
    for (; p; p = p->next)
        if (strcmp(p->key, key))
            return p->val;
    return NULL; /* not found */
}

void ctable_insert(CTable_t ct, const char *key, void *val, int lvl) {
    unsigned int hv = (ct->hfunc(key)) % MAX_TABLE_SIZE;
    CTNode *np = NEW(CTNode);
    np->key = key;
    np->val = val;
    np->lvl = lvl;
    np->next = ct->head[hv];
    ct->head[hv] = np;
}

void ctable_clip(CTable_t ct, unsigned int hv, int max_lvl) {
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
    p->lvl = 0;
    p->top = NULL;
    p->tvar = ctable_create(bkdr_hash);
    p->ttype = ctable_create(bkdr_hash);
    return p;
}

void cscope_push_var(CScope_t cs, CVar *var) {
#ifdef CIBIC_DEBUG
    assert(cs->top);
#endif
    var->next = cs->top->vhead;
    cs->top->vhead = var;
    ctable_insert(cs->tvar, var->name, var, cs->lvl);
}

void cscope_push_type(CScope_t cs, CType *type) {
#ifdef CIBIC_DEBUG
    assert(cs->top);
#endif
    type->next = cs->top->thead;
    cs->top->thead = type;
    ctable_insert(cs->ttype, type->name, type, cs->lvl);
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
    CSNode *lower = cs->top->next;
    CVar *vp;
    CType *tp;
    for (vp = cs->top->vhead; vp; vp = vp->next)
        ctable_clip(cs->tvar, bkdr_hash(vp->name), cs->lvl);
    for (tp = cs->top->thead; tp; tp = tp->next)
        ctable_clip(cs->ttype, bkdr_hash(tp->name), cs->lvl);
    free(cs->top);
    cs->top = lower;
    cs->lvl--;
}

CVar *cscope_lookup_var(CScope_t cs, const char *name) {
    return ctable_lookup(cs->tvar, name);
}

CType *cscope_lookup_type(CScope_t cs, const char *name) {
    return ctable_lookup(cs->ttype, name);
}

unsigned int bkdr_hash(const char *str) {
    unsigned int seed = 131;
    unsigned int hv = 0;
    while (*str)
        hv = hv * seed + (unsigned)(*str++);
    return hv;
}

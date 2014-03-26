#ifndef SEMANTICS_H
#define SEMANTICS_H
#include "const.h"
#define CIBIC_DEBUG

struct CTable;
struct CType;

typedef struct CVar{
    const char *name;
    struct CVar *next;    /* next in the linked list */
    struct CType *type;
    int offset;
} CVar;

typedef CVar *CVar_t;
CVar_t cvar_create(const char *name, struct CType *type);

typedef struct CType {
    enum {
        CINT,
        CCHAR,
        CVOID,
        CSTRUCT,
        CUNION,
        CARR,
        CPTR
    } type;
    const char *name;
    struct CType *next;
    union {
        struct CType *fields; /* for a struct or union */
        struct CType *ref;    /* for a pointer */
        struct {
            struct CType *elem;
            int len;
        } arr;                /* for an array */
    } rec;
    int size;   /* memory footprint */
} CType;

typedef CType *CType_t;
CType_t ctype_create(const char *name, int type);

typedef unsigned int (*Hashfunc_t) (const char *);
#ifdef CIBIC_DEBUG
typedef const char *(*Printfunc_t) (void *);
#endif
typedef struct CTable *CTable_t;

typedef struct CTNode {
    const char *key;
    void *val;
    struct CTNode *next;
    int lvl;
} CTNode;

typedef struct CTable {
    struct CTNode *head[MAX_TABLE_SIZE];
    Hashfunc_t hfunc;
#ifdef CIBIC_DEBUG
    Printfunc_t pfunc;
#endif
} CTable;


#ifdef CIBIC_DEBUG
CTable_t ctable_create(Hashfunc_t hfunc, Printfunc_t pfunc);
#else
CTable_t ctable_create(Hashfunc_t hfunc);
#endif
void *ctable_lookup(CTable_t ct, const char *key);
int ctable_insert(CTable_t ct, const char *key, void *val, int lvl);
void ctable_clip(CTable_t ct, const char *key, int max_lvl);
void ctable_debug_print(CTable_t ct);

typedef struct CSNode {
    struct CVar *vhead;
    struct CType *thead;
    struct CSNode *next;
} CSNode;

typedef struct CScope *CScope_t;
typedef struct CScope {
    int lvl;
    struct CSNode *top; 
    struct CTable *tvar;
    struct CTable *ttype;
} CScope;

CScope_t cscope_create();
CVar *cscope_lookup_var(CScope_t cs, const char *name);
CType *cscope_lookup_type(CScope_t cs, const char *name);
int cscope_push_var(CScope_t cs, CVar *var);
int cscope_push_type(CScope_t cs, CType *type);
void cscope_enter(CScope_t cs);
void cscope_exit(CScope_t cs);
void cscope_debug_print(CScope_t cs);

unsigned int bkdr_hash(const char *str);
const char *cvar_print(void *var);
const char *ctype_print(void *type);
#endif

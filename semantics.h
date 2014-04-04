#ifndef SEMANTICS_H
#define SEMANTICS_H
#include "const.h"

typedef struct CNode CNode;
struct CTable;
typedef struct CTable *CTable_t;
struct CType;

typedef struct CVar{
    const char *name;
    struct CVar *next;    /* next in the linked list */
    struct CType *type;
    int offset;
    CNode *ast;
} CVar;

typedef CVar *CVar_t;
CVar_t cvar_create(const char *name, struct CType *type, CNode *ast);
void cvar_print(CVar_t cv);

typedef struct CType {
    enum {
        CINT,
        CCHAR,
        CVOID,
        CSTRUCT,
        CUNION,
        CARR,
        CPTR,
        CFUNC
    } type;
    const char *name;
    struct CType *next;
    union {
        CTable_t fields; /* for a struct or union */
        struct CType *ref;    /* for a pointer */
        struct {
            struct CType *elem;
            int len;
        } arr;                /* for an array */
        struct {
            CVar *params;
            CVar *local;
            struct CType *ret;
            CNode *body;
        } func;               /* for a function */
    } rec;
    int size;   /* memory footprint */
    CNode *ast;
} CType;

typedef CType *CType_t;
CType_t ctype_create(const char *name, int type, CNode *ast);
void ctype_debug_print(CType_t ct);

typedef unsigned int (*Hashfunc_t) (const char *);
#ifdef CIBIC_DEBUG
typedef const char *(*Printfunc_t) (void *);
#endif

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

typedef struct CSVar {
    struct CVar *var;
    struct CSVar *next;
} CSVar;

typedef struct CSType {
    struct CType *type;
    struct CSType *next;
} CSType;

typedef struct CSNode {
    struct CSVar *vhead;
    struct CSType *thead;
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
const char *ctable_cvar_print(void *var);
const char *ctable_ctype_print(void *type);

void semantics_check(CNode *ast);
#endif

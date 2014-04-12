#ifndef SEMANTICS_H
#define SEMANTICS_H
#include "const.h"

typedef struct CNode CNode;
typedef struct CTable *CTable_t;
typedef struct CType CType;
typedef CType *CType_t;
typedef struct CVar CVar;
typedef CVar *CVar_t;

struct CVar {
    const char *name;
    CVar_t next;    /* next in the linked list */
    CType_t type;
    int offset;
    int is_const;
    CNode *ast;
};

CVar_t cvar_create(const char *name, CType_t type, CNode *ast);
void cvar_print(CVar_t cv);

struct CType {
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
    union {
        CTable_t fields; /* for a struct or union */
        CType_t ref;    /* for a pointer */
        struct {
            CType_t elem;
            int len;
        } arr;                /* for an array */
        struct {
            CVar_t params;
            CVar_t local;
            CType_t ret;
            CNode *body;
        } func;               /* for a function */
    } rec;
    int size;   /* memory footprint */
    CNode *ast;
};

CType_t ctype_create(const char *name, int type, CNode *ast);
void ctype_debug_print(CType_t ct);

typedef unsigned int (*Hashfunc_t) (const char *);
#ifdef CIBIC_DEBUG
typedef const char *(*Printfunc_t) (void *);
#endif

typedef struct CTNode CTNode;
struct CTNode {
    const char *key;
    void *val;
    CTNode *next;
    int lvl;
};

typedef struct CTable {
    CTNode *head[MAX_TABLE_SIZE];
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
void ctable_destroy(CTable_t ct);
void *ctable_lookup(CTable_t ct, const char *key);
int ctable_insert(CTable_t ct, const char *key, void *val, int lvl);
void ctable_clip(CTable_t ct, const char *key, int max_lvl);
void ctable_debug_print(CTable_t ct);

typedef struct CSVar CSVar;
struct CSVar {
    CVar_t var;
    CSVar *next;
};

typedef struct CSType CSType;
struct CSType {
    CType_t type;
    CSType *next;
};

typedef struct CSNode CSNode;
struct CSNode {
    CSVar *vhead;
    CSType *thead;
    CSNode *next;
};

typedef struct CScope CScope;
typedef CScope *CScope_t;
struct CScope {
    int lvl;
    CType_t func;
    int inside_loop;
    CSNode *top; 
    CTable_t tvar;
    CTable_t ttype;
    CTable_t ext_link;
};

typedef struct ExpType {
    CType_t type;
    int lval;
} ExpType;

CScope_t cscope_create();
CVar_t cscope_lookup_var(CScope_t cs, const char *name);
CType_t cscope_lookup_type(CScope_t cs, const char *name);
int cscope_push_var(CScope_t cs, CVar_t var);
int cscope_push_type(CScope_t cs, CType_t type);
void cscope_enter(CScope_t cs);
void cscope_exit(CScope_t cs);
void cscope_debug_print(CScope_t cs);

unsigned int bkdr_hash(const char *str);
const char *ctable_cvar_print(void *var);
const char *ctable_ctype_print(void *type);

void semantics_check(CNode *ast);
#endif

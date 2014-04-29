#ifndef SEMANTICS_H
#define SEMANTICS_H
#include "const.h"

typedef struct CNode CNode;
typedef struct CTable *CTable_t;
typedef struct CType CType;
typedef CType *CType_t;
typedef struct CVar CVar;
typedef CVar *CVar_t;
typedef struct CSymbol CSymbol;
typedef CSymbol *CSymbol_t;
typedef struct CDef CDef;
typedef CDef *CDef_t;

typedef struct CBList *CBList_t;
struct CVar {
    char *name;
    CVar_t next;    /* next in the linked list */
    CType_t type;
    int start;
    CNode *ast;
    CBList_t defsite;
};

CVar_t cvar_create(char *name, CType_t type, CNode *ast);
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
    char *name;
    union {
        struct {
            CTable_t fields; /* for a struct or union */
            CVar_t flist;
        } st;
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

CType_t ctype_create(char *name, int type, CNode *ast);
void ctype_debug_print(CType_t ct);

typedef unsigned int (*Hashfunc_t) (const char *);
typedef const char *(*Printfunc_t) (void *);

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
    Printfunc_t pfunc;
} CTable;


CTable_t ctable_create(Hashfunc_t hfunc, Printfunc_t pfunc);
void ctable_destroy(CTable_t ct);
void *ctable_lookup(CTable_t ct, const char *key);
int ctable_insert(CTable_t ct, const char *key, void *val, int lvl);
void ctable_clip(CTable_t ct, const char *key, int max_lvl);
void ctable_debug_print(CTable_t ct);

typedef struct CSElem CSElem;
struct CSElem {
    CSymbol_t sym;
    CSElem *next;
};

typedef struct CSNode CSNode;
struct CSNode {
    CSElem *symlist;
    CSNode *next;
};

typedef struct CScope CScope;
typedef CScope *CScope_t;
struct CScope {
    int lvl;
    CType_t func;
    int inside_loop;
    CSNode *top; 
    CTable_t ids;       /* ordinary identifiers */
    CTable_t tags;      /* union & struct tags */
    CTable_t ext_link;  /* external linkage */
};

typedef struct ExpType {
    CType_t type;
    int lval;
} ExpType;

struct CSymbol {
    enum {
        CTYPE,
        CVAR,
        CDEF
    } kind;
    union {
        CType_t type;
        CVar_t var;
        CDef_t def;
    } rec;
};
const char *csymbol_print(void *csym);
const char *csym_getname(CSymbol_t csym);

struct CDef {
    const char *name;
    CType_t type;
    CNode *ast;
};
CDef_t cdef_create(const char *name, CType_t type, CNode *ast);

CScope_t cscope_create(void);
CSymbol_t cscope_lookup(CScope_t cs, const char *name, int nspace);
int cscope_push_var(CScope_t cs, CVar_t var, int nspace);
int cscope_push_def(CScope_t cs, CDef_t def, int nspace);
int cscope_push_type(CScope_t cs, CType_t type, int nspace);
void cscope_enter(CScope_t cs);
void cscope_exit(CScope_t cs);
void cscope_debug_print(CScope_t cs);

unsigned int bkdr_hash(const char *str);
const char *ctable_cvar_print(void *var);

CScope_t semantics_check(CNode *ast);

enum DefState{
    FORCE_ID,
    IN_TYPEDEF,
    NONE
};

int is_identifier(const char *name);
void push(char *name);
void cibic_init(void);
void block_enter(void);
void block_exit(void);
void def_enter(enum DefState kind);
void def_exit(void);
int calc_size(CType_t type);
#endif

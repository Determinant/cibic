#ifndef SEMANTICS_H
#define SEMANTICS_H
#include <stdint.h>
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

typedef struct CTList CTList;
typedef CTList *CTList_t;
struct CTList {
    CType_t type;
    CTList_t next;
};

typedef struct CVList CVList;
typedef CVList *CVList_t;
struct CVList {
    CVar_t var;
    CVList_t next;
};

typedef struct CSList CSList;
typedef CSList *CSList_t;
struct CSList {
    char *str;
    int id;
    int start;
    CSList_t prev, next;
};

typedef struct CBList *CBList_t;
typedef struct COList *COList_t;
typedef struct CVList *CVList_t;

struct CVar {
    char *name;
    CVar_t next;    /* next in the linked list */
    CType_t type;
    int start;
    CNode *ast;
    CNode *initr;
    CBList_t defsite;
    int loc;
    int reload;
    /* the following fields are used for renaming */
    int cnt;
    int weight;
    COList_t stack;
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
            int local_size;
            int frame_size;
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

void semantics_check(CNode *ast, int quiet);

enum DefState{
    FORCE_ID,
    IN_TYPEDEF,
    NONE
};

typedef struct CPNode CPNode;
typedef struct CPSet {
    struct CPNode {
        uintptr_t key;
        CPNode *next;
    } *head[MAX_TABLE_SIZE];
} CPSet;
typedef CPSet *CPSet_t;

CPSet_t cpset_create(void);
int cpset_insert(CPSet_t cps, uintptr_t key);
int cpset_belongs(CPSet_t cps, uintptr_t key);
void cpset_erase(CPSet_t cps, uintptr_t key);
void cpset_destroy(CPSet_t cps);

int is_identifier(const char *name);
void push(char *name);
void cibic_init(void);
void block_enter(void);
void block_exit(void);
void def_enter(enum DefState kind);
void def_exit(void);
int calc_size(CType_t type);
int align_shift(int x);

extern int scnt;
extern CTList_t funcs;
extern CVList_t gvars;
extern CSList_t cstrs;
#endif

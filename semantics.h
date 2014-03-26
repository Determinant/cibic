#ifndef SEMANTICS_H
#define SEMANTICS_H

#define MAX_TABLE_SIZE  1021

struct CTable;

typedef struct CVar{
    char *name;
    struct CVar *next;    /* next in the linked list */
    struct CType *type;
    int offset;
} CVar;

typedef struct CType {
    enum {
        INT,
        CHAR,
        VOID,
        STRUCT,
        UNION,
        ARR,
        PTR
    } type;
    char *name;
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

typedef unsigned int (*Hashfunc_t) (const char *);
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
} CTable;


CTable_t ctable_create(Hashfunc_t hfunc);
void *ctable_lookup(CTable_t ct, const char *key);
void ctable_insert(CTable_t ct, const char *key, void *val, int lvl);
void ctable_clip(CTable_t ct, unsigned int hv, int max_lvl);

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
void cscope_push_var(CScope_t cs, CVar *var);
void cscope_push_type(CScope_t cs, CType *type);
void cscope_enter(CScope_t cs);
void cscope_exit(CScope_t cs);

unsigned int bkdr_hash(const char *str);
#endif

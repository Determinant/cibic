#ifndef SSA_H
#define SSA_H
#include "const.h"
#include "semantics.h"

typedef struct CInst CInst;
typedef CInst *CInst_t;

typedef struct CRange CRange;
typedef struct CRange *CRange_t;
struct CRange {
    int l, r;   /* [l, r) */
    CRange_t next;
};

typedef struct COpr COpr;
typedef COpr *COpr_t;
struct COpr {
    enum {
        VAR,
        TMP,
        IMM,
        IMMS,
        IMMF
    } kind;
    union {
        CVar_t var;
        CSList_t cstr;
        char *str;
        int imm;
    } info;

    int sub;
    int dep;
    int mod;
    int reg;        /* -1 for spilled, -2 for discarded */
    int begin, end; /* for reg allocation */
    CType_t type;
    CInst_t def;
    CRange_t range;
    COpr_t par;     /* union-find */
    COpr_t cval;
    COpr_t spill;   /* check this reference if spilled */
};

typedef struct COList COList;
typedef COList *COList_t;
struct COList {
    COpr_t opr;
    COList_t next, prev;
};

void colist_remove(COList_t node);

struct CInst {
    enum OpCode {
        BEQZ,   /* conditional jump */
        BNEZ,
        GOTO,   /* unconditional jump */
        ARR,    /* displacement */
        PUSH,   /* push to stack top */
        CALL,   /* call function */
        RET,    /* return */
        WARR,
        MOVE,
        LOAD,   /* load from memory */
        ADDR,   /* get address */
        MUL, DIV, MOD, ADD, SUB,
        SHL, SHR, AND, XOR, OR, NOR,
        LOR, LAND,
        EQ, NE, LT, GT, LE, GE,
        NEG
    } op;
    COpr_t dest, src1, src2;
    CInst_t next, prev;
    CType_t wtype;  /* for WARR */
    int id;
    int is_def;
    int bret;   /* for CALL */
    int offset; /* for PUSH */
};

typedef struct CPhi CPhi;
typedef CPhi *CPhi_t;
struct CPhi {
    COpr_t dest;
    COpr_t *oprs;
    CPhi_t next, prev;
};

typedef struct CBlock CBlock;
typedef CBlock *CBlock_t;
struct CBlock {
    CPhi_t phis;
    CInst_t insts;  /* instructions */
    CBlock_t next, prev;
    int id;
    int ref;        /* if referenced by any gotos */
    int pred;       /* the number of predecessors */
    int first, last;
};

typedef struct CBList CBList;
typedef CBList *CBList_t;
struct CBList {
    CBlock_t cblk;
    CBList_t next;
};

CBlock_t cblock_create(int inc);
void cblock_append(CBlock_t cblk, CInst_t inst);
void cblock_pappend(CBlock_t cblk, CPhi_t phi);
CInst_t cblock_getback(CBlock_t cblk);
int cblock_isempty(CBlock_t cblk);

typedef struct CEdge CEdge;
typedef struct CGraph {
    struct CEdge {
        CBlock *to;
        CEdge *next;
    } *head[MAX_BLOCK], *rhead[MAX_BLOCK];
} CGraph;

typedef struct CPNode CPNode;
typedef struct CPSet {
    struct CPNode {
        long key;
        CPNode *next;
    } *head[MAX_TABLE_SIZE];
} CPSet;
typedef CPSet *CPSet_t;

CPSet_t cpset_create(void);
int cpset_insert(CPSet_t cps, long key);
int cpset_belongs(CPSet_t cps, long key);
void cpset_destroy(CPSet_t cps);


typedef struct CInterv {
    COpr_t opr;
    CRange_t range;
} CInterv;

typedef struct CENode CENode; 
typedef struct CExpMap {
    struct CENode {
        CInst_t exp;
        CENode *next;
    } *head[MAX_TABLE_SIZE];
} CExpMap;
typedef CExpMap *CExpMap_t;

CExpMap_t cexpmap_create(void);
unsigned int cexpmap_hash(CInst_t exp);
int cexpmap_comp(CInst_t exp1, CInst_t exp2);
void cexpmap_insert(CExpMap_t cem, CInst_t exp);
CInst_t cexpmap_lookup(CExpMap_t cem, CInst_t exp);
void cexpmap_clear(CExpMap_t cem);
void cexpmap_destroy(CExpMap_t cem);

void ssa_generate(void);
COpr_t cinterv_repr(COpr_t opr);
void cinst_print(FILE *stream, CInst_t inst);
int overlap_with_beg(COpr_t i, int beg);

extern int gbbase;
extern CBlock_t entry;
extern COList_t defs;
extern CType_t func;
extern const int avail_regs[];
extern const int MAX_AVAIL_REGS;

#endif

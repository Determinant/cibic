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
        int imm;
        char *str;
    } info;

    int sub;
    CInst_t def;
    CRange_t range;
    int reg;        /* -1 for spilled */
    COpr_t par;     /* union-find */
};

typedef struct COList COList;
typedef COList *COList_t;
struct COList {
    COpr_t opr;
    COList_t next;
};

struct CInst {
    enum {
        BEQZ,   /* conditional jump */
        BNEZ,
        GOTO,   /* unconditional jump */
        ARR,    /* displacement */
        WARR,
        PUSH,   /* push to stack top */
        CALL,   /* call function */
        RET,    /* return */
        MOVE,
        LOAD,   /* load from memory */
        MUL, DIV, MOD, ADD, SUB,
        SHL, SHR, AND, XOR, OR,
        LOR, LAND, NEG, NOR, SEQ,
        EQ, NE, LT, GT, LE, GE
    } op;
    COpr_t dest, src1, src2;
    CInst_t next, prev;
    int id;
    int is_def;
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

typedef struct CVList CVList;
typedef CVList *CVList_t;
struct CVList {
    CVar_t var;
    CVList_t next;
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

CPSet_t cpset_create();
int cpset_insert(CPSet_t cps, long key);
int cpset_belongs(CPSet_t cps, long key);
void cpset_destroy(CPSet_t cps);


typedef struct CInterv {
    COpr_t opr;
    CRange_t range;
} CInterv;

void ssa_generate(CScope_t scope);
#endif

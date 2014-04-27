#ifndef SSA_H
#define SSA_H
#include "const.h"
#include "semantics.h"
typedef struct COpr {
    enum {
        VAR,
        TMP,
        IMM
    } kind;
    union {
        CVar_t var;
        int imm;
    } info;
} COpr;

typedef struct CInst CInst;
typedef CInst *CInst_t;
struct CInst {
    enum {
        MOVE,
        BEQZ,   /* conditional jump */
        BNEZ,
        GOTO,   /* unconditional jump */
        ARR,    /* displacement */
        WARR,
        MUL, DIV, MOD, ADD, SUB, SHL, SHR, AND, XOR, OR, LOR, LAND, NEG, NOR, SEQ, EQ, NE, LT, GT, LE, GE
    } op;
    COpr dest, src1, src2;
    CInst_t next, prev;
};

typedef struct CBlock CBlock;
typedef CBlock *CBlock_t;
struct CBlock {
    CInst_t insts;  /* instructions */
    CBlock_t next, prev;
    int id;
    int ref;        /* if referenced by any gotos */
};

CBlock_t cblock_create();
void cblock_append(CBlock_t cblk, CInst_t inst);
CInst_t cblock_getback(CBlock_t cblk);
int cblock_isempty(CBlock_t cblk);

typedef struct CEdge CEdge;
typedef struct CGraph {
    struct CEdge {
        CBlock *to;
        CEdge *next;
    } *head[MAX_BLOCK], *rhead[MAX_BLOCK];
} CGraph;

void ssa_generate(CScope_t scope);
#endif

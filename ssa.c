#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "ast.h"
#include "ssa.h"
#include "mips.h"
#define NEW(type) ((type *)malloc(sizeof(type)))
#define DBLINK(from, to) ((from)->next = (to))->prev = (from)

static CGraph cfg, dtree;
static CBlock_t blks[MAX_BLOCK];
static COList_t raw_defs;   /* defintion of all vars and tmps */
static int bcnt;            /* block counter */
static int tcnt;            /* temporary counter */

/* for code generation */
int gbbase;
CBlock_t entry;
CType_t func;
COList_t defs;              /* all defintions that have actual effects */

COpr_t copr_create(void) {
    COpr_t opr = NEW(COpr);
    opr->type = NULL;
    opr->cval = NULL;
    opr->same = opr;
    opr->dep = 0;
    opr->mod = 0;
    opr->par = opr;
    return opr;
}

CInst_t cinst_create(void) {
    CInst_t inst = NEW(CInst);
    inst->dest = NULL;
    inst->src1 = NULL;
    inst->src2 = NULL;
    return inst;
}

CBlock_t cblock_create(int inc) {
    CBlock_t cblk = NEW(CBlock);
    CInst_t dum = cinst_create();
    CPhi_t pdum = NEW(CPhi);
    dum->prev = dum;
    dum->next = dum;
    pdum->prev = pdum;
    pdum->next = pdum;
    cblk->insts = dum;
    cblk->phis = pdum;
    cblk->next = NULL;
    if (inc)
        cblk->id = bcnt++;
    cblk->ref = 0;
    return cblk;
}

void cblock_append(CBlock_t cblk, CInst_t inst) {
    CInst_t head = cblk->insts;
    (inst->prev = head->prev)->next = inst;
    (inst->next = head)->prev = inst;
}

void cblock_pushfront(CBlock_t cblk, CInst_t inst) {
    CInst_t head = cblk->insts;
    (inst->next = head->next)->prev = inst;
    (inst->prev = head)->next = inst;
}

void cblock_pappend(CBlock_t cblk, CPhi_t phi) {
    CPhi_t head = cblk->phis;
    (phi->prev = head->prev)->next = phi;
    (phi->next = head)->prev = phi;
}

void cblock_popback(CBlock_t cblk) {
    CInst_t last = cblk->insts->prev;
    last->next->prev = last->prev;
    last->prev->next = last->next;
}

void cblock_popfront(CBlock_t cblk) {
    CInst_t first = cblk->insts->next;
    first->next->prev = first->prev;
    first->prev->next = first->next;
}

CInst_t cblock_getback(CBlock_t cblk) {
    CInst_t res = cblk->insts->prev;
    return res != cblk->insts ? res : NULL;
}

int cblock_isempty(CBlock_t cblk) {
    return cblk->insts->prev == cblk->insts;
}

CVar_t ctmp_create(void) {
    static char buff[MAX_NAMELEN];
    sprintf(buff, "t%d", tcnt++);
    return cvar_create(strdup(buff), NULL, NULL);
}

void ctmp_destroy(CVar_t type) {
    /* allocated dynamically */
    free(type->name);
    free(type);
}

void cfg_clear(void) {
    int i;
    for (i = 0; i < MAX_BLOCK; i++)
    {
        CEdge *p, *np;
        for (p = cfg.head[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
        cfg.head[i] = NULL;
        for (p = cfg.rhead[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
        cfg.rhead[i] = NULL;
    }
}

void dtree_clear(void) {
    int i;
    CEdge *p, *np;
    for (i = 0; i < MAX_BLOCK; dtree.head[i++] = NULL)
        for (p = dtree.head[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
}

void cfg_add_edge(CBlock_t from, CBlock_t to) {
    int fid = from->id, tid = to->id;
    fprintf(stderr, "%d -> %d\n", from->id, to->id);
    CEdge *e = NEW(CEdge), *re = NEW(CEdge);
    e->to = to;
    e->next = cfg.head[fid];
    cfg.head[fid] = e;

    re->to = from;
    re->next = cfg.rhead[tid];
    cfg.rhead[tid] = re;
}

void dtree_add_edge(CBlock_t from, CBlock_t to) {
/*    printf("%d d-> %d\n", from->id, to->id); */
    int id = from->id;
    CEdge *e = NEW(CEdge);
    e->to = to;
    e->next = dtree.head[id];
    dtree.head[id] = e;
}

void copr_print(FILE *f, COpr_t opr) {
    switch (opr->kind)
    {
        case VAR: 
                  fprintf(f, "%s_%d", opr->info.var->name, opr->sub);
                  break;
        case TMP: fprintf(f, "%s", opr->info.var->name);
                  break;
        case IMM: fprintf(f, "%d", opr->info.imm);
                  break;
        case IMMS: fprintf(f, "\"%s\"", opr->info.cstr->str);
                  break;
        case IMMF: fprintf(f, "%s", opr->info.str);
                  break;
    }
}

void cinst_print(FILE *f, CInst_t inst) {
    switch (inst->op)
    {
        case LOAD:
            fprintf(f, "load ");
            copr_print(f, inst->dest);
            break;
        case MOVE:
            copr_print(f, inst->dest);
            fprintf(f, " = ");
            copr_print(f, inst->src1);
            break;
        case BEQ:
            fprintf(f, "if (");
            copr_print(f, inst->src1);
            fprintf(f, " == ");
            copr_print(f, inst->src2);
            fprintf(f, ") goto _L");
            copr_print(f, inst->dest);
            break;
        case BNE:
            fprintf(f, "if (");
            copr_print(f, inst->src1);
            fprintf(f, " != ");
            copr_print(f, inst->src2);
            fprintf(f, ") goto _L");
            copr_print(f, inst->dest);
            break;
        case GOTO:
            fprintf(f, "goto _L");
            copr_print(f, inst->dest);
            break;
        case ARR:
            copr_print(f, inst->dest);
            fprintf(f, " = ");
            copr_print(f, inst->src1);
            fprintf(f, "[");
            copr_print(f, inst->src2);
            fprintf(f, "]");
            break;
        case NEG:
            copr_print(f, inst->dest);
            fprintf(f, " = -");
            copr_print(f, inst->src1);
            break;
        case WARR:
            copr_print(f, inst->dest);
            fprintf(f, "[");
            copr_print(f, inst->src2);
            fprintf(f, "] = ");
            copr_print(f, inst->src1);
            break;
        case PUSH:
            fprintf(f, "push ");
            copr_print(f, inst->src1);
            break;
        case CALL:
            copr_print(f, inst->dest);
            fprintf(f, " = call ");
            copr_print(f, inst->src1);
            break;
        case RET:
            if (inst->src1)
            {
                fprintf(f, "return ");
                copr_print(f, inst->src1);
            }
            else fprintf(f, "return");
            break;
        case ADDR:
            copr_print(f, inst->dest);
            fprintf(f, " = addr ");
            copr_print(f, inst->src1);
            break;
        default:
            {
                const char *op;
                switch (inst->op)
                {
                    case MUL: op = "*"; break;
                    case DIV: op = "/"; break;
                    case MOD: op = "%"; break;
                    case ADD: op = "+"; break;
                    case SUB: op = "-"; break;
                    case SHL: op = "<<"; break;
                    case SHR: op = ">>"; break;
                    case AND: op = "&"; break;
                    case XOR: op = "^"; break;
                    case OR: op = "|"; break;
                    case LT: op = "<"; break;
                    case GT: op = ">"; break;
                    case LE: op = "<="; break;
                    case GE: op = ">="; break;
                    case EQ: op = "=="; break;
                    case NE: op = "!="; break;
                    case NOR: op = "nor"; break;
                    default: ;
                }
                copr_print(f, inst->dest);
                fprintf(f, " = ");
                copr_print(f, inst->src1);
                fprintf(f, " %s ", op);
                copr_print(f, inst->src2);
            }
    }
    fprintf(f, "\n");
}

void cphi_print(CPhi_t phi, CBlock_t blk) {
    int i;
    fprintf(stderr, "%s_%d = phi", phi->dest->info.var->name,
                                    phi->dest->sub);
    for (i = 0; i < blk->pred; i++)
        fprintf(stderr, " %s_%d", phi->oprs[i]->info.var->name,
                                    phi->oprs[i]->sub);
    fprintf(stderr, "\n");
}

void cblock_print(CBlock_t blk) {
    /*if (blk->ref)*/
    fprintf(stderr, "_L%d:\n", blk->id + gbbase);
    {
        CPhi_t p, sp = blk->phis;
        for (p = sp->next; p != sp; p = p->next)
        {
            fprintf(stderr, "\t");
            cphi_print(p, blk);
        }
    }
    {
        CInst_t p, sp = blk->insts;
        for (p = sp->next; p != sp; p = p->next)
        {
            fprintf(stderr, "%02d\t", p->id);
            cinst_print(stderr, p);
        }
    }
}
void ssa_func_print(CBlock_t p) {
    for (; p; p = p->next)
        cblock_print(p);
}
void ssa_func(CType_t);
void ssa_generate(void) {
    CTList_t f;
    mips_prologue();
    for (f = funcs; f; f = f->next)
    {
        func = f->type;
        fprintf(stderr, "%s:\n", func->name);
        ssa_func(func);
        ssa_func_print(entry);
        mips_generate();
        gbbase += bcnt;
        bcnt = 0;
    }
}

#define POINTER_CONV(inst) \
do { \
    if (rt->type == CARR) \
    { \
        /* convert to pointer type */ \
        CType_t a; \
        a = ctype_create("", CPTR, p); \
        a->rec.ref = rt->rec.arr.elem; \
        (inst)->op = ADD; \
        (inst)->dest->type = a;  \
    } \
    else if (rt->type == CSTRUCT || rt->type == CUNION) \
        (inst)->op = ADD; \
    else (inst)->op = ARR; \
} while (0)


COpr_t ssa_exp_(CNode *p, CBlock_t *, CInst_t, CBlock_t);
COpr_t ssa_postfix(CNode *p, CBlock_t *cur, CInst_t lval, CBlock_t succ) {
    CNode *post = p->chd->next;
    CType_t rt = p->ext.type;
    CInst_t base = cinst_create();
    switch (post->rec.subtype)
    {
        case POSTFIX_ARR:
            {
                CInst_t off = cinst_create();
                off->dest = copr_create();
                off->dest->kind = TMP;
                off->dest->info.var = ctmp_create();
                off->dest->type = post->chd->ext.type;
                off->op = MUL;
                off->src1 = ssa_exp_(post->chd, cur, NULL, succ);
                off->src2 = copr_create();
                off->src2->kind = IMM;
                off->src2->info.imm = calc_size(rt);
                cblock_append(*cur, off);

                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create();
                base->dest->type = rt;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = off->dest;
                POINTER_CONV(base);
            }
            break;
        case POSTFIX_DOT:
            {
                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create();
                base->dest->type = rt;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = copr_create();
                base->src2->kind = IMM;
                base->src2->info.imm = p->ext.offset;
                POINTER_CONV(base);
            }
            break;
        case POSTFIX_PTR:
            {
                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create();
                base->dest->type = rt;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = copr_create();
                base->src2->kind = IMM;
                base->src2->info.imm = p->ext.offset;
                POINTER_CONV(base);
            }
            break;
        case POSTFIX_CALL:
            {
                CNode *arg = post->chd->chd;
                CInst h;
                CInst_t t = &h, n;
                base->op = CALL;
                base->src1 = ssa_exp_(p->chd, cur, lval, succ);
                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create();
                base->dest->type = rt;
                for (; arg; arg = arg->next)
                {
                    CInst_t pi = cinst_create();
                    pi->op = PUSH;
                    pi->src1 = ssa_exp_(arg, cur, lval, succ);
                    t->next = pi;
                    t = pi;
                }
                t->next = NULL;
                for (t = h.next; t; t = n)
                {
                    n = t->next;
                    cblock_append(*cur, t);
                }
            }
            break;
        default:
            {
                CInst_t tins = cinst_create();
                ssa_exp_(p->chd, cur, tins, succ);
                base->op = post->rec.subtype == OPT_INC ? ADD : SUB;
                base->src2 = copr_create();
                base->src2->kind = IMM;
                base->src2->info.imm = 1;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                if (tins->op == MOVE)
                {
                    base->dest = tins->dest;
                    cblock_append(succ, base);
                    free(tins);
                }
                else
                {
                    CInst_t tins2 = cinst_create();
                    base->dest = copr_create();
                    base->dest->kind = TMP;
                    base->dest->info.var = ctmp_create();
                    base->dest->type = rt;
                    tins->src1 = base->dest;
                    tins2->op = ARR;
                    tins2->src1 = tins->dest;
                    tins2->src2 = tins->src2;
                    tins2->dest = copr_create();
                    tins2->dest->kind = TMP;
                    tins2->dest->info.var = ctmp_create();
                    tins2->dest->type = rt;

                    cblock_append(succ, base);
                    cblock_append(succ, tins);
                    cblock_append(succ, tins2);
                }
                return base->src1;
            }
            break;
    }

    if (lval)
    {
        lval->op = WARR;
        lval->dest = base->src1;
        lval->src2 = base->src2;
        lval->wtype = p->ext.type;
        free(base);
        return lval->dest;
    }
    cblock_append(*cur, base);
    return base->dest;
}

CInst_t compress_branch(COpr_t r, CBlock_t blk, int rev) {
    int flag = -1;
    CInst_t b;
    if (r->kind == TMP)
    {
        b = cblock_getback(blk);
        if (b)
        {
            assert(r == b->dest);
            if (b->op == EQ)
                flag = 0;
            else if (b->op == NE)
                flag = 1;
        }
    }
    if (flag != -1)
        b->op = flag ? BNE : BEQ;
    else
    {
        b = cinst_create();
        b->op = BNE;
        b->src1 = r;
        b->src2 = copr_create();
        b->src2->kind = IMM;
        b->src2->info.imm = 0;
        cblock_append(blk, b);
    }
    b->op ^= rev;
    b->dest = copr_create();
    b->dest->kind = IMM;
    return b;
}

#define IS_PTR(tt) ((tt) == CPTR || (tt) == CARR)
COpr_t ssa_exp(CNode *, CBlock_t *, int);
COpr_t ssa_exp_(CNode *p, CBlock_t *cur, CInst_t lval, CBlock_t succ) {/*{{{*/
    COpr_t res;
    CInst_t inst = cinst_create();
    switch (p->type)
    {
        case NOP: ; break;
        case ID:
            res = copr_create();
            res->kind = VAR;
            res->info.var = p->ext.var;
            res->type = p->ext.type;
            {
                CVar_t var = res->info.var;
                CType_t type = var->type;
                if (type->type == CPTR &&
                        type->rec.ref->type == CFUNC)
                {
                    char *name = type->rec.ref->name;
                    if (*name != '\0')
                    {
                        res->kind = IMMF;
                        res->info.str = name;
                    }
                }
            }

            if (lval)
            {
                lval->op = MOVE;
                lval->dest = res;
            }
            break;
        case STR:
            res = copr_create();
            res->kind = IMMS;
            res->info.cstr = (CSList_t)(p->ext.const_val);
            break;
        default:
            if (p->ext.is_const)
            {
                res = copr_create();
                res->kind = IMM;
                res->info.imm = p->ext.const_val;
            }
            else
            {
                int op = p->rec.subtype;
                int rec = 1, auto_dest = 1;

                if (op == ',')
                {
                    ssa_exp(p->chd, cur, 1);
                    return ssa_exp_(p->chd->next, cur, NULL, succ);
                }
                else if (op == '=')
                {
                    inst->src1 = ssa_exp_(p->chd->next, cur, NULL, succ);
                    ssa_exp_(p->chd, cur, inst, succ);
                    if (inst->op == MOVE)
                    {
                        CInst_t last = cblock_getback(*cur);
                        if (last && last->dest->kind == TMP
                                && last->dest == inst->src1)
                        {
                            free(last->dest);
                            last->dest = inst->dest;
                            free(inst);
                            return last->dest;
                        }
                        else 
                        {
                            cblock_append(*cur, inst);
                            return inst->dest;
                        }
                    }
                    else
                    {
                        CInst_t tins = cinst_create();
                        cblock_append(*cur, inst);
                        tins->op = ARR;
                        tins->src1 = inst->dest;    /* base */
                        tins->src2 = inst->src2;    /* displacement */
                        tins->dest = copr_create();
                        tins->dest->kind = TMP;
                        tins->dest->info.var = ctmp_create();
                        tins->dest->type = p->ext.type;
                        cblock_append(*cur, tins);
                        return tins->dest;
                    }
                }
                else if (op == '*' && !p->chd->next)
                {
                    if (lval)
                    {
                        lval->op = WARR;
                        lval->dest = ssa_exp_(p->chd, cur, NULL, succ);
                        lval->src2 = copr_create();
                        lval->src2->kind = IMM;
                        lval->src2->info.imm = 0;
                        lval->wtype = p->ext.type;
                        return lval->dest;
                    }
                    else
                    {
                        CType_t rt = p->ext.type;
                        inst->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                        inst->src2 = copr_create();
                        inst->src2->kind = IMM;
                        inst->src2->info.imm = 0;
                        inst->dest = copr_create();
                        inst->dest->kind = TMP;
                        inst->dest->info.var = ctmp_create();
                        inst->dest->type = rt;
                        POINTER_CONV(inst);
                        cblock_append(*cur, inst);
                        return inst->dest;
                    }
                }
                else if (op == OPT_AND)
                {
                    CBlock_t else_h = cblock_create(1), else_t = else_h,
                             one_blk = cblock_create(1),
                             zero_blk = cblock_create(1),
                             next_blk = cblock_create(1), sblk;
                    COpr_t r0, r1, ri;
                    CInst_t b, a0, a1;
                    CPhi_t m = NEW(CPhi);
                    CNode *t;
                    /* constant opt */
                    a0 = cinst_create();
                    a0->op = MOVE;
                    a0->dest = copr_create();
                    a0->dest->kind = TMP;
                    a0->dest->info.var = ctmp_create();
                    a0->dest->type = p->ext.type; /* int */
                    a0->src1 = copr_create();
                    a0->src1->kind = IMM;
                    a0->src1->info.imm = 0;
                    cblock_append(zero_blk, a0);

                    a1 = cinst_create();
                    a1->op = MOVE;
                    a1->dest = copr_create();
                    a1->dest->kind = TMP;
                    a1->dest->info.var = ctmp_create();
                    a1->dest->type = p->ext.type;
                    a1->src1 = copr_create();
                    a1->src1->kind = IMM;
                    a1->src1->info.imm = 1;
                    cblock_append(one_blk, a1);

                    m->dest = copr_create();
                    m->dest->kind = TMP;
                    m->dest->info.var = ctmp_create();
                    m->dest->type = p->ext.type;
                    m->oprs = (COpr_t *)malloc(sizeof(COpr_t) * 2);
                    m->oprs[0] = a0->dest;
                    m->oprs[1] = a1->dest;
                    cblock_pappend(next_blk, m);

                    r1 = ssa_exp_(p->chd->next, &else_t, NULL, succ);
                    compress_branch(r1, else_t, 1)->dest->info.imm = zero_blk->id + gbbase;
                    zero_blk->ref = 1;

                    sblk = else_h;
                    for (t = p->chd; t->rec.subtype == OPT_AND; t = t->chd)
                    {
                        CBlock_t c_h = cblock_create(1), c_t = c_h;
                        ri = ssa_exp_(t->chd->next, &c_t, NULL, succ);
                        compress_branch(ri, c_t, 1)->dest->info.imm = zero_blk->id + gbbase;
                        cfg_add_edge(c_t, zero_blk);  /* tail */
                        DBLINK(c_t, sblk);
                        cfg_add_edge(c_t, sblk);      /* connect to header */
                        sblk = c_h;
                    }

                    r0 = ssa_exp_(t, cur, NULL, succ);
                    compress_branch(r0, *cur, 1)->dest->info.imm = zero_blk->id + gbbase;
                    cfg_add_edge(*cur, zero_blk);
                    DBLINK(*cur, sblk);
                    cfg_add_edge(*cur, sblk);

                    b = cinst_create();
                    b->op = GOTO;
                    b->dest = copr_create();
                    b->dest->kind = IMM;
                    b->dest->info.imm = next_blk->id + gbbase;
                    cblock_append(one_blk, b);
                    next_blk->ref = 1;

                    DBLINK(else_t, one_blk);
                    DBLINK(one_blk, zero_blk);
                    DBLINK(zero_blk, next_blk);

                    cfg_add_edge(else_t, one_blk);
                    cfg_add_edge(else_t, zero_blk);
                    cfg_add_edge(one_blk, next_blk);
                    cfg_add_edge(zero_blk, next_blk);

                    *cur = next_blk;
                    return m->dest;
                }
                else if (op == OPT_OR)
                {
                    CBlock_t else_h = cblock_create(1), else_t = else_h,
                             one_blk = cblock_create(1),
                             zero_blk = cblock_create(1),
                             next_blk = cblock_create(1), sblk;
                    COpr_t r0, r1, ri;
                    CInst_t b, a0, a1;
                    CPhi_t m = NEW(CPhi);
                    CNode *t;
                    /* constant opt */
                    a0 = cinst_create();
                    a0->op = MOVE;
                    a0->dest = copr_create();
                    a0->dest->kind = TMP;
                    a0->dest->info.var = ctmp_create();
                    a0->dest->type = p->ext.type; /* int */
                    a0->src1 = copr_create();
                    a0->src1->kind = IMM;
                    a0->src1->info.imm = 0;
                    cblock_append(zero_blk, a0);

                    a1 = cinst_create();
                    a1->op = MOVE;
                    a1->dest = copr_create();
                    a1->dest->kind = TMP;
                    a1->dest->info.var = ctmp_create();
                    a1->dest->type = p->ext.type;
                    a1->src1 = copr_create();
                    a1->src1->kind = IMM;
                    a1->src1->info.imm = 1;
                    cblock_append(one_blk, a1);

                    m->dest = copr_create();
                    m->dest->kind = TMP;
                    m->dest->info.var = ctmp_create();
                    m->dest->type = p->ext.type;
                    m->oprs = (COpr_t *)malloc(sizeof(COpr_t) * 2);
                    m->oprs[0] = a1->dest;
                    m->oprs[1] = a0->dest;
                    cblock_pappend(next_blk, m);

                    r1 = ssa_exp_(p->chd->next, &else_t, NULL, succ);
                    compress_branch(r1, else_t, 0)->dest->info.imm = one_blk->id + gbbase;
                    one_blk->ref = 1;

                    sblk = else_h;
                    for (t = p->chd; t->rec.subtype == OPT_OR; t = t->chd)
                    {
                        CBlock_t c_h = cblock_create(1), c_t = c_h;
                        ri = ssa_exp_(t->chd->next, &c_t, NULL, succ);
                        compress_branch(ri, c_t, 0)->dest->info.imm = one_blk->id + gbbase;
                        cfg_add_edge(c_t, one_blk);  /* tail */
                        DBLINK(c_t, sblk);
                        cfg_add_edge(c_t, sblk);      /* connect to header */
                        sblk = c_h;
                    }

                    r0 = ssa_exp_(t, cur, NULL, succ);
                    compress_branch(r0, *cur, 0)->dest->info.imm = one_blk->id + gbbase;
                    cfg_add_edge(*cur, one_blk);
                    DBLINK(*cur, sblk);
                    cfg_add_edge(*cur, sblk);

                    b = cinst_create();
                    b->op = GOTO;
                    b->dest = copr_create();
                    b->dest->kind = IMM;
                    b->dest->info.imm = next_blk->id + gbbase;
                    cblock_append(zero_blk, b);
                    next_blk->ref = 1;

                    DBLINK(else_t, zero_blk);
                    DBLINK(zero_blk, one_blk);
                    DBLINK(one_blk, next_blk);

                    cfg_add_edge(else_t, zero_blk);
                    cfg_add_edge(else_t, one_blk);
                    cfg_add_edge(zero_blk, next_blk);
                    cfg_add_edge(one_blk, next_blk);

                    *cur = next_blk;
                    return m->dest;
                }
                else if (op == '+' && IS_PTR(p->ext.type->type))
                {
                    COpr_t lhs = ssa_exp_(p->chd, cur, lval, succ),
                           rhs = ssa_exp_(p->chd->next, cur, lval, succ);
                    CInst_t index = cinst_create();
                    CType_t et = p->chd->ext.type;
                    if (et->type == CPTR)
                        et = et->rec.ref;
                    else
                        et = et->rec.arr.elem;
                    index->op = MUL;
                    index->dest = copr_create();
                    index->dest->kind = TMP;
                    index->dest->info.var = ctmp_create();
                    index->dest->type = p->chd->next->ext.type;
                    index->src1 = rhs;
                    index->src2 = copr_create();
                    index->src2->kind = IMM;
                    index->src2->info.imm = calc_size(et);

                    inst->op = ADD;
                    inst->dest = copr_create();
                    inst->dest->kind = TMP;
                    inst->dest->info.var = ctmp_create();
                    inst->dest->type = p->ext.type;
                    inst->src1 = lhs;
                    inst->src2 = index->dest;
                    cblock_append(*cur, index);
                    cblock_append(*cur, inst);
                    return inst->dest;
                }
                else if (op == '-' && IS_PTR(p->chd->ext.type->type))
                {
                    CType_t nt = p->chd->next->ext.type;
                    CType_t et = p->chd->ext.type;
                    COpr_t lhs = ssa_exp_(p->chd, cur, lval, succ),
                           rhs = ssa_exp_(p->chd->next, cur, lval, succ);
                    CInst_t diff = cinst_create();

                    if (et->type == CPTR)
                        et = et->rec.ref;
                    else
                        et = et->rec.arr.elem;

                    if (IS_PTR(nt->type))
                    {
                        diff->op = SUB;
                        diff->dest = copr_create();
                        diff->dest->kind = TMP;
                        diff->dest->info.var = ctmp_create();
                        diff->dest->type = p->ext.type;
                        diff->src1 = lhs;
                        diff->src2 = rhs;

                        inst->op = DIV;
                        inst->dest = copr_create();
                        inst->dest->kind = TMP;
                        inst->dest->info.var = ctmp_create();
                        inst->dest->type = p->ext.type;
                        inst->src1 = diff->dest;
                        inst->src2 = copr_create();
                        inst->src2->kind = IMM;
                        inst->src2->info.imm = calc_size(et);
                    }
                    else
                    {
                        diff->op = MUL;
                        diff->dest = copr_create();
                        diff->dest->kind = TMP;
                        diff->dest->info.var = ctmp_create();
                        diff->dest->type = p->chd->next->ext.type;
                        diff->src1 = rhs;
                        diff->src2 = copr_create();
                        diff->src2->kind = IMM;
                        diff->src2->info.imm = calc_size(et);

                        inst->op = SUB;
                        inst->dest = copr_create();
                        inst->dest->kind = TMP;
                        inst->dest->info.var = ctmp_create();
                        inst->dest->type = p->ext.type;
                        inst->src1 = lhs;
                        inst->src2 = diff->dest;
                    }
                    cblock_append(*cur, diff);
                    cblock_append(*cur, inst);
                    return inst->dest;
                }
                else if (op == '&' && !p->chd->next)
                {
                    ssa_exp_(p->chd, cur, inst, succ);
                    if (inst->op == MOVE)
                    {
                        inst->op = ADDR;
                        inst->src1 = inst->dest;
                    }
                    else
                    {
                        inst->op = ADD;
                        inst->src1 = inst->dest;
                    }
                    inst->dest = copr_create();
                    inst->dest->kind = TMP;
                    inst->dest->info.var = ctmp_create();
                    inst->dest->type = p->ext.type;
                    cblock_append(*cur, inst);
                    return inst->dest;
                }
                else
                {
                    int unary = 0;
                    inst->op = (unsigned)-1;
                    switch (op)
                    {
                        case ASS_MUL: inst->op = MUL; break;
                        case ASS_DIV: inst->op = DIV; break;
                        case ASS_MOD: inst->op = MOD; break;
                        case ASS_ADD: inst->op = ADD; break;
                        case ASS_SUB: inst->op = SUB; break;
                        case ASS_SHL: inst->op = SHL; break;
                        case ASS_SHR: inst->op = SHR; break;
                        case ASS_AND: inst->op = AND; break;
                        case ASS_XOR: inst->op = XOR; break;
                        case ASS_OR:  inst->op = OR; break;
                        case OPT_INC: inst->op = ADD; unary = 1; break;
                        case OPT_DEC: inst->op = SUB; unary = 1; break;
                    }
                    if (inst->op != (unsigned)-1)
                    {
                        CInst_t tins = cinst_create();
                        ssa_exp_(p->chd, cur, tins, succ);                 /* as lval */
                        inst->src1 = ssa_exp_(p->chd, cur, NULL, succ);    /* as rval */
                        if (unary)
                        {
                            inst->src2 = copr_create();
                            inst->src2->kind = IMM;
                            inst->src2->info.imm = 1;
                        }
                        else
                            inst->src2 = ssa_exp_(p->chd->next, cur, NULL, succ);
                        if (tins->op == MOVE)
                        {
                            inst->dest = tins->dest;
                            cblock_append(*cur, inst);
                            free(tins);
                            return inst->dest;
                        }
                        else
                        {
                            CInst_t tins2 = cinst_create();
                            inst->dest = copr_create();
                            inst->dest->kind = TMP;
                            inst->dest->info.var = ctmp_create();
                            inst->dest->type = p->ext.type;
                            tins->src1 = inst->dest;
                            tins2->op = ARR;
                            tins2->src1 = tins->dest;    /* base */
                            tins2->src2 = tins->src2;    /* displacement */
                            tins2->dest = copr_create();
                            tins2->dest->kind = TMP;
                            tins2->dest->info.var = ctmp_create();
                            tins2->dest->type = p->ext.type;
                            cblock_append(*cur, inst);
                            cblock_append(*cur, tins);
                            cblock_append(*cur, tins2);
                            return tins2->dest;
                        }
                    }
                }

                switch (op)
                {
                    case EXP_CAST:
                        {
                            res = ssa_exp_(p->chd->next, cur, lval, succ);
                            res->type = p->ext.type;
                            free(inst);
                            return res;
                        }
                    case EXP_POSTFIX:
                        free(inst);
                        return ssa_postfix(p, cur, lval, succ);
                    /* KW_SIZEOF is eliminated during semantic checking */
                    default:
                        {
                            COpr_t lhs = ssa_exp_(p->chd, cur, lval, succ),
                                   rhs = NULL;
                            if (p->chd->next)
                                rhs = ssa_exp_(p->chd->next, cur, lval, succ);

                            inst->src1 = lhs;
                            inst->src2 = rhs;
                            switch (op)
                            {
                                case OPT_SHL: inst->op = SHL; break;
                                case OPT_SHR: inst->op = SHR; break;
                                case '|': inst->op = OR; break;
                                case '^': inst->op = XOR; break;
                                case OPT_EQ: inst->op = EQ; break;
                                case OPT_NE: inst->op = NE; break;
                                case '<': inst->op = LT; break;
                                case '>': inst->op = GT; break;
                                case OPT_LE: inst->op = LE; break;
                                case OPT_GE: inst->op = GE; break;
                                case '/': inst->op = DIV; break;
                                case '%': inst->op = MOD; break;
                                case '*':
                                          inst->op = MUL;
                                          break;
                                case '&':
                                          inst->op = AND;
                                          break;
                                case '+':
                                          if (p->chd->next)
                                              inst->op = ADD;
                                          else res = lhs;
                                          break;
                                case '-':
                                          if (p->chd->next)
                                              inst->op = SUB;
                                          else
                                          {
                                              inst->op = NEG;
                                              inst->src1 = lhs;
                                          }
                                          break;
                                case '~':
                                          inst->op = NOR;
                                          inst->src1 = lhs;
                                          inst->src2 = copr_create();
                                          inst->src2->kind = IMM;
                                          inst->src2->info.imm = 0;
                                          break;
                                case '!':
                                          inst->op = EQ;
                                          inst->src1 = lhs;
                                          inst->src2 = copr_create();
                                          inst->src2->kind = IMM;
                                          inst->src2->info.imm = 0;
                                          break;
                                default:
                                          auto_dest = 0;
                            }
                            if (rec)
                            {
                                if (auto_dest)
                                {
                                    inst->dest = copr_create();
                                    inst->dest->kind = TMP;
                                    inst->dest->info.var = ctmp_create();
                                    inst->dest->type = p->ext.type;
                                }
                                cblock_append(*cur, inst);
                                res = inst->dest;
                            }
                        }
                }
            }
    }
    return res;
}/*}}}*/

COpr_t ssa_exp(CNode *p, CBlock_t *cur, int discard_last) {
    CBlock_t succ = cblock_create(0);
    COpr_t res = ssa_exp_(p, cur, NULL, succ);
    CInst_t last; 
    {
        CInst_t head = succ->insts, t;
        while (head->next != head)
        {
            t = head->next;
            cblock_popfront(succ);
            cblock_append(*cur, t);
        }
        free(succ);
    }
    last = cblock_getback(*cur);
    if (discard_last && last 
                    && last->dest->kind == TMP
                    && last->op != CALL) /* temporary not used */
    {
        ctmp_destroy(last->dest->info.var);
        cblock_popback(*cur);
        free(last);
    }
    return res;
}

CBlock_t ssa_stmt(CNode *, CBlock_t, CBlock_t);
CBlock_t ssa_while(CNode *p, CBlock_t cur) {/*{{{*/
    CNode *exp = p->chd;
    CBlock_t loop_h = cblock_create(1), loop_t,
             cond_h= cblock_create(1), cond_t = cond_h,
             next_blk = cblock_create(1);
    CInst_t j_inst = cinst_create();
    COpr_t e = ssa_exp(exp, &cond_t, 0);
    compress_branch(e, cond_t, 0)->dest->info.imm = loop_h->id + gbbase;
    loop_h->ref = 1;

    DBLINK(cond_t, next_blk);
    loop_t = ssa_stmt(exp->next, loop_h, next_blk);

    j_inst->op = GOTO;
    j_inst->dest = copr_create();
    j_inst->dest->kind = IMM;
    j_inst->dest->info.imm = cond_h->id + gbbase;
    cond_h->ref = 1;
    cblock_append(cur, j_inst);

    cfg_add_edge(cur, cond_h);
    cfg_add_edge(cond_t, loop_h);
    cfg_add_edge(loop_t, cond_h);
    cfg_add_edge(cond_t, next_blk);

    DBLINK(cur, loop_h);
    DBLINK(loop_t, cond_h);

    return next_blk;
}/*}}}*/

CBlock_t ssa_for(CNode *p, CBlock_t cur) {/*{{{*/
    CNode *exp1 = p->chd,
          *exp2 = exp1->next,
          *exp3 = exp2->next;
    CBlock_t loop_h = cblock_create(1), loop_t,
             cond_h = cblock_create(1), cond_t = cond_h,
             next_blk = cblock_create(1);
    CInst_t j_inst = cinst_create();
    COpr_t e = ssa_exp(exp2, &cond_t, 0);
    compress_branch(e, cond_t, 0)->dest->info.imm = loop_h->id + gbbase;
    loop_h->ref = 1;

    DBLINK(cond_t, next_blk);
    loop_t = ssa_stmt(exp3->next, loop_h, next_blk);

    ssa_exp(exp1, &cur, 1);
    ssa_exp(exp3, &loop_t, 1);

    j_inst->op = GOTO;
    j_inst->dest = copr_create();
    j_inst->dest->kind = IMM;
    j_inst->dest->info.imm = cond_h->id + gbbase;
    cond_h->ref = 1;
    cblock_append(cur, j_inst);

    cfg_add_edge(cur, cond_h);
    cfg_add_edge(cond_t, loop_h);
    cfg_add_edge(loop_t, cond_h);
    cfg_add_edge(cond_t, next_blk);

    DBLINK(cur, loop_h);
    DBLINK(loop_t, cond_h);

    return next_blk;
}/*}}}*/

CBlock_t ssa_if(CNode *p, CBlock_t cur, CBlock_t loop_exit) {/*{{{*/
    CNode *body1 = p->chd->next,
          *body2 = body1->next;
    CBlock_t then_blk, then_t, next_blk,
             else_blk, else_t;
    CInst_t if_inst; /* = cinst_create(); */
    COpr_t rt = ssa_exp(p->chd, &cur, 0);
    if (rt->kind == IMM)
    {
        if (rt->info.imm)
            return ssa_stmt(body1, cur, loop_exit);
        else if (body2->type != NOP)
            return ssa_stmt(body2, cur, loop_exit);
        else
            return cur;
    }
    then_blk = cblock_create(1);
    if_inst = compress_branch(rt, cur, 1);

    cfg_add_edge(cur, then_blk);
    DBLINK(cur, then_blk);
    then_t = ssa_stmt(body1, then_blk, loop_exit);
    if (body2->type != NOP)
    {
        CInst_t j_inst = cinst_create();
        j_inst->op = GOTO;
        j_inst->dest = copr_create();
        j_inst->dest->kind = IMM;

        else_blk = cblock_create(1);
        if_inst->dest->info.imm = else_blk->id + gbbase;
        else_blk->ref = 1;
        DBLINK(then_t, else_blk);
        else_t = ssa_stmt(body2, else_blk, loop_exit);
        if (cblock_isempty(else_t))
            next_blk = else_t;
        else
        {
            next_blk = cblock_create(1);
            DBLINK(else_t, next_blk);
            cfg_add_edge(else_t, next_blk);
        }

        j_inst->dest->info.imm = next_blk->id + gbbase;
        next_blk->ref = 1;
        cblock_append(then_t, j_inst);

        cfg_add_edge(cur, else_blk);
        cfg_add_edge(then_t, next_blk);
    }
    else
    {
        if (cblock_isempty(then_t))
            next_blk = then_t;
        else
        {
            next_blk = cblock_create(1);
            DBLINK(then_t, next_blk);
            cfg_add_edge(then_t, next_blk);
        }
        cfg_add_edge(cur, next_blk);
        if_inst->dest->info.imm = next_blk->id + gbbase;
        next_blk->ref = 1;
    }
    return next_blk;
}/*}}}*/

CBlock_t ssa_ret(CNode *p, CBlock_t cur) {
    CInst_t inst = cinst_create();
    inst->op = RET;
    if (p->chd->type != NOP)
        inst->src1 = ssa_exp(p->chd, &cur, 0);
    cblock_append(cur, inst);
    return cur;
}

CBlock_t ssa_break(CBlock_t cur, CBlock_t loop_exit) {
    CInst_t inst = cinst_create();
    assert(loop_exit);
    inst->op = GOTO;
    inst->dest = copr_create();
    inst->dest->kind = IMM;
    inst->dest->info.imm = loop_exit->id + gbbase;
    loop_exit->ref = 1;
    cblock_append(cur, inst);
    cfg_add_edge(cur, loop_exit);
    return cur;
}

CBlock_t ssa_cont(CBlock_t cur, CBlock_t loop_exit) {
    CInst_t inst = cinst_create();
    assert(loop_exit);
    loop_exit = loop_exit->prev;    /* loop cond */
    inst->op = GOTO;
    inst->dest = copr_create();
    inst->dest->kind = IMM;
    inst->dest->info.imm = loop_exit->id + gbbase;
    loop_exit->ref = 1;
    cblock_append(cur, inst);
    cfg_add_edge(cur, loop_exit);
    return cur;
}

CBlock_t ssa_comp(CNode *, CBlock_t, CBlock_t loop_exit);
CBlock_t ssa_stmt(CNode *p, CBlock_t cur, CBlock_t loop_exit) {
    switch (p->rec.subtype)
    {
        case STMT_EXP:
            ssa_exp(p->chd, &cur, 1);
            break;
        case STMT_COMP:
            cur = ssa_comp(p, cur, loop_exit);
            break;
        case STMT_IF:
            return ssa_if(p, cur, loop_exit);
        case STMT_FOR:
            return ssa_for(p, cur);
        case STMT_WHILE:
            return ssa_while(p, cur);
        case STMT_CONT:
            return ssa_cont(cur, loop_exit);
        case STMT_BREAK:
            return ssa_break(cur, loop_exit);
        case STMT_RET:
            return ssa_ret(p, cur);
    }
    return cur;
}

CBlock_t ssa_comp(CNode *p, CBlock_t cur, CBlock_t loop_exit) {
    CNode *decls = p->chd,
          *stmts = p->chd->next, *i;
    if (decls->chd->type != NOP)
    {
        CVList_t a;
        for (a = p->ext.autos; a; a = a->next)
        {
            CNode *initr = a->var->initr;
            CInst_t last, inst;
            if (!initr) continue;
            assert(initr->rec.subtype == INITR_NORM);
            inst = cinst_create();
            inst->src1 = ssa_exp(initr->chd, &cur, 0);
            last = cblock_getback(cur);
            if (last && last->dest->kind == TMP)
            {
                last->dest->kind = VAR;
                free(last->dest->info.var);
                free(inst);
                last->dest->info.var = a->var;
                last->dest->type = a->var->type;
            }
            else
            {
                inst->op = MOVE;
                inst->dest = copr_create();
                inst->dest->kind = VAR;
                inst->dest->info.var = a->var;
                inst->dest->type = a->var->type;
                cblock_append(cur, inst);
            }
        }
    }
    if (stmts->chd->type != NOP)
        for (i = stmts->chd; i; i = i->next)
            cur = ssa_stmt(i, cur, loop_exit);
    return cur;
}

CPSet_t cpset_create(void) {
    CPSet_t res = NEW(CPSet);
    memset(res->head, 0, sizeof res->head);
    return res;
}

void cpset_destroy(CPSet_t cps) {
    int i;
    for (i = 0; i < MAX_TABLE_SIZE; i++)
    {
        CPNode *p, *np;
        for (p = cps->head[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
    }
    free(cps);
}

int cpset_insert(CPSet_t cps, long key) {
    unsigned int hv = key % MAX_TABLE_SIZE;
    CPNode *p = cps->head[hv], *np;
    for (; p; p = p->next)
        if (p->key == key)
            return 0;
    np = NEW(CPNode);
    np->key = key;
    np->next = cps->head[hv];
    cps->head[hv] = np;
    return 1;
}

void cpset_erase(CPSet_t cps, long key) {
    unsigned int hv = key % MAX_TABLE_SIZE;
    int flag = 0;
    CPNode *p = cps->head[hv], *pp = NULL;
    for (; p; pp = p, p = p->next)
        if (p->key == key)
        {
            flag = 1;
            break;
        }
    if (!flag) return;
    if (pp)
        pp->next = p->next;
    else
        cps->head[hv] = p->next;
    free(p);
}

int cpset_belongs(CPSet_t cps, long key) {
    unsigned int hv = key % MAX_TABLE_SIZE;
    CPNode *p = cps->head[hv];
    for (; p; p = p->next)
        if (p->key == key)
            return 1;
    return 0;
}

int dom[MAX_BLOCK], ord[MAX_BLOCK], vis[MAX_BLOCK], par[MAX_BLOCK], ocnt;
int loop_tail[MAX_BLOCK];
CPSet_t dfset[MAX_BLOCK], phi[MAX_BLOCK];
CBList_t df[MAX_BLOCK];

void dfs(CBlock_t u, int v) {
    CEdge *e;
    par[u->id] = v;
    vis[u->id] = -2;
    for (e = cfg.head[u->id]; e; e = e->next)
    {
        CBlock_t v = e->to;
        if (vis[v->id] == -1)
            dfs(v, u->id);
        else if (vis[v->id] == -2)
            loop_tail[v->id] = u->id;
    }
    vis[u->id] = ocnt;
    ord[ocnt++] = u->id;
}

int intersect(int b1, int b2) {
    while (b1 != b2)
        if (b1 < b2) b1 = dom[b1];
        else b2 = dom[b2];
    return b1;
}

void calc_dominant_frontier(void) {
    int i;
    int ch = 1;
    ocnt = 0;
    memset(vis, -1, sizeof vis);
    memset(dom, -1, sizeof dom);
    memset(loop_tail, -1, sizeof loop_tail);
    dfs(entry, -1);
    dom[vis[entry->id]] = vis[entry->id];
    while (ch)
    {
        int i;
        ch = 0;
        for (i = bcnt - 2; i >= 0; i--)
        {
            int id = ord[i];
            CEdge *e = cfg.rhead[id];
            int new_idom = vis[par[id]];
            for (; e; e = e->next)
            {
                int p = e->to->id;
                if (vis[p] == new_idom) continue;
                if (dom[vis[p]] != -1)
                    new_idom = intersect(vis[p], new_idom);
            }
            if (dom[i] != new_idom)
            {
                ch = 1;
                dom[i] = new_idom;
            }
        }
    }
    for (i = 0; i < bcnt; i++)
        dfset[i] = cpset_create();
    for (i = 0; i < bcnt; i++)
        if (cfg.rhead[i] && cfg.rhead[i]->next)
        {
            CEdge *p = cfg.rhead[i];
            for (; p; p = p->next)
            {
                int runner = p->to->id;
                while (vis[runner] != dom[vis[i]])
                {
                    if (!cpset_belongs(dfset[runner], i))
                    {
                        CBList_t np = NEW(CBList);
                        np->cblk = blks[i];
                        np->next = df[runner];
                        cpset_insert(dfset[runner], i);
                        df[runner] = np;
                    }
                    runner = ord[dom[vis[runner]]];
                }
            }
        }
    for (i = 1; i < bcnt; i++)
        dtree_add_edge(blks[ord[dom[vis[i]]]], blks[i]);
}

void insert_phi(CVList_t vars) {
    CVList_t vp;
    int i;
    for (i = 0; i < bcnt; i++)
        phi[i] = cpset_create();
    for (vp = vars; vp; vp = vp->next)
    { /* for each variable */
        CVar_t var = vp->var;
        CBList_t t = var->defsite;
        CBList_t def = NULL;
        static int indef[MAX_BLOCK];
#ifdef CIBIC_DEBUG
        fprintf(stderr, "%s:", var->name);
#endif
        for (t = var->defsite; t; t = t->next)
            if (++indef[t->cblk->id] == 1)
            {
                CBList_t p = NEW(CBList);
                p->cblk = t->cblk;
                p->next = def;
                def = p;
            }
        for (t = var->defsite; t; t = t->next)
            indef[t->cblk->id] = 0; /* clear */
#ifdef CIBIC_DEBUG
        for (t = def; t; t = t->next)
            fprintf(stderr, " %d", t->cblk->id);
        fprintf(stderr, "\n");
#endif
        while (def) /* while def not empty */
        {
            CBList_t n = def, i; /* remove some node n from def */
            def = def->next;
            for (i = df[n->cblk->id]; i; i = i->next)
            {
                CBlock_t y = i->cblk;
                CPSet_t phiy = phi[y->id];
                if (!cpset_belongs(phiy, (long)var))
                {
                    CPhi_t phi = NEW(CPhi);
                    CBList_t ndef;
                    phi->dest = copr_create();
                    phi->dest->kind = VAR;
                    phi->dest->info.var = var;
                    phi->dest->type = var->type;
                    phi->oprs = (COpr_t *)malloc(sizeof(COpr_t) * y->pred);
                    cblock_pappend(y, phi); 
                    cpset_insert(phiy, (long)var);
                    ndef = NEW(CBList);
                    ndef->cblk = y;
                    ndef->next = def;
                    def = ndef;
                }
            }
        }
    }
    for (i = 0; i < bcnt; i++)
    {
        CBList_t p, np;
        for (p = df[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
        df[i] = NULL;
        if (phi[i]) cpset_destroy(phi[i]);
        if (dfset[i]) cpset_destroy(dfset[i]);
    }
}

void renaming_dfs(CBlock_t blk) {
    CInst_t ih = blk->insts, i;
    CPhi_t ph = blk->phis, pi;
    CEdge *e = cfg.head[blk->id];
    COList_t defl = NULL, dn;
    for (pi = ph->next; pi != ph; pi = pi->next)
    {
        COpr_t dest = pi->dest;
        CVar_t var = dest->info.var;
        COList_t n = NEW(COList), n2;
        dest->sub = var->cnt++;
        dest->def = ih->next; /* the first inst */ 
        dest->range = NULL;
        n->opr = dest;
        n->next = var->stack;
        var->stack = n;
        n2 = NEW(COList);
        n2->opr = dest;
        n2->next = defl;
        defl = n2;
    }
    for (i = ih->next; i != ih; i = i->next)
    {
        COpr_t dest = i->dest;
        COpr_t *opr[3] = {NULL, &(i->src1), &(i->src2)};
        int t;
        if (i->op == WARR)
            opr[0] = &(i->dest);
        for (t = 0; t < 3; t++)
        {
            COpr_t p;
            if (!opr[t]) continue;
            p = *(opr[t]);
            if (!(p && (p->kind == VAR || p->kind == TMP))) continue;
            /* free(p); */  /* memory leak */
            (*opr[t] = p->info.var->stack->opr)->type = p->type;
            (*opr[t])->info.var->weight++;
        }
        if (dest)
        {
            if (i->op == WARR)
                i->dest = dest->info.var->stack->opr;
            else
            {
                if (dest->kind == VAR || dest->kind == TMP)
                {
                    CVar_t var = dest->info.var;
                    COList_t n = NEW(COList), n2;
                    dest->sub = var->cnt++;
                    dest->def = i;
                    dest->range = NULL;
                    n->opr = dest;
                    n->next = var->stack;
                    var->stack = n;
                    n2 = NEW(COList);
                    n2->opr = dest;
                    n2->next = defl;
                    defl = n2;
                }
            }
        }
    }
    for (; e; e = e->next) /* for each successor */
    {
        CBlock_t y = e->to;
        int j = 0;
        CEdge *pre = cfg.rhead[y->id];
        for (; pre->to != blk; pre = pre->next) j++;
        ph = y->phis;
        for (pi = ph->next; pi != ph; pi = pi->next)
        {
            if (pi->dest->kind == VAR)
                pi->oprs[j] = pi->dest->info.var->stack->opr;
            pi->oprs[j]->dep = 1;
        }
    }
    for (e = dtree.head[blk->id]; e; e = e->next)
        renaming_dfs(e->to);
    for (; defl; defl = dn)
    {
        CVar_t var = defl->opr->info.var;
        COList_t nf = var->stack->next;
        free(var->stack);
        var->stack = nf;
        dn = defl->next;
        free(defl);
    }
}

void renaming_vars(COList_t oprs) {
    COList_t p;
    for (p = oprs; p; p = p->next)
        if (p->opr->kind == VAR)
        {
            CInst_t ld = cinst_create();
            CVar_t var = p->opr->info.var;
            var->cnt = 0;
            var->reload = var->loc > 0 && var->type->type != CARR;
            ld->op = LOAD;
            ld->dest = copr_create();
            ld->dest->kind = VAR;
            ld->dest->info.var = var;
            ld->dest->type = var->type;
            cblock_pushfront(entry, ld);
        }
    renaming_dfs(entry);
}

void mark_insts(void) {
    int i, icnt = 0;
    for (i = bcnt - 1; i >= 0; i--)
    {
        CBlock_t b = blks[ord[i]];
        CInst_t ih = b->insts, ii;
        CPhi_t ph = b->phis, pi;
        if (cblock_isempty(b))
            b->first = b->last = icnt++;
        else
        {
            for (pi = ph->next; pi != ph; pi = pi->next)
                icnt++;
            for (ii = ih->next; ii != ih; ii = ii->next)
                ii->id = icnt++;
            b->first = ih->next->id;
            b->last = ih->prev->id;
        }
    }
}

CPSet_t liveset[MAX_BLOCK];
COList_t live[MAX_BLOCK];

CRange_t crange_merge(CRange_t a, CRange_t b) {
    CRange res;
    CRange_t tail = &res;
    res.next = NULL;
    res.r = -1;
    for (; a || b;)
    {
        if (a && (!b || (a->l < b->l || (a->l == b->l && a->r < b->r))))
        {
            if (tail->r >= a->l)
            {
                if (a->r > tail->r)
                    tail->r = a->r;
            }
            else
            {
                assert(tail->r < a->l);
                tail->next = a;
                tail = a;
            }
            a = a->next;
        }
        else
        {
            if (tail->r >= b->l)
            {
                if (b->r > tail->r)
                    tail->r = b->r;
            }
            else
            {
                assert(tail->r < b->l);
                tail->next = b;
                tail = b;
            }
            b = b->next;
        }
    }
    tail->next = NULL;
    return res.next;
}

void add_range_(COpr_t opr, int begin, int end) {
    CRange_t range;
    range = NEW(CRange);
    range->l = begin;
    range->r = end;
    range->next = NULL;
    opr->range = crange_merge(opr->range, range);
}

void add_range(COpr_t opr, CBlock_t blk, int end) {
    int dfid = opr->def->id;
    int begin;
    if (blk->first <= dfid && dfid <= blk->last)
        begin = dfid;
    else
        begin = blk->first;
    add_range_(opr, begin, end);
}

void build_intervals(void) {
    int i;
    for (i = 0; i < bcnt; i++)
        liveset[i] = cpset_create();
    for (i = 0; i < bcnt; i++)
    {
        int id = ord[i];
        CBlock_t b = blks[id];
        CEdge *e = cfg.head[id]; 
        CPSet_t curlive = liveset[id];
        for (; e; e = e->next)
        {
            int sid = e->to->id;
            CBlock_t s = blks[sid];
            COList_t p = live[sid];
            CPhi_t ph = s->phis, i;
            for (i = ph->prev; i != ph; i = i->prev)
            {
                CEdge *pe;
                int t;
                for (t = 0, pe = cfg.rhead[sid]; pe->to != b; pe = pe->next) t++;
                COpr_t opr = i->oprs[t];
                if (opr && 
                        (opr->kind == VAR ||
                         opr->kind == TMP) && !cpset_belongs(curlive, (long)opr))
                {
                    COList_t np = NEW(COList);
                    np->opr = opr;
                    np->next = live[id];
                    live[id] = np;
                    cpset_insert(curlive, (long)opr);
                }
            }
            for (; p; p = p->next)
                if (cpset_belongs(liveset[sid], (long)p->opr) &&
                        cpset_insert(curlive, (long)p->opr))
                {
                    COList_t np = NEW(COList);
                    np->opr = p->opr;
                    np->next = live[id];
                    live[id] = np;
                }
        }
        {
            COList_t p;
            for (p = live[id]; p; p = p->next) 
                add_range(p->opr, b, b->last + 1);
        }
        {
            CInst_t ih = b->insts, i;
            for (i = ih->prev; i != ih; i = i->prev)
            {
                int t;
                COpr_t oprs[3] = {NULL, i->src1, i->src2};
                if (i->dest &&
                    (i->dest->kind == VAR ||
                     i->dest->kind == TMP) && i->op != WARR) /* def */
                {
                    i->is_def = 1;
                    cpset_erase(curlive, (long)i->dest);
                }
                else
                {
                    if (i->op == WARR)
                        oprs[0] = i->dest;
                    i->is_def = 0;
                }
                for (t = 0; t < 3; t++)
                {
                    COpr_t opr = oprs[t];
                    if (opr && 
                        (opr->kind == VAR ||
                         opr->kind == TMP) && !cpset_belongs(curlive, (long)opr))
                    {
                        COList_t np = NEW(COList);
                        np->opr = opr;
                        np->next = live[id];
                        live[id] = np;
                        cpset_insert(curlive, (long)opr);
                        add_range(opr, b, i->id);
                    }
                }
            }
        }
        {
            CPhi_t ph = b->phis, i;
            for (i = ph->prev; i != ph; i = i->prev)
                cpset_erase(curlive, (long)i->dest);
        }
        if (loop_tail[id] != -1)
        {
            COList_t p;
            for (p = live[id]; p; p = p->next) 
                if (cpset_belongs(curlive, (long)p->opr))
                    add_range_(p->opr, b->first, blks[loop_tail[id]]->last + 1);
        }
    }
    for (i = 0; i < bcnt; i++)
    {
        COList_t p = live[i], np;
        for (; p; p = np)
        {
            np = p->next;
            free(p);
        }
        live[i] = NULL;
        if (liveset[i]) cpset_destroy(liveset[i]);
    }
}

COpr_t cinterv_repr(COpr_t opr) {
    return opr->par == opr ? opr : (opr->par = cinterv_repr(opr->par));
}

void cinterv_union(COpr_t a, COpr_t b) {
    a = cinterv_repr(a);
    b = cinterv_repr(b);
    fprintf(stderr, "merging ");
    copr_print(stderr, a);
    fprintf(stderr, " ");
    copr_print(stderr, b);
    fprintf(stderr, "\n");
    if (a == b) return;
    b->range = crange_merge(b->range, a->range);
    a->par = b;
    b->mod |= a->mod;
}

void init_def(void) {
    CBlock_t p;
    COList_t def;
    raw_defs = NULL;
    for (p = entry; p; p = p->next)
    {
        CInst_t i, ih = p->insts;
        CPhi_t pi, ph = p->phis;
        for (i = ih->next; i != ih; i = i->next)
            if (i->is_def)
            {
                def = NEW(COList);
                def->opr = i->dest;
                def->next = raw_defs;
                raw_defs = def;
            }
        for (pi = ph->next; pi != ph; pi = pi->next)
        {
            def = NEW(COList);
            def->opr = pi->dest;
            def->next = raw_defs;
            raw_defs = def;
        }
    }
    for (p = entry; p; p = p->next)
    {
        CPhi_t pi, ph = p->phis;
        for (pi = ph->next; pi != ph; pi = pi->next)
        {
            int i;
            for (i = 0; i < p->pred; i++)
                cinterv_union(pi->dest, pi->oprs[i]);
        }
    }
}

void print_intervals(void) {
    COList_t d;
    for (d = raw_defs; d; d = d->next)
    {
        COpr_t opr = d->opr,
               repr = cinterv_repr(opr);
        CRange_t p;
        copr_print(stderr, opr);
        fprintf(stderr, ": ");
        if (repr == opr)
        {
            for (p = opr->range; p; p = p->next)
                fprintf(stderr, "[%d, %d)", p->l, p->r);
        }
        else copr_print(stderr, repr);
        fprintf(stderr, "\n");
    }
}

void colist_remove(COList_t node) {
    node->prev->next = node->next;
    node->next->prev = node->prev;
}

void colist_add(COList_t head, COList_t p) {
    (p->next = head->next)->prev = p;
    (p->prev = head)->next = p;
}

int overlap_with_beg(COpr_t i, int beg) {
    CRange_t r;
    for (r = i->range; r && r->l <= beg; r = r->next)
        if (r->r > beg) return 1;
    return 0;
}

int overlap_with_interv(COpr_t i, COpr_t cur) {
    CRange_t pi, pc;
    for (pi = i->range, pc = cur->range; pi && pc;)
    {
        if (pi->r <= pc->l) pi = pi->next;
        else if (pc->r <= pi->l) pc = pc->next;
        else return 1;
    }
    return 0;
}

int copr_comp(const void *a, const void *b) {
    return (*(COpr_t *)a)->range->l - (*(COpr_t *)b)->range->l;
}

const int avail_regs[] = {8, 9, 10, 11, 12, 13, 14, 15, 16, 24, 25};
const int MAX_AVAIL_REGS = sizeof(avail_regs) / sizeof(avail_regs[0]);

void register_alloc(void) {
    /* Algorithm from the paper:
     * Linear Scan Register Allocation
     * in the Context of SSA Form and Register Constraints */
    static int freg[32], f[32];
    int dn = 0, i;
    COpr_t *unhandled;
    COList_t p;
    COList_t active = NEW(COList),
             inactive = NEW(COList);
    active->next = active->prev = active;
    inactive->next = inactive->prev = inactive;
    memset(freg, -1, sizeof freg);
    init_def();
    for (i = 0; i < MAX_AVAIL_REGS; i++)
        freg[avail_regs[i]] = 1; /* available */
    for (p = raw_defs; p; p = p->next)
    {
        COpr_t opr = p->opr;
        /*
        if (opr->info.var->loc < 0)
        {
            opr->reg = 3 - opr->info.var->loc;
            continue;
        } */ /* arguments */
        opr->reg = -2;
        if (opr->par != opr) continue;
        if (cinterv_repr(opr)->range)
        {
            opr->reg = -1;
            dn++;
        }
    }
    unhandled = (COpr_t *)malloc(dn * sizeof(COpr_t));
    i = 0;
    for (p = raw_defs; p; p = p->next)
        if (p->opr->reg == -1)
            unhandled[i++] = p->opr;
    for (i = 0; i < dn; i++)
    {
        COpr_t opr = unhandled[i];
        CRange_t r;
        /*
        if (opr->kind == VAR && opr->range->next)
        {
            free(opr->range);
            opr->range = opr->range->next;
        }*/   /* discard uncessary load */
        for (r = opr->range; r->next; r = r->next);
        opr->begin = opr->range->l;
        opr->end = r->r;
        copr_print(stderr, opr);
        fprintf(stderr, " (key: %d begin: %d, end: %d, weight: %d)\n",
                opr->range->l, opr->begin, opr->end, opr->info.var->weight);
    }
    qsort(unhandled, dn, sizeof(COpr_t), copr_comp);
    print_intervals();
    /* preparation done */
    for (i = 0; i < dn; i++)
    {
        COList_t c = NEW(COList);
        COpr_t cur;
        COList_t np;
        int reg, t;
        cur = c->opr = unhandled[i];
        /* for each interval in active */
        for (p = active->next; p != active; p = np)
        {
            COpr_t i = p->opr;
            np = p->next;
            if (i->end <= cur->begin) /* if i ends before cur.beg */
            {
                colist_remove(p);
                free(p);            /* move i to handled */
                freg[i->reg] = 1;   /* add i.reg to free */
            }
            else if (!overlap_with_beg(i, cur->begin))
            {
                colist_remove(p); 
                colist_add(inactive, p); /* move i to inactive */
                freg[i->reg] = 1;        /* add i.reg to free */
            }
        }
        /* for each interval i in inactive */
        for (p = inactive->next; p != inactive; p = np)
        {
            COpr_t i = p->opr;
            np = p->next;
            if (i->end <= cur->begin)   /* if i ends before cur.beg */
            {
                colist_remove(p);
                free(p);                /* move i to handled */
            }
            else if (overlap_with_beg(i, cur->begin))
            {
                colist_remove(p);
                colist_add(active, p);  /* move i to active */
                freg[i->reg] = 0;       /* remove i.reg from free */
            }
        }
        memmove(f, freg, sizeof f);
        /* for each interval i in inactive that overlaps cur do
         * f <- f - {i.reg} */
        for (p = inactive->next; p != inactive; p = p->next)
            if (overlap_with_interv(p->opr, cur))
                f[p->opr->reg] = 0;
        reg = -1;
        for (t = 0; t < 32; t++)
            if (f[t] > 0) { reg = t; break; }
        if (reg == -1) /* if f = {} */
        { /* assign mem loc */
            static int w[32];
            int min = 0x7fffffff;
            memset(w, 0, sizeof w);
            for (p = active->next; p != active; p = p->next)
                if (overlap_with_interv(p->opr, cur))
                    w[p->opr->reg] += p->opr->info.var->weight;
            for (p = inactive->next; p != inactive; p = p->next)
                if (overlap_with_interv(p->opr, cur))
                    w[p->opr->reg] += p->opr->info.var->weight;
            for (t = 0; t < 32; t++)
                if (f[t] != -1 && w[t] < min) min = t, reg = t;
            if (reg == -1 || cur->info.var->weight < w[reg])
            {
                cur->reg = -1;      /* assign a memory location to cur */
                free(c);            /* and move cur to handled */
            }
            else
            {
                /* move all active or inactive intervals to which r was assigned to handled
                 * assign memory locations to them */
                for (p = active->next; p != active; p = np)
                {
                    np = p->next;
                    if (p->opr->reg == reg)
                    {
                        p->opr->reg = -1;
                        colist_remove(p);
                        free(p);
                    }
                }
                for (p = inactive->next; p != inactive; p = np)
                {
                    np = p->next;
                    if (p->opr->reg == reg)
                    {
                        p->opr->reg = -1;
                        colist_remove(p);
                        free(p);
                    }
                }
                cur->reg = reg;
                colist_add(active, c); /* move cur to active */
            }
        }
        else if (cur->mod)      /* may be referenced by a pointer */
        {
            cur->reg = -1;      /* assign a memory location to cur */
            free(c);            /* and move cur to handled */
        }
        else
        {
            cur->reg = reg; /* cur.reg <- any register in f */
            freg[reg] = 0;  /* free <- free - {cur.reg} */
            colist_add(active, c); /* move cur to active */
        }
    }
    for (i = 0; i < dn; i++)
    {
        COpr_t opr = unhandled[i];
        copr_print(stderr, opr);
        fprintf(stderr, " (begin: %d, end: %d, weight: %d, reg: %d)\n",
                opr->begin, opr->end, opr->info.var->weight, opr->reg);
    }
    for (p = raw_defs; p; p = p->next)
    {
        COpr_t opr = p->opr;
        opr->spill = cinterv_repr(opr);
        if (cinterv_repr(opr)->range)
            opr->reg = opr->spill->reg;
    }
    defs = NULL;
    for (i = 0; i < dn; i++)
    {
        COList_t p = NEW(COList);
        p->opr = unhandled[i];
        p->next = defs;
        defs = p;
    }
    free(unhandled);
}

void const_propagation(void) {
#define IS_STATIC(opr) (!((opr)->mod || (opr)->info.var->reload))
    int i;
    for (i = bcnt - 1; i >= 0; i--)
    {
        CBlock_t b = blks[vis[i]];
        CInst_t i, ni, ih = b->insts;
        for (i = ih->next; i != ih; i = ni)
        {
            int flag = 0;
            COpr_t t;
            if (i->op == ADDR)
                i->src1->mod = 1;
            if (i->src1 && i->src1->cval && IS_STATIC(i->src1))
            {
                t = i->src1->cval;
                i->src1 = copr_create();
                *(i->src1) = *t;
            }
            if (i->src2 && i->src2->cval && IS_STATIC(i->src2))
            {
                t = i->src2->cval;
                i->src2 = copr_create();
                *(i->src2) = *t;
            }
            if (!i->dest) 
            {
                ni = i->next;
                continue;
            }
            if (i->src1 && i->src1->kind == IMM)
            {
                if (i->op == MOVE)
                    flag = 1;
                else if (i->op == NEG)
                {
                    i->op = MOVE;
                    i->src1->info.imm *= -1;
                    flag = 1;
                }
                else if (i->src2 && i->src2->kind == IMM)
                {
                    COpr_t c;
                    int immd;
                    int imm1 = i->src1->info.imm;
                    int imm2 = i->src2->info.imm;
                    flag = 1;
                    switch (i->op)
                    {
                        case MUL: immd = imm1 * imm2; break;
                        case DIV: immd = imm1 / imm2; break;
                        case MOD: immd = imm1 % imm2; break;
                        case ADD: immd = imm1 + imm2; break;
                        case SUB: immd = imm1 - imm2; break;
                        case SHL: immd = imm1 << imm2; break;
                        case SHR: immd = imm1 >> imm2; break;
                        case AND: immd = imm1 & imm2; break;
                        case XOR: immd = imm1 ^ imm2; break;
                        case OR: immd = imm1 | imm2; break;
                        case NOR: immd = ~(imm1 | imm2); break;
                        case EQ: immd = imm1 == imm2; break;
                        case NE: immd = imm1 != imm2; break;
                        case LT: immd = imm1 < imm2; break;
                        case GT: immd = imm1 > imm2; break;
                        case LE: immd = imm1 <= imm2; break;
                        case GE: immd = imm1 >= imm2; break;
                        default: flag = 0;
                    }
                    if (flag)
                    {
                        c = copr_create();
                        c->kind = IMM;
                        free(i->src1);
                        free(i->src2);
                        i->op = MOVE;
                        i->src1 = c;
                        c->info.imm = immd;
                    }
                }
            }
            ni = i->next;
            if (flag)
            {
                i->dest->cval = i->src1;
                if (i->dest->kind == TMP && !i->dest->dep)
                {
                    i->next->prev = i->prev;
                    i->prev->next = i->next;
                    free(i);
                }
            }
        }
    }
}

void strength_reduction(void) {
#define SWAP_IMM \
    do { \
        if (i->src1->kind == IMM) \
        { \
            COpr_t t = i->src1; \
            i->src1 = i->src2; \
            i->src2 = t; \
        } \
    } while (0)

    int i;
    for (i = bcnt - 1; i >= 0; i--)
    {
        CBlock_t b = blks[vis[i]];
        CInst_t i, ni, ih = b->insts;
        for (i = ih->next; i != ih; i = ni)
        {
            ni = i->next;
            switch (i->op)
            {
                case ADD:
                    SWAP_IMM;
                    if (i->src2->kind == IMM && !i->src2->info.imm)
                    {
                        i->op = MOVE;
                        i->src2 = NULL;
                    }
                    break;
                case MUL:
                    SWAP_IMM;
                    if (i->src2->kind == IMM)
                    {
                        int p = 0, n = i->src2->info.imm;
                        while (!(n & 1)) n >>= 1, p++;
                        if (n == 1)
                        {
                            i->op = SHL;
                            i->src2->info.imm = p;
                        }
                    } 
                    break;

                default: ;
            }
        }
    }
}

unsigned int copr_hash(COpr_t opr) {
    if (!opr) return 0;
    switch (opr->kind)
    {
        case VAR:
        case TMP:
            return (unsigned long)opr;
        default:
            return (unsigned int)opr->info.imm;
    }
}

int copr_eq(COpr_t a, COpr_t b) {
    if (a->kind != b->kind) return 0;
    switch (a->kind)
    {
        case VAR:
        case TMP:
            return a == b;
        case IMM:
            return a->info.imm == b->info.imm;
        case IMMS:
            return a->info.cstr == b->info.cstr;
        case IMMF:
            return !strcmp(a->info.str, b->info.str);
        default:
            return 0;
    }
}

unsigned int cexpmap_hash(CInst_t exp) {
    unsigned int res = 0;
    res = ((unsigned int)exp->op) << 10;
    res ^= copr_hash(exp->src1);
    res = (res << 1) ^ copr_hash(exp->src2);
    return res;
}

CExpMap_t cexpmap_create(void) {
    CExpMap_t res = NEW(CExpMap);
    memset(res->head, 0, sizeof res->head);
    return res;
}

int cexpmap_comp(CInst_t exp1, CInst_t exp2) {
    if (exp1->op != exp2->op) return 0;
    if (!copr_eq(exp1->src1, exp2->src1)) return 0;
    return copr_eq(exp1->src2, exp2->src2);
}

void cexpmap_insert(CExpMap_t cem, CInst_t exp) {
    int hv = cexpmap_hash(exp) % MAX_TABLE_SIZE;
    CENode *np = NEW(CENode);
    np->exp = exp;
    np->next = cem->head[hv];
    cem->head[hv] = np;
}

CInst_t cexpmap_lookup(CExpMap_t cem, CInst_t exp) {
    int hv = cexpmap_hash(exp) % MAX_TABLE_SIZE;
    CENode *p;
    for (p = cem->head[hv]; p; p = p->next)
        if (cexpmap_comp(p->exp, exp))
            return p->exp;
    return NULL;
}

void cexpmap_clear(CExpMap_t cem) {
    int i;
    CENode *p, *np;
    for (i = 0; i < MAX_TABLE_SIZE; cem->head[i++] = NULL)
        for (p = cem->head[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
}

void cexpmap_destroy(CExpMap_t cem) {
    cexpmap_clear(cem);
    free(cem);
}
void copr_shortcut(COpr_t *opr) {
    COpr_t t = *opr;
    if (!t) return;
    t = t->same;
    if (t->kind == TMP)
        *opr = t->same;
}

void subexp_elimination(void) {
    int i;
    CExpMap_t cem = cexpmap_create();
    for (i = bcnt - 1; i >= 0; i--)
    {
        CBlock_t b = blks[vis[i]];
        CInst_t i, ih = b->insts;
        for (i = ih->next; i != ih; i = i->next)
        {
            CInst_t t;
            if (i->op == MOVE)
            {
                i->dest->same = i->src1->same;
                continue;
            }
            else if (i->op == CALL)
            {
                cexpmap_clear(cem);
                continue;
            }
            copr_shortcut(&i->src1);
            copr_shortcut(&i->src2);
            t = cexpmap_lookup(cem, i);
            if (t)
            {
                i->op = MOVE;
                i->src1 = t->dest;
                i->src2 = NULL;
                i->dest->same = i->src1;
            }
            else
            {
                switch (i->op)
                {
                    case MUL: case DIV: case MOD: case ADD: case SUB:
                    case SHL: case SHR: case AND: case XOR: case OR: case NOR: 
                    case EQ: case NE: case LT: case GT: case LE: case GE:
                    case NEG:
                        cexpmap_insert(cem, i);
                        break;
                    default: ;
                }
            }
        }
        cexpmap_clear(cem);
    }
    cexpmap_destroy(cem);
}

void ssa_func(CType_t func) {
#define OPRS_ADD(_opr) \
    do { \
        if (cpset_insert(avs, (long)((_opr)->info.var))) \
        { \
            COList_t n = NEW(COList); \
            n->next = oprs; \
            n->opr = _opr; \
            oprs = n; \
        } \
    } while (0)

#define VS_ADD(_d) \
    do { \
        if (cpset_insert(vs, (long)(_d))) \
        { \
            CVList_t n = NEW(CVList); \
            n->next = vars; \
            n->var = _d; \
            vars = n; \
        } \
    } while (0)


    CBlock_t p;
    entry = cblock_create(1);
    CPSet_t vs = cpset_create(), avs = cpset_create();
    CVList_t vars = NULL;
    COList_t oprs = NULL;
    /* CVar_t pr; */
    cfg_clear();
    dtree_clear();
    ssa_comp(func->rec.func.body, entry, NULL);
    /*
    for (i = 0, pr = func->rec.func.params;
        i < 4 && pr;
        i++, pr = pr->next)
        pr->loc = -(i + 1); */ /* mark arguments */

    for (p = entry; p; p = p->next)
    {
        CInst_t head = p->insts, i;
        CEdge *e;
        p->pred = 0;
        for (e = cfg.rhead[p->id]; e; e = e->next)
            p->pred++;
        for (i = head->next; i != head; i = i->next)
        {
            if (i->src1 && (i->src1->kind == VAR || i->src1->kind == TMP))
                OPRS_ADD(i->src1);
            if (i->src2 && (i->src2->kind == VAR || i->src2->kind == TMP))
                OPRS_ADD(i->src2);
            if (i->op == WARR)
                OPRS_ADD(i->dest);
            else if (i->dest && i->dest->kind == VAR)
            {
                CVar_t d = i->dest->info.var;
                CBList_t b = NEW(CBList);
                VS_ADD(d);
                OPRS_ADD(i->dest);
                b->next = d->defsite;
                b->cblk = p;
                d->defsite = b;
            }
        }
        blks[p->id] = p;
    }
    cpset_destroy(avs);
    cpset_destroy(vs);
    calc_dominant_frontier();
    /* build SSA */
    insert_phi(vars);
    renaming_vars(oprs);
    /* optimization on SSA */
    const_propagation();
    subexp_elimination();
    strength_reduction();
    /* out of SSA */
    mark_insts();
    build_intervals();
    register_alloc();
}

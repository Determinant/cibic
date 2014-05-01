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
static int bcnt;        /* block counter */
static int tcnt;        /* temporary counter */

/* for code generation */
int gbbase;
CBlock_t entry;
COList_t defs;
CType_t func;

COpr_t copr_create() {
    COpr_t opr = NEW(COpr);
    opr->cval = NULL;
    return opr;
}

CInst_t cinst_create() {
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

CVar_t ctmp_create(CType_t type) {
    static char buff[MAX_NAMELEN];
    sprintf(buff, "t%d", tcnt++);
    return cvar_create(strdup(buff), type, NULL);
}

void ctmp_destroy(CVar_t type) {
    free(type->name); /* allocated dynamically */
    free(type);
}

void cfg_clear() {
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

void dtree_clear() {
    int i;
    for (i = 0; i < MAX_BLOCK; i++)
    {
        CEdge *p, *np;
        for (p = dtree.head[i]; p; p = np)
        {
            np = p->next;
            free(p);
        }
        dtree.head[i] = NULL;
    }
}

void cfg_add_edge(CBlock_t from, CBlock_t to) {
    int fid = from->id, tid = to->id;
/*     printf("%d -> %d\n", from->id, to->id); */
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

void copr_print(COpr_t opr) {
    switch (opr->kind)
    {
        case VAR: 
                  fprintf(stderr, "%s_%d", opr->info.var->name, opr->sub);
                  break;
        case TMP: fprintf(stderr, "%s", opr->info.var->name);
                  break;
        case IMM: fprintf(stderr, "%d", opr->info.imm);
                  break;
        case IMMS: fprintf(stderr, "\"%s\"", opr->info.str);
                  break;
        case IMMF: fprintf(stderr, "%s", opr->info.str);
                  break;
    }
}

void cinst_print(CInst_t inst) {
    switch (inst->op)
    {
        case LOAD:
            fprintf(stderr, "load ");
            copr_print(inst->dest);
            break;
        case MOVE:
            copr_print(inst->dest);
            fprintf(stderr, " = ");
            copr_print(inst->src1);
            break;
        case BEQZ:
            fprintf(stderr, "if not (");
            copr_print(inst->src1);
            fprintf(stderr, ") goto _L");
            copr_print(inst->dest);
            break;
        case BNEZ:
            fprintf(stderr, "if (");
            copr_print(inst->src1);
            fprintf(stderr, ") goto _L");
            copr_print(inst->dest);
            break;
        case GOTO:
            fprintf(stderr, "goto _L");
            copr_print(inst->dest);
            break;
        case ARR:
            copr_print(inst->dest);
            fprintf(stderr, " = ");
            copr_print(inst->src1);
            fprintf(stderr, "[");
            copr_print(inst->src2);
            fprintf(stderr, "]");
            break;
        case NEG:
            copr_print(inst->dest);
            fprintf(stderr, " = -");
            copr_print(inst->src1);
            break;
        case WARR:
            copr_print(inst->dest);
            fprintf(stderr, "[");
            copr_print(inst->src2);
            fprintf(stderr, "] = ");
            copr_print(inst->src1);
            break;
        case PUSH:
            fprintf(stderr, "push ");
            copr_print(inst->src1);
            break;
        case CALL:
            copr_print(inst->dest);
            fprintf(stderr, " = call ");
            copr_print(inst->src1);
            break;
        case RET:
            if (inst->src1)
            {
                fprintf(stderr, "return ");
                copr_print(inst->src1);
            }
            else fprintf(stderr, "return");
            break;
        case ADDR:
            copr_print(inst->dest);
            fprintf(stderr, " = addr ");
            copr_print(inst->src1);
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
                    case LOR: op = "||"; break;
                    case LAND: op = "&&"; break;
                    case EQ: op = "=="; break;
                    case NE: op = "!="; break;
                    case NOR: op = "nor"; break;
                    default: ;
                }
                copr_print(inst->dest);
                fprintf(stderr, " = ");
                copr_print(inst->src1);
                fprintf(stderr, " %s ", op);
                copr_print(inst->src2);
            }
    }
    fprintf(stderr, "\n");
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
            cinst_print(p);
        }
    }
}
void ssa_func_print(CBlock_t p) {
    for (; p; p = p->next)
        cblock_print(p);
}
void ssa_func(CType_t);
void ssa_generate() {
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

COpr_t ssa_exp_(CNode *p, CBlock_t, CInst_t, CBlock_t);
COpr_t ssa_postfix(CNode *p, CBlock_t cur, CInst_t lval, CBlock_t succ) {
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
                off->dest->info.var = ctmp_create(post->chd->ext.type);
                off->op = MUL;
                off->src1 = ssa_exp_(post->chd, cur, NULL, succ);
                off->src2 = copr_create();
                off->src2->kind = IMM;
                off->src2->info.imm = calc_size(rt);
                cblock_append(cur, off);

                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = off->dest;
                base->op = rt->type == CARR ? ADD : ARR;
            }
            break;
        case POSTFIX_DOT:
            {
                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                base->op = ARR;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = copr_create();
                base->src2->kind = IMM;
                base->src2->info.imm = p->ext.offset;
            }
            break;
        case POSTFIX_PTR:
            {
                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                base->op = ARR;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = copr_create();
                base->src2->kind = IMM;
                base->src2->info.imm = p->ext.offset;
            }
            break;
        case POSTFIX_CALL:
            {
                CNode *arg = post->chd->chd;
                /* CInst_t ps = NULL, t; */
                base->op = CALL;
                base->src1 = ssa_exp_(p->chd, cur, lval, succ);
                base->dest = copr_create();
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                for (; arg; arg = arg->next)
                {
                    CInst_t pi = cinst_create();
                    pi->op = PUSH;
                    pi->src1 = ssa_exp_(arg, cur, lval, succ);
                    /* pi->next = ps;
                    ps = pi; */
                    cblock_append(cur, pi);
                }
                /*
                for (; ps; ps = t)
                {
                    t = ps->next;
                    cblock_append(cur, ps);
                } */
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
                    base->dest->info.var = ctmp_create(rt);
                    tins->src1 = base->dest;
                    tins2->op = ARR;
                    tins2->src1 = tins->dest;
                    tins2->src2 = tins->src2;
                    tins2->dest = copr_create();
                    tins2->dest->kind = TMP;
                    tins2->dest->info.var = ctmp_create(rt);

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
        free(base);
        return lval->dest;
    }
    cblock_append(cur, base);
    return base->dest;
}

COpr_t ssa_exp(CNode *, CBlock_t, int);
COpr_t ssa_exp_(CNode *p, CBlock_t cur, CInst_t lval, CBlock_t succ) {
    COpr_t res;
    CInst_t inst = cinst_create();
    switch (p->type)
    {
        case NOP: ; break;
        case ID:
            res = copr_create();
            res->kind = VAR;
            res->info.var = p->ext.var;
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
            res->info.str = p->rec.strval;
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
                        CInst_t last = cblock_getback(cur);
                        if (last && last->dest == inst->src1)
                        {
                            free(last->dest);
                            last->dest = inst->dest;
                            free(inst);
                            return last->dest;
                        }
                        else 
                        {
                            cblock_append(cur, inst);
                            return inst->dest;
                        }
                    }
                    else
                    {
                        CInst_t tins = cinst_create();
                        cblock_append(cur, inst);
                        tins->op = ARR;
                        tins->src1 = inst->dest;    /* base */
                        tins->src2 = inst->src2;    /* displacement */
                        tins->dest = copr_create();
                        tins->dest->kind = TMP;
                        tins->dest->info.var = ctmp_create(p->ext.type);
                        cblock_append(cur, tins);
                        return tins->dest;
                    }
                }
                else if (op == '*' && !p->chd->next)
                {
                    {
                        if (lval)
                        {
                            lval->op = WARR;
                            lval->dest = ssa_exp_(p->chd, cur, NULL, succ);
                            lval->src2 = copr_create();
                            lval->src2->kind = IMM;
                            lval->src2->info.imm = 0;
                            return lval->dest;
                        }
                        else
                        {
                            inst->op = ARR;
                            inst->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                            inst->src2 = copr_create();
                            inst->src2->kind = IMM;
                            inst->src2->info.imm = 0;
                            inst->dest = copr_create();
                            inst->dest->kind = TMP;
                            inst->dest->info.var = ctmp_create(p->ext.type);
                            cblock_append(cur, inst);
                            return inst->dest;
                        }
                    }
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
                    inst->dest->info.var = ctmp_create(p->ext.type);
                    cblock_append(cur, inst);
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
                            cblock_append(cur, inst);
                            free(tins);
                            return inst->dest;
                        }
                        else
                        {
                            CInst_t tins2 = cinst_create();
                            inst->dest = copr_create();
                            inst->dest->kind = TMP;
                            inst->dest->info.var = ctmp_create(p->ext.type);
                            tins->src1 = inst->dest;
                            tins2->op = ARR;
                            tins2->src1 = tins->dest;    /* base */
                            tins2->src2 = tins->src2;    /* displacement */
                            tins2->dest = copr_create();
                            tins2->dest->kind = TMP;
                            tins2->dest->info.var = ctmp_create(p->ext.type);
                            cblock_append(cur, inst);
                            cblock_append(cur, tins);
                            cblock_append(cur, tins2);
                            return tins2->dest;
                        }
                    }
                }

                switch (op)
                {
                    case EXP_CAST:
                        free(inst);
                        return ssa_exp_(p->chd->next, cur, lval, succ);
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
                                case OPT_OR: inst->op = LOR; break;
                                case OPT_AND: inst->op = LAND; break;
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
                                    /*
                                    if (inst->src1->kind == IMM)
                                    {
                                        if (inst->op == NEG)
                                        {
                                           inst->src1->info.imm *= -1;
                                           res = inst->src1;
                                           free(inst);
                                           return res;
                                        }
                                        else if (inst->src2->kind == IMM)
                                        {
                                            int *imm = &inst->src1->info.imm;
                                            int imm2 = inst->src2->info.imm;
                                            switch (inst->op)
                                            {
                                                case MUL: *imm *= imm2; break;
                                                case DIV: *imm /= imm2; break;
                                                case MOD: *imm %= imm2; break;
                                                case ADD: *imm += imm2; break;
                                                case SUB: *imm -= imm2; break;
                                                case SHL: *imm <<= imm2; break;
                                                case SHR: *imm >>= imm2; break;
                                                case AND: *imm &= imm2; break;
                                                case XOR: *imm ^= imm2; break;
                                                case OR: *imm  |= imm2; break;
                                                case LOR: *imm = *imm || imm2; break;
                                                case LAND: *imm = *imm && imm2; break;
                                                case NOR: *imm = ~(*imm | imm2); break;
                                                case EQ: *imm = *imm == imm2; break;
                                                case NE: *imm = *imm != imm2; break;
                                                case LT: *imm = *imm < imm2; break;
                                                case GT: *imm = *imm > imm2; break;
                                                case LE: *imm = *imm <= imm2; break;
                                                case GE: *imm = *imm >= imm2; break;
                                                default: assert(0);
                                            }
                                            res = inst->src1;
                                            free(inst->src2);
                                            free(inst);
                                            return res;
                                        }
                                    }
                                    */
                                    inst->dest = copr_create();
                                    inst->dest->kind = TMP;
                                    inst->dest->info.var = ctmp_create(p->ext.type);
                                }
                                cblock_append(cur, inst);
                                res = inst->dest;
                            }
                        }
                }
            }
    }
    return res;
}

COpr_t ssa_exp(CNode *p, CBlock_t cur, int discard_last) {
    CBlock_t succ = cblock_create(0);
    COpr_t res = ssa_exp_(p, cur, NULL, succ);
    CInst_t last; 
    {
        CInst_t head = succ->insts, t;
        while (head->next != head)
        {
            t = head->next;
            cblock_popfront(succ);
            cblock_append(cur, t);
        }
        free(succ);
    }
    last = cblock_getback(cur);
    if (discard_last && last 
                    && last->dest->kind == TMP
                    && last->op != CALL) /* temporary not used */
    {
        ctmp_destroy(last->dest->info.var);
        cblock_popback(cur);
        free(last);
    }
    return res;
}

CBlock_t ssa_stmt(CNode *, CBlock_t, CBlock_t);
CBlock_t ssa_while(CNode *p, CBlock_t cur) {
    CNode *exp = p->chd;
    CBlock_t loop_blk = cblock_create(1), loop_t,
             cond_blk = cblock_create(1),
             next_blk = cblock_create(1);
    CInst_t j_inst = cinst_create(),
            if_inst = cinst_create();

    DBLINK(cond_blk, next_blk);
    loop_t = ssa_stmt(exp->next, loop_blk, next_blk);

    DBLINK(loop_t, cond_blk);
    cfg_add_edge(loop_t, cond_blk);


    j_inst->op = GOTO;
    j_inst->dest = copr_create();
    j_inst->dest->kind = IMM;
    j_inst->dest->info.imm = cond_blk->id + gbbase;
    cond_blk->ref = 1;
    cblock_append(cur, j_inst);

    if_inst->op = BNEZ;
    if_inst->src1 = ssa_exp(exp, cond_blk, 0);
    if_inst->dest = copr_create();
    if_inst->dest->kind = IMM;
    if_inst->dest->info.imm = loop_blk->id + gbbase;
    loop_blk->ref = 1;
    cblock_append(cond_blk, if_inst);

    cfg_add_edge(cur, cond_blk);
    cfg_add_edge(cond_blk, loop_blk);
    cfg_add_edge(cond_blk, next_blk);

    DBLINK(cur, loop_blk);

    return next_blk;
}

CBlock_t ssa_for(CNode *p, CBlock_t cur) {
    CNode *exp1 = p->chd,
          *exp2 = exp1->next,
          *exp3 = exp2->next;
    CBlock_t loop_blk = cblock_create(1), loop_t,
             cond_blk = cblock_create(1),
             next_blk = cblock_create(1);
    CInst_t j_inst = cinst_create(),
            if_inst = cinst_create();

    DBLINK(cond_blk, next_blk);
    loop_t = ssa_stmt(exp3->next, loop_blk, next_blk);

    DBLINK(loop_t, cond_blk);
    cfg_add_edge(loop_t, cond_blk);

    ssa_exp(exp1, cur, 1);
    ssa_exp(exp3, loop_t, 1);

    j_inst->op = GOTO;
    j_inst->dest = copr_create();
    j_inst->dest->kind = IMM;
    j_inst->dest->info.imm = cond_blk->id + gbbase;
    cond_blk->ref = 1;
    cblock_append(cur, j_inst);

    if_inst->op = BNEZ;
    if_inst->src1 = ssa_exp(exp2, cond_blk, 0);
    if_inst->dest = copr_create();
    if_inst->dest->kind = IMM;
    if_inst->dest->info.imm = loop_blk->id + gbbase;
    loop_blk->ref = 1;
    cblock_append(cond_blk, if_inst);

    cfg_add_edge(cur, cond_blk);
    cfg_add_edge(cond_blk, loop_blk);
    cfg_add_edge(cond_blk, next_blk);

    DBLINK(cur, loop_blk);

    return next_blk;
}

CBlock_t ssa_if(CNode *p, CBlock_t cur, CBlock_t loop_exit) {
    CNode *body1 = p->chd->next,
          *body2 = body1->next;
    CBlock_t then_blk, then_t, next_blk,
             else_blk, else_t;
    CInst_t if_inst = cinst_create();
    COpr_t rt = ssa_exp(p->chd, cur, 0);
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
    if_inst->op = BEQZ;
    if_inst->src1 = rt; /* calculated cond */
    if_inst->dest = copr_create();
    if_inst->dest->kind = IMM;
    cblock_append(cur, if_inst);

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
}

CBlock_t ssa_ret(CNode *p, CBlock_t cur) {
    CInst_t inst = cinst_create();
    inst->op = RET;
    if (p->chd->type != NOP)
        inst->src1 = ssa_exp(p->chd, cur, 0);
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
            ssa_exp(p->chd, cur, 1);
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
    CNode *stmts = p->chd->next, *i;
    if (stmts->chd->type != NOP)
        for (i = stmts->chd; i; i = i->next)
            cur = ssa_stmt(i, cur, loop_exit);
    return cur;
}

CPSet_t cpset_create() {
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
CPSet_t dfset[MAX_BLOCK], phi[MAX_BLOCK];
CBList_t df[MAX_BLOCK];

void dfs(CBlock_t u, int v) {
    CEdge *e;
    if (vis[u->id] != -1) return;
    par[u->id] = v;
    vis[u->id] = 0;
    for (e = cfg.head[u->id]; e; e = e->next)
        dfs(e->to, u->id);
    vis[u->id] = ocnt;
    ord[ocnt++] = u->id;
}

int intersect(int b1, int b2) {
    while (b1 != b2)
        if (b1 < b2) b1 = dom[b1];
        else b2 = dom[b2];
    return b1;
}

void calc_dominant_frontier() {
    int i;
    int ch = 1;
    ocnt = 0;
    memset(vis, -1, sizeof vis);
    memset(dom, -1, sizeof dom);
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
    {
        CBList_t p, np;
        for (p = df[i]; p; p = np)
        {
            free(p);
            np = p->next;
        }
        df[i] = NULL;
        if (dfset[i]) cpset_destroy(dfset[i]);
        dfset[i] = cpset_create();
    }
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
    /*
    for (i = 0; i < bcnt; i++)
    {
        CBList_t p = df[i];
        printf("%d: ", i);
        for (; p; p = p->next)
            printf("%d ", p->cblk->id); puts("");
    }
    */
}

void insert_phi(CVList_t vars) {
    CVList_t vp;
    int i;
    for (i = 0; i < bcnt; i++)
    {
        if (phi[i]) cpset_destroy(phi[i]);
        phi[i] = cpset_create();
    }
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
                    phi->dest->info.var = var;
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

}

void renaming_dfs(CBlock_t blk) {
    CInst_t ih = blk->insts, i;
    CPhi_t ph = blk->phis, pi;
    CEdge *e = cfg.head[blk->id];
    COList_t defs = NULL, dn;
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
        n2->next = defs;
        defs = n2;
    }
    for (i = ih->next; i != ih; i = i->next)
    {
        COpr_t dest = i->dest;
        COpr_t *opr[2] = {&(i->src1), &(i->src2)};
        int t;
        for (t = 0; t < 2; t++)
        {
            COpr_t p = *(opr[t]);
            if (!(p && p->kind == VAR)) continue;
            /* free(p); */ /* memory leak */
            *(opr[t]) = p->info.var->stack->opr;
        }
        if (dest)
        {
            if (dest->kind == VAR)
            {
                if (i->op == WARR)
                    i->dest = dest->info.var->stack->opr;
                else
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
                    n2->next = defs;
                    defs = n2;
                }
            }
            else if (dest->kind == TMP)
            {
                dest->def = i;
                dest->range = NULL;
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
            pi->oprs[j] = pi->dest->info.var->stack->opr;
    }
    for (e = dtree.head[blk->id]; e; e = e->next)
        renaming_dfs(e->to);
    for (; defs; defs = dn)
    {
        CVar_t var = defs->opr->info.var;
        COList_t nf = var->stack->next;
        free(var->stack);
        var->stack = nf;
        dn = defs->next;
        free(defs);
    }
}

void renaming_vars(CVList_t vars) {
    CVList_t vp;
    for (vp = vars; vp; vp = vp->next)
    {
        CInst_t ld = cinst_create();
        vp->var->cnt = 0;
        ld->op = LOAD;
        ld->dest = copr_create();
        ld->dest->kind = VAR;
        ld->dest->info.var = vp->var;
        cblock_pushfront(entry, ld);
/*      CVar_t var = vp->var;
        COpr_t idef = copr_create();
        COList_t n = NEW(COList);
        var->cnt = 0;
        var->stack = n;
        n->opr = idef;
        n->next = NULL;
        idef->def = NULL;
        idef->sub = 0;
        idef->kind = VAR;
        idef->info.var = var;
        */
    }
    renaming_dfs(entry);
}

void mark_insts() {
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
            for (ii = ih->next; ii != ih; ii = ii->next)
                ii->id = icnt++;
            for (pi = ph->next; pi != ph; pi = pi->next)
                icnt++;
            b->first = ih->next->id;
            b->last = ih->prev->id;
        }
    }
}

CPSet_t liveset[MAX_BLOCK];
COList_t live[MAX_BLOCK];

void add_range(COpr_t opr, CBlock_t blk, int end) {
    int dfid = opr->def->id;
    CRange_t range = NEW(CRange);
    if (blk->first <= dfid && dfid <= blk->last)
        range->l = dfid;
    else
        range->l = blk->first;
    range->r = end;
    range->next = opr->range;
    opr->range = range;
}

void build_intervals() {
    int i;
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
        liveset[i] = cpset_create();
    }
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
                COpr_t oprs[2] = {i->src1, i->src2};
                if (i->dest &&
                    (i->dest->kind == VAR ||
                     i->dest->kind == TMP) && i->op != WARR) /* def */
                {
                    i->is_def = 1;
                    cpset_erase(curlive, (long)i->dest);
                }
                else
                    i->is_def = 0;
                for (t = 0; t < 2; t++)
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
    }
}

COpr_t cinterv_repr(COpr_t opr) {
    return opr->par == opr ? opr : (opr->par = cinterv_repr(opr->par));
}

CRange_t crange_merge(CRange_t a, CRange_t b) {
    CRange res;
    CRange_t tail = &res;
    res.next = NULL;
    res.r = -1;
    for (; a || b;)
    {
        if (a && (!b || a->l < b->l))
        {
            if (tail->r == a->l)
                tail->r = a->r;
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
            if (tail->r == b->l)
                tail->r = b->r;
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

void cinterv_union(COpr_t a, COpr_t b) {
    a = cinterv_repr(a);
    b = cinterv_repr(b);
    fprintf(stderr, "merging ");
    copr_print(a);
    fprintf(stderr, " ");
    copr_print(b);
    fprintf(stderr, "\n");
    if (a == b) return;
    b->range = crange_merge(b->range, a->range);
    a->par = b;
}

COList_t init_def() {
    CBlock_t p;
    COList_t defs = NULL, def;
    for (p = entry; p; p = p->next)
    {
        CInst_t i, ih = p->insts;
        CPhi_t pi, ph = p->phis;
        for (i = ih->next; i != ih; i = i->next)
            if (i->is_def)
            {
                i->dest->par = i->dest;
                def = NEW(COList);
                def->opr = i->dest;
                def->next = defs;
                defs = def;
            }
        for (pi = ph->next; pi != ph; pi = pi->next)
        {
            pi->dest->par = pi->dest;
            def = NEW(COList);
            def->opr = pi->dest;
            def->next = defs;
            defs = def;
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
    return defs;
}

void print_intervals(COList_t defs) {
    for (; defs; defs = defs->next)
    {
        COpr_t opr = defs->opr,
               repr = cinterv_repr(opr);
        CRange_t p;
        copr_print(opr);
        fprintf(stderr, ": ");
        if (repr == opr)
        {
            for (p = opr->range; p; p = p->next)
                fprintf(stderr, "[%d, %d)", p->l, p->r);
        }
        else copr_print(repr);
        fprintf(stderr, "\n");
    }
}

void register_alloc() {
    mark_insts();
    build_intervals();
    defs = init_def();
    /* FIXME: all vars are spilled */
    {
        COList_t p;
        for (p = defs; p; p = p->next)
            p->opr->reg = -1;
    }
    print_intervals(defs);
}

void const_propagation() {
    int i;
    for (i = bcnt - 1; i >= 0; i--)
    {
        CBlock_t b = blks[vis[i]];
        CInst_t i, ih = b->insts;
        for (i = ih->next; i != ih; i = i->next)
        {
            int flag = 0;
            COpr_t t;
            if (!i->dest) continue;
            if (i->src1 && i->src1->cval)
            {
                t = i->src1->cval;
                i->src1 = copr_create();
                *(i->src1) = *t;
            }
            if (i->src2 && i->src2->cval)
            {
                t = i->src2->cval;
                i->src2 = copr_create();
                *(i->src2) = *t;
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
                        case LOR: immd = imm1 || imm2; break;
                        case LAND: immd = imm1 && imm2; break;
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
            if (flag) i->dest->cval = i->src1;
        }
    }
}

void ssa_func(CType_t func) {
    CBlock_t p;
    entry = cblock_create(1);
    CPSet_t vs = cpset_create(), avs = cpset_create();
    CVList_t vars = NULL, all_vars = NULL;
    int i;
    cfg_clear();
    dtree_clear();
    ssa_comp(func->rec.func.body, entry, NULL);
    for (p = entry; p; p = p->next)
    {
        CInst_t head = p->insts, i;
        CEdge *e;
        p->pred = 0;
        for (e = cfg.rhead[p->id]; e; e = e->next)
            p->pred++;
        for (i = head->next; i != head; i = i->next)
        {
            if (i->src1 && i->src1->kind == VAR)
                cpset_insert(avs, (long)(i->src1->info.var));
            if (i->src2 && i->src2->kind == VAR)
                cpset_insert(avs, (long)(i->src2->info.var));
            if (i->dest && i->dest->kind == VAR)
            {
                if (i->op == WARR)
                    cpset_insert(avs, (long)(i->dest->info.var));
                else
                {
                    CVar_t d = i->dest->info.var;
                    CBList_t b = NEW(CBList);
                    cpset_insert(vs, (long)d);
                    cpset_insert(avs, (long)d);
                    b->next = d->defsite;
                    b->cblk = p;
                    d->defsite = b;
                }
            }
        }
        blks[p->id] = p;
    }
    calc_dominant_frontier();
    {
        CPNode *p;
        for (i = 0; i < MAX_TABLE_SIZE; i++)
        {
            for (p = vs->head[i]; p; p = p->next)
            {
                CVList_t n = NEW(CVList);
                n->next = vars;
                n->var = (CVar_t)p->key;
                vars = n;
            }
            for (p = avs->head[i]; p; p = p->next)
            {
                CVList_t n = NEW(CVList);
                n->next = all_vars;
                n->var = (CVar_t)p->key;
                all_vars = n;
            }
        }
    }
    insert_phi(vars);
    renaming_vars(all_vars);
    const_propagation();
    register_alloc();
    cpset_destroy(vs);
    cpset_destroy(avs);
}

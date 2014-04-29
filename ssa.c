#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "ast.h"
#include "ssa.h"
#define NEW(type) ((type *)malloc(sizeof(type)))
#define DBLINK(from, to) ((from)->next = (to))->prev = (from)

static CGraph cfg;
static CBlock_t blks[MAX_BLOCK];
static int bcnt;        /* block counter */
static int tcnt;        /* temporary counter */
static int gbbase;

CBlock_t cblock_create() {
    CBlock_t cblk = NEW(CBlock);
    CInst_t dum = NEW(CInst);
    CPhi_t pdum = NEW(CPhi);
    dum->prev = dum;
    dum->next = dum;
    pdum->prev = pdum;
    pdum->next = pdum;
    cblk->insts = dum;
    cblk->phis = pdum;
    cblk->next = NULL;
    cblk->id = (bcnt++) + gbbase;
    cblk->ref = 0;
    return cblk;
}

void cblock_append(CBlock_t cblk, CInst_t inst) {
    CInst_t head = cblk->insts;
    (inst->prev = head->prev)->next = inst;
    (inst->next = head)->prev = inst;
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
        if (cfg.head[i])
        {
            CEdge *p, *np;
            for (p = cfg.head[i]; p; p = np)
            {
                np = p->next;
                free(p);
            }
            cfg.head[i] = NULL;
        }
}

void cfg_add_edge(CBlock_t from, CBlock_t to) {
    int id = from->id;
    CEdge *e = NEW(CEdge);
    e->to = to;
    e->next = cfg.head[id];
    cfg.head[id] = e;
}

void copr_print(COpr_t opr) {
    switch (opr->kind)
    {
        case VAR: 
        case TMP: fprintf(stderr, "%s", opr->info.var->name);
                  break;
        case IMM: fprintf(stderr, "%d", opr->info.imm);
                  break;
        case IMMS: fprintf(stderr, "\"%s\"", opr->info.str);
                  break;
    }
}

void cinst_print(CInst_t inst) {
    switch (inst->op)
    {
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
                    case SEQ: op = "seq"; break;
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

void cblock_print(CBlock_t blk) {
    CInst_t p, sp = blk->insts;
    /*if (blk->ref)*/
        fprintf(stderr, "_L%d:\n", blk->id);
    for (p = sp->next; p != sp; p = p->next)
    {
        fprintf(stderr, "\t");
        cinst_print(p);
    }
}
void ssa_func_print(CBlock_t p) {
    for (; p; p = p->next)
        cblock_print(p);
}
CBlock_t  ssa_func(CType_t);
void ssa_generate(CScope_t scope) {
    CTNode *p;
    int i;
    for (i = 0; i < MAX_TABLE_SIZE; i++)
        for (p = scope->ids->head[i]; p; p = p->next)
        {
            CSymbol_t tp = (CSymbol_t)(p->val);
            CType_t func = tp->rec.type;
            if (tp->kind != CTYPE ||
                func->type != CFUNC ||
                !func->rec.func.body) continue;
            fprintf(stderr, "%s:\n", tp->rec.type->name);
            ssa_func_print(ssa_func(func));
            gbbase += bcnt;
            bcnt = 0;
        }
}

COpr_t ssa_exp_(CNode *p, CBlock_t, CInst_t, CBlock_t);
COpr_t ssa_postfix(CNode *p, CBlock_t cur, CInst_t lval, CBlock_t succ) {
    CNode *post = p->chd->next;
    CType_t rt = p->ext.type;
    CInst_t base = NEW(CInst);
    switch (post->rec.subtype)
    {
        case POSTFIX_ARR:
            {
                CInst_t off = NEW(CInst);
                off->dest = NEW(COpr);
                off->dest->kind = TMP;
                off->dest->info.var = ctmp_create(post->chd->ext.type);
                off->op = MUL;
                off->src1 = ssa_exp_(post->chd, cur, NULL, succ);
                off->src2 = NEW(COpr);
                off->src2->kind = IMM;
                off->src2->info.imm = calc_size(rt);
                cblock_append(cur, off);

                base->dest = NEW(COpr);
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = off->dest;
                base->op = rt->type == CARR ? ADD : ARR;
            }
            break;
        case POSTFIX_DOT:
            {
                base->dest = NEW(COpr);
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                base->op = ARR;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = NEW(COpr);
                base->src2->kind = IMM;
                base->src2->info.imm = p->ext.offset;
            }
            break;
        case POSTFIX_PTR:
            {
                base->dest = NEW(COpr);
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                base->op = ARR;
                base->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                base->src2 = NEW(COpr);
                base->src2->kind = IMM;
                base->src2->info.imm = p->ext.offset;
            }
            break;
        case POSTFIX_CALL:
            {
                CNode *arg = post->chd->chd;
                CInst_t ps = NULL, t;
                base->op = CALL;
                base->src1 = ssa_exp_(p->chd, cur, lval, succ);
                base->dest = NEW(COpr);
                base->dest->kind = TMP;
                base->dest->info.var = ctmp_create(rt);
                for (; arg; arg = arg->next)
                {
                    CInst_t pi = NEW(CInst);
                    pi->op = PUSH;
                    pi->src1 = ssa_exp_(arg, cur, lval, succ);
                    pi->next = ps;
                    ps = pi;
                }
                for (; ps; ps = t)
                {
                    t = ps->next;
                    cblock_append(cur, ps);
                }
            }
            break;
        default:
            {
                CInst_t tins = NEW(CInst);
                ssa_exp_(p->chd, cur, tins, succ);
                base->op = post->rec.subtype == OPT_INC ? ADD : SUB;
                base->src2 = NEW(COpr);
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
                    CInst_t tins2 = NEW(CInst);
                    base->dest = NEW(COpr);
                    base->dest->kind = TMP;
                    base->dest->info.var = ctmp_create(p->ext.type);
                    tins->src1 = base->dest;
                    tins2->op = ARR;
                    tins2->src1 = tins->dest;
                    tins2->src2 = tins->src2;
                    tins2->dest = NEW(COpr);
                    tins2->dest->kind = TMP;
                    tins2->dest->info.var = ctmp_create(p->ext.type);

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
    CInst_t inst = NEW(CInst);
    switch (p->type)
    {
        case NOP: ; break;
        case ID:
            res = NEW(COpr);
            res->kind = VAR;
            res->info.var = p->ext.var;
            if (lval)
            {
                lval->op = MOVE;
                lval->dest = res;
            }
            break;
        case STR:
            res = NEW(COpr);
            res->kind = IMMS;
            res->info.str = p->rec.strval;
            break;

        default:
            if (p->ext.is_const)
            {
                res = NEW(COpr);
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
                    cblock_append(cur, inst);
                    if (inst->op == MOVE)
                        return inst->dest;
                    else
                    {
                        CInst_t tins = NEW(CInst);
                        tins->op = ARR;
                        tins->src1 = inst->dest;    /* base */
                        tins->src2 = inst->src2;    /* displacement */
                        tins->dest = NEW(COpr);
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
                            lval->src2 = NEW(COpr);
                            lval->src2->kind = IMM;
                            lval->src2->info.imm = 0;
                            return lval->dest;
                        }
                        else
                        {
                            inst->op = ARR;
                            inst->src1 = ssa_exp_(p->chd, cur, NULL, succ);
                            inst->src2 = NEW(COpr);
                            inst->src2->kind = IMM;
                            inst->src2->info.imm = 0;
                            inst->dest = NEW(COpr);
                            inst->dest->kind = TMP;
                            inst->dest->info.var = ctmp_create(p->ext.type);
                            cblock_append(cur, inst);
                            return inst->dest;
                        }
                    }
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
                        CInst_t tins = NEW(CInst);
                        ssa_exp_(p->chd, cur, tins, succ);                 /* as lval */
                        inst->src1 = ssa_exp_(p->chd, cur, NULL, succ);    /* as rval */
                        if (unary)
                        {
                            inst->src2 = NEW(COpr);
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
                            CInst_t tins2 = NEW(CInst);
                            inst->dest = NEW(COpr);
                            inst->dest->kind = TMP;
                            inst->dest->info.var = ctmp_create(p->ext.type);
                            tins->src1 = inst->dest;
                            tins2->op = ARR;
                            tins2->src1 = tins->dest;    /* base */
                            tins2->src2 = tins->src2;    /* displacement */
                            tins2->dest = NEW(COpr);
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
                            COpr_t lhs = ssa_exp_(p->chd, cur, lval, succ), rhs;
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
                                case '&':
                                          if (p->chd->next)
                                              inst->op = AND;
                                          else
                                          {
                                              rec = 0;
                                              res = NEW(COpr);
                                              res->kind = IMM;
                                              res->info.imm = 0;
                                              /* TODO: be filled in with correct address */
                                          }
                                          break;
                                case '*':
                                          inst->op = MUL;
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
                                          inst->src2 = NEW(COpr);
                                          inst->src2->kind = IMM;
                                          inst->src2->info.imm = 0;
                                          break;
                                case '!':
                                          inst->op = SEQ;
                                          inst->src1 = lhs;
                                          inst->src2 = NEW(COpr);
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
                                    inst->dest = NEW(COpr);
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
    CBlock_t succ = cblock_create();
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
    CBlock_t loop_blk = cblock_create(), loop_t,
             cond_blk = cblock_create(),
             next_blk = cblock_create();
    CInst_t j_inst = NEW(CInst),
            if_inst = NEW(CInst);

    DBLINK(cond_blk, next_blk);
    loop_t = ssa_stmt(exp->next, loop_blk, next_blk);

    DBLINK(loop_t, cond_blk);
    cfg_add_edge(loop_t, cond_blk);


    j_inst->op = GOTO;
    j_inst->dest = NEW(COpr);
    j_inst->dest->kind = IMM;
    j_inst->dest->info.imm = cond_blk->id;
    cond_blk->ref = 1;
    cblock_append(cur, j_inst);

    if_inst->op = BNEZ;
    if_inst->src1 = ssa_exp(exp, cond_blk, 0);
    if_inst->dest = NEW(COpr);
    if_inst->dest->kind = IMM;
    if_inst->dest->info.imm = loop_blk->id;
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
    CBlock_t loop_blk = cblock_create(), loop_t,
             cond_blk = cblock_create(),
             next_blk = cblock_create();
    CInst_t j_inst = NEW(CInst),
            if_inst = NEW(CInst);

    DBLINK(cond_blk, next_blk);
    loop_t = ssa_stmt(exp3->next, loop_blk, next_blk);

    DBLINK(loop_t, cond_blk);
    cfg_add_edge(loop_t, cond_blk);

    ssa_exp(exp1, cur, 1);
    ssa_exp(exp3, loop_t, 1);

    j_inst->op = GOTO;
    j_inst->dest = NEW(COpr);
    j_inst->dest->kind = IMM;
    j_inst->dest->info.imm = cond_blk->id;
    cond_blk->ref = 1;
    cblock_append(cur, j_inst);

    if_inst->op = BNEZ;
    if_inst->src1 = ssa_exp(exp2, cond_blk, 0);
    if_inst->dest = NEW(COpr);
    if_inst->dest->kind = IMM;
    if_inst->dest->info.imm = loop_blk->id;
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
    CBlock_t then_blk = cblock_create(), then_t,
             next_blk,
             else_blk, else_t;
    CInst_t if_inst = NEW(CInst);
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

    if_inst->op = BEQZ;
    if_inst->src1 = rt; /* calculated cond */
    if_inst->dest = NEW(COpr);
    if_inst->dest->kind = IMM;
    cblock_append(cur, if_inst);

    cfg_add_edge(cur, then_blk);
    DBLINK(cur, then_blk);
    then_t = ssa_stmt(body1, then_blk, loop_exit);
    if (body2->type != NOP)
    {
        CInst_t j_inst = NEW(CInst);
        j_inst->op = GOTO;
        j_inst->dest = NEW(COpr);
        j_inst->dest->kind = IMM;

        else_blk = cblock_create();
        if_inst->dest->info.imm = else_blk->id;
        else_blk->ref = 1;
        DBLINK(then_t, else_blk);
        else_t = ssa_stmt(body2, else_blk, loop_exit);
        if (cblock_isempty(else_t))
            next_blk = else_t;
        else
        {
            next_blk = cblock_create();
            DBLINK(else_t, next_blk);
        }

        j_inst->dest->info.imm = next_blk->id;
        next_blk->ref = 1;
        cblock_append(then_t, j_inst);

        cfg_add_edge(cur, else_blk);
        cfg_add_edge(then_t, next_blk);
        cfg_add_edge(else_t, next_blk);
    }
    else
    {
        if (cblock_isempty(then_t))
            next_blk = then_t;
        else
        {
            next_blk = cblock_create();
            DBLINK(then_t, next_blk);
        }
        cfg_add_edge(cur, next_blk);
        cfg_add_edge(then_t, next_blk);
        if_inst->dest->info.imm = next_blk->id;
        next_blk->ref = 1;
    }
    return next_blk;
}

CBlock_t ssa_ret(CNode *p, CBlock_t cur) {
    CInst_t inst = NEW(CInst);
    inst->op = RET;
    if (p->chd->type != NOP)
        inst->src1 = ssa_exp(p->chd, cur, 0);
    else
        inst->src1 = NULL;
    cblock_append(cur, inst);
    return cur;
}

CBlock_t ssa_break(CBlock_t cur, CBlock_t loop_exit) {
    CInst_t inst = NEW(CInst);
    assert(loop_exit);
    inst->op = GOTO;
    inst->dest = NEW(COpr);
    inst->dest->kind = IMM;
    inst->dest->info.imm = loop_exit->id;
    loop_exit->ref = 1;
    cblock_append(cur, inst);
    cfg_add_edge(cur, loop_exit);
    return cur;
}

CBlock_t ssa_cont(CBlock_t cur, CBlock_t loop_exit) {
    CInst_t inst = NEW(CInst);
    assert(loop_exit);
    loop_exit = loop_exit->prev;    /* loop cond */
    inst->op = GOTO;
    inst->dest = NEW(COpr);
    inst->dest->kind = IMM;
    inst->dest->info.imm = loop_exit->id;
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

void cpset_insert(CPSet_t cps, long key) {
    unsigned int hv = key % MAX_TABLE_SIZE;
    CPNode *p = cps->head[hv], *np;
    for (; p; p = p->next)
        if (p->key == key)
            return;
    np = NEW(CPNode);
    np->key = key;
    np->next = cps->head[hv];
    cps->head[hv] = np;
}

CBlock_t ssa_func(CType_t func) {
    CBlock_t start = cblock_create(), p;
    CPSet_t vars = cpset_create();
    cfg_clear();
    ssa_comp(func->rec.func.body, start, NULL);
    for (p = start; p; p = p->next)
    {
        CInst_t head = p->insts, i;
        for (i = head->next; i != head; i = i->next)
        {
            switch (i->op)
            {
                case BEQZ: case BNEZ: case GOTO:
                case PUSH: case RET:
                case WARR:
                    ; break;
                default:
                    {
                        CVar_t d = i->dest->info.var;
                        CBList_t b = NEW(CBList);
                        cpset_insert(vars, (long)d);
                        b->next = d->defsite;
                        b->cblk = p;
                        d->defsite = b;
                    }
            }
        }
        blks[p->id] = p;
    }
    {
        int i;
        CPNode *p;
        for (i = 0; i < MAX_TABLE_SIZE; i++)
            for (p = vars->head[i]; p; p = p->next)
            {
                CVar_t var = (CVar_t)p->key;
                CBList_t t = var->defsite;
                CBList_t def = NULL;
                static int indef[MAX_BLOCK];
                fprintf(stderr, "%s:", var->name);
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
                for (t = def; t; t = t->next)
                    fprintf(stderr, " %d", t->cblk->id);
                fprintf(stderr, "\n");
            }
    } 
    return start;
}

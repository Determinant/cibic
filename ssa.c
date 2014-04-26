#include <stdlib.h>
#include <stdio.h>
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
    CInst_t dummy = NEW(CInst);
    dummy->prev = dummy;
    dummy->next = dummy;
    cblk->insts = dummy;
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

CInst_t cblock_getback(CBlock_t cblk) {
    return cblk->insts->prev;
}

int cblock_isempty(CBlock_t cblk) {
    return cblk->insts->prev == cblk->insts;
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

void copr_print(COpr *opr) {
    switch (opr->kind)
    {
        case VAR: 
        case TMP: fprintf(stderr, "%s", opr->info.var->name);
                  break;
        case IMM: fprintf(stderr, "%d", opr->info.imm);
                  break;
    }
}

void cinst_print(CInst_t inst) {
    switch (inst->op)
    {
        case MOVE:
            copr_print(&inst->dest);
            fprintf(stderr, " = ");
            copr_print(&inst->src1);
            break;
        case BEQZ:
            fprintf(stderr, "if not (");
            copr_print(&inst->src1);
            fprintf(stderr, ") goto _L");
            copr_print(&inst->dest);
            break;
        case GOTO:
            fprintf(stderr, "goto _L");
            copr_print(&inst->dest);
            break;
        case ADD:
            copr_print(&inst->dest);
            fprintf(stderr, " = ");
            copr_print(&inst->src1);
            fprintf(stderr, " + ");
            copr_print(&inst->src2);
            break;
    }
    fprintf(stderr, "\n");
}

void cblock_print(CBlock_t blk) {
    CInst_t p, sp = blk->insts;
    if (blk->ref)
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
        }
}

COpr ssa_exp(CNode *p, CBlock_t cur) {
    COpr res;
    CInst_t inst = NEW(CInst);
    switch (p->type)
    {
        case NOP: ; break;
        case ID:
            res.kind = VAR;
            res.info.var = p->ext.var;
            break;
        default:
            if (p->ext.is_const)
            {
                res.kind = IMM;
                res.info.imm = p->ext.const_val;
            }
            else
            {
                COpr lhs = ssa_exp(p->chd, cur), rhs;
                if (p->chd->next)
                    rhs = ssa_exp(p->chd->next, cur);
                switch (p->rec.subtype)
                {
                case '=' : 
                    inst->op = MOVE;
                    inst->dest = lhs;
                    inst->src1 = rhs;
                    break;
                case ASS_ADD:
                    inst->op = ADD;
                    inst->dest = lhs;
                    inst->src1 = lhs;
                    inst->src2 = rhs;
                    break;
                    /*
                case ASS_MUL:
                case ASS_DIV:
                case ASS_MOD:
                case ASS_ADD:
                case ASS_SUB:
                case ASS_SHL:
                case ASS_SHR:
                case ASS_AND:
                case ASS_XOR:
                case ASS_OR:
                */
                }
                cblock_append(cur, inst);
                res = inst->dest;
            }
    }
    return res;
}

CBlock_t ssa_stmt(CNode *, CBlock_t, CBlock_t);
CBlock_t ssa_while(CNode *p, CBlock_t cur) {
    CNode *exp = p->chd;
    CBlock_t loop_blk = cblock_create(), loop_t,
             cond_blk,
             next_blk = cblock_create();
    CInst_t j_inst = NEW(CInst),
            if_inst = NEW(CInst);

    loop_t = ssa_stmt(exp->next, loop_blk, next_blk);

    cond_blk = cblock_create();
    DBLINK(loop_t, cond_blk);
    cfg_add_edge(loop_t, cond_blk);

    ssa_exp(exp, cond_blk);

    j_inst->op = GOTO;
    j_inst->dest.kind = IMM;
    j_inst->dest.info.imm = cond_blk->id;
    cond_blk->ref = 1;
    cblock_append(cur, j_inst);

    if_inst->op = BEQZ;
    if_inst->src1 = cblock_getback(cond_blk)->dest;
    if_inst->dest.kind = IMM;
    if_inst->dest.info.imm = loop_blk->id;
    loop_blk->ref = 1;
    cblock_append(cond_blk, if_inst);

    cfg_add_edge(cur, cond_blk);
    cfg_add_edge(cond_blk, loop_blk);
    cfg_add_edge(cond_blk, next_blk);

    DBLINK(cur, loop_blk);
    DBLINK(cond_blk, next_blk);
}

CBlock_t ssa_for(CNode *p, CBlock_t cur) {
    CNode *exp1 = p->chd,
          *exp2 = exp1->next,
          *exp3 = exp2->next;
    CBlock_t loop_blk = cblock_create(), loop_t,
             cond_blk,
             next_blk = cblock_create();
    CInst_t j_inst = NEW(CInst),
            if_inst = NEW(CInst);

    loop_t = ssa_stmt(exp3->next, loop_blk, next_blk);

    cond_blk = cblock_create();
    DBLINK(loop_t, cond_blk);
    cfg_add_edge(loop_t, cond_blk);

    ssa_exp(exp1, cur);
    ssa_exp(exp2, cond_blk);
    ssa_exp(exp3, loop_t);

    j_inst->op = GOTO;
    j_inst->dest.kind = IMM;
    j_inst->dest.info.imm = cond_blk->id;
    cond_blk->ref = 1;
    cblock_append(cur, j_inst);

    if_inst->op = BEQZ;
    if_inst->src1 = cblock_getback(cond_blk)->dest;
    if_inst->dest.kind = IMM;
    if_inst->dest.info.imm = loop_blk->id;
    loop_blk->ref = 1;
    cblock_append(cond_blk, if_inst);

    cfg_add_edge(cur, cond_blk);
    cfg_add_edge(cond_blk, loop_blk);
    cfg_add_edge(cond_blk, next_blk);

    DBLINK(cur, loop_blk);
    DBLINK(cond_blk, next_blk);

    return next_blk;
}

CBlock_t ssa_if(CNode *p, CBlock_t cur, CBlock_t loop_exit) {
    CNode *body1 = p->chd->next,
          *body2 = body1->next;
    CBlock_t then_blk = cblock_create(), then_t,
             next_blk,
             else_blk, else_t;
    CInst_t if_inst = NEW(CInst);
    ssa_exp(p->chd, cur);
    if_inst->op = BEQZ;
    if_inst->src1 = cblock_getback(cur)->dest; /* calculated cond */
    if_inst->dest.kind = IMM;
    cblock_append(cur, if_inst);

    cfg_add_edge(cur, then_blk);
    DBLINK(cur, then_blk);
    then_t = ssa_stmt(body1, then_blk, loop_exit);
    if (body2->type != NOP)
    {
        CInst_t j_inst = NEW(CInst);
        j_inst->op = GOTO;
        j_inst->dest.kind = IMM;

        else_blk = cblock_create();
        if_inst->dest.info.imm = else_blk->id;
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

        j_inst->dest.info.imm = next_blk->id;
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
        if_inst->dest.info.imm = next_blk->id;
        next_blk->ref = 1;
    }
    return next_blk;
}

CBlock_t ssa_comp(CNode *, CBlock_t, CBlock_t loop_exit);
CBlock_t ssa_stmt(CNode *p, CBlock_t cur, CBlock_t loop_exit) {
    switch (p->rec.subtype)
    {
        case STMT_EXP:
            ssa_exp(p->chd, cur);
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
/*            return ssa_while(p, cur);*/
            /*
        case STMT_CONT:
            return ssa_cont(p, cur, loop_exit);
        case STMT_BREAK:
            return ssa_break(p, cur, loop_exit);
        case STMT_RET:
            return ssa_return(p, cur, loop_exit);
            */
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

CBlock_t ssa_func(CType_t func) {
    CBlock_t start = cblock_create();
    ssa_comp(func->rec.func.body, start, NULL);
    return start;
}


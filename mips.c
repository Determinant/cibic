#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "ast.h"
#include "ssa.h"
#include "mips.h"

int reg_v0 = 2;
int reg_v1 = 3;
int memcpy_cnt;
static int save_pos[32];
static int used_reg[32]; 

void mips_prologue(void) {
    CVList_t v;
    CSList_t s;
    int prev = 0;
    printf(".data 0x10000000\n");
    for (v = gvars; v; v = v->next)
    {
        CVar_t var = v->var;
        printf("\t.align 2\n");
        printf("_%s:\n", var->name);
        prev += align_shift(prev);
        var->start = prev;
        prev += calc_size(var->type);
        if (var->initr)
        {
            CNode *initr = var->initr->chd;
            assert(var->initr->rec.subtype == INITR_NORM);
            switch (initr->ext.type->type)
            {
                case CINT:
                    printf("\t.word %ld\n", initr->ext.const_val);
                    break;
                case CCHAR:
                    printf("\t.byte %ld\n", initr->ext.const_val);
                    break;
                case CPTR:
                    {
                        CType_t ref = initr->ext.type->rec.ref;
                        printf("\t.word ");
                        switch (ref->type)
                        {
                            case CFUNC:
                                printf("%s\n", initr->rec.strval);
                                break;
                            case CCHAR:
                                printf("_str_%d\n", ((CSList_t)initr->ext.const_val)->id);
                                break;
                            default: assert(0);
                        }
                    }
                    break;
                default: assert(0);
            }
        }
        else
            printf("\t.space %d\n", calc_size(var->type));
    }
    printf("\t.align 2\n");
    prev += align_shift(prev);
    for (s = cstrs; s; s = s->next)
    {
        int len = 1;
        char *p = s->str;
        printf("_str_%d:\n", s->id);
        printf("\t.asciiz \"%s\"\n", s->str);
        s->start = prev;
        for (; *p != '\0'; p++)
        {
            len++;
            if (*p == '\\')
            {
                switch (*(++p))
                {
                    case '0': p += 3; break;
                    case 'x': p += 2; break;
                    default: ;
                }
            }
        }
        prev += len;
    }
    /* pre-calc done */
    printf(".text\n");
}

#define IN_IMM(x) (-0x8000 <= (x) && (x) < 0x8000)

void mips_global_addr(int reg, CVar_t var) {
    int offset = var->start - 0x8000;
    if (IN_IMM(offset))
        printf("\taddiu $%d, $gp, %d #%s\n", reg, offset, var->name);
    else
        printf("\tla $%d, _%s\n", reg, var->name);
}

void mips_global(const char *l, int reg, CVar_t var) {
    int offset = var->start - 0x8000;
    if (IN_IMM(offset))
        printf("\t%s $%d, %d($gp) #%s\n", l, reg, offset, var->name);
    else
        printf("\t%s $%d, _%s\n", l, reg, var->name);
}

void mips_load(int reg, COpr_t opr) {
    CVar_t var = opr->spill->info.var;
    CType_t type = opr->type;
    if (opr->kind == VAR &&
        (type->type == CSTRUCT ||
        type->type == CUNION ||
        type->type == CARR))
    {
        if (var->loc > 0)
            mips_global_addr(reg, var);
        else
            printf("\taddiu $%d, $sp, %d\n", reg, var->start);
    }
    else
    {
        const char *l = type->type == CCHAR ? "lb" : "lw";
        if (var->loc > 0)
            mips_global(l, reg, var);
        else
            printf("\t%s $%d, %d($sp)\t#%s\n", l, reg, var->start, var->name);
    }
}

void mips_store(int reg, COpr_t opr) {
    CVar_t var = opr->spill->info.var;
    CType_t type = opr->type;
    const char *l = type->type == CCHAR ? "sb" : "sw";
    if (var->loc > 0)
        mips_global(l, reg, var);
    else if (opr->reg == -1)
        printf("\t%s $%d, %d($sp)\t#%s\n", l, reg, var->start, var->name);
}

int mips_to_reg(COpr_t opr, int reg0) {
    if (opr->kind == IMM)
    {
        printf("\tli $%d, %d\n", reg0, opr->info.imm);
        return reg0;
    }
    else if (opr->kind == IMMS)
    {
        CSList_t cstr = opr->info.cstr;
        int offset = cstr->start - 0x8000;
        if (IN_IMM(offset))
            printf("\taddiu $%d, $gp, %d # _str_%d\n", reg0, offset, cstr->id);
        else
            printf("\tla $%d, _str_%d\n", reg0, cstr->id);
        return reg0;
    }
    else if (opr->kind == IMMF)
    {
        printf("\tla $%d, _func_%s\n", reg0, opr->info.str);
        return reg0;
    }
    if (opr->reg != -1) 
        return opr->reg;
    mips_load(reg0, opr);
    return reg0;
}

void mips_memcpy(void) {
    printf("\tj _mem_cond%d\n", memcpy_cnt);
    printf("_mem_loop%d:\n", memcpy_cnt);
    printf("\tlw $5, 0($%d)\n", reg_v0);
    printf("\tsw $5, 0($%d)\n", reg_v1);
    printf("\taddiu $%d, $%d, 4\n", reg_v1, reg_v1);
    printf("\taddiu $%d, $%d, 4\n", reg_v0, reg_v0);
    printf("\taddiu $a0, $a0, -4\n");
    printf("_mem_cond%d:\n", memcpy_cnt);
    printf("\tbnez $a0, _mem_loop%d\n", memcpy_cnt);
    memcpy_cnt++;
}

/* memcpy requires three arguments */
#define RESERVED_CALL_SIZE 12
void mips_space_alloc(void) {
    int local_size = func->rec.func.local_size;
    int arg_size = RESERVED_CALL_SIZE;
    int bret_size = 0;
    int tmp_size = 0;
    int offset = 0;
    int prev = 0;
    int save_size = MAX_AVAIL_REGS * INT_SIZE;
    CBlock_t p;
    COList_t d;
    CVar_t v;
    for (d = defs; d; d = d->next)
    {
        COpr_t opr = d->opr;
        assert(opr->par == opr);
        if (opr->kind == TMP && opr->reg == -1)
        {
            int t = opr->type->type;
            tmp_size += align_shift(tmp_size);
            opr->info.var->start = tmp_size;
            if (t == CSTRUCT || t == CUNION || t == CARR)
                tmp_size += PTR_SIZE;
            else if (t == CVOID)
                tmp_size += INT_SIZE;
            else
                tmp_size += calc_size(opr->type);
        }
    }
    for (p = entry; p; p = p->next)
    {
        CInst_t i, ih = p->insts;
        for (i = ih->next; i != ih; i = i->next)
        {
            if (i->op == PUSH)
            {
                COpr_t arg = i->src1;
                offset += align_shift(offset);
                i->offset = offset;
                if (arg->kind == IMM)
                    offset += INT_SIZE;
                else if (arg->kind == IMMS)
                    offset += PTR_SIZE;
                else if (arg->kind == IMMF)
                    offset += PTR_SIZE;
                else
                {
                    CType_t t = arg->type;
                    if (t->type == CARR)
                        offset += PTR_SIZE;
                    else
                        offset += calc_size(t);
                }
            }
            else if (i->op == CALL)
            {
                CType_t rt = i->dest->type;
                if (offset > arg_size)
                    arg_size = offset;
                offset = 0;
                if (rt->type == CSTRUCT || rt->type == CUNION)
                {
                    bret_size += align_shift(bret_size);
                    i->bret = bret_size;
                    bret_size += calc_size(rt);
                }
                else i->bret = -1;
            }
        }
    }
    prev += arg_size;
    prev += align_shift(prev);
    /* adjust offset for local variables */
    for (v = func->rec.func.local; v; v = v->next)
        v->start += prev;
    prev += local_size;
    prev += align_shift(prev);
    {
        int i;
        for (i = 0; i < MAX_AVAIL_REGS; i++)
            save_pos[avail_regs[i]] = prev + i * INT_SIZE;
        save_pos[30] = prev + save_size;
        save_size += INT_SIZE;
    }
    prev += save_size;
    /* adjust offset for spilled temporaries */
    for (d = defs; d; d = d->next)
    {
        COpr_t opr = d->opr;
        assert(opr->par == opr);
        if (opr->kind == TMP && opr->reg == -1)
            opr->info.var->start += prev;
    }
    prev += tmp_size;
    prev += align_shift(prev);
    for (p = entry; p; p = p->next)
    {
        CInst_t i, ih = p->insts;
        for (i = ih->next; i != ih; i = i->next)
            if (i->op == CALL && i->bret != -1)
                i->bret += prev;
    }
    prev += bret_size;
    prev += align_shift(prev);
    prev += 4;  /* return address */
    for (v = func->rec.func.params; v; v = v->next)
        v->start += prev; /* skip the whole frame to reach args */
    func->rec.func.frame_size = prev;
}

void mips_func_begin(void) {
    int fsize = func->rec.func.frame_size;
    if (fsize < 0x8000)
        printf("\taddiu $sp, $sp, -%d\n", fsize);
    else
    {
        printf("\tli $%d, %d\n", reg_v0, -fsize);
        printf("\taddu $sp, $sp, $%d\n", reg_v0);
    }
    printf("\tsw $31, %d($sp) #%s\n", fsize - 4, func->name);
}

void mips_func_end(void) {
    int fsize = func->rec.func.frame_size;
    printf("_ret_%s:\n", func->name);
    printf("\tlw $31, %d($sp)\n", fsize - 4);
    if (fsize < 0x8000)
        printf("\taddiu $sp, $sp, %d\n", fsize);
    else
    {
        printf("\tli $%d, %d\n", reg_v0, fsize);
        printf("\taddu $sp, $sp, $%d\n", reg_v0);
    }
    printf("\tjr $31\n");
}

void mips_generate(void) {
    CBlock_t p;
    CType_t rt = func->rec.func.ret;
    /* int arg_cnt = 0; */
    mips_space_alloc();
    if (strcmp(func->name, "main"))
        printf("_func_%s:\n",func->name);
    else
        printf("main:\n");
    mips_func_begin();
    for (p = entry; p; p = p->next)
    {
        if (p->ref) printf("_L%d:\n", p->id + gbbase);
        CInst_t i, ih = p->insts;
        const char *bop;
        for (i = ih->next; i != ih; i = i->next)
        {
            int flag = 1, swap, rimm;
            if (i->dest && i->dest->reg == -2 && i->dest->kind == TMP && i->op != CALL)
                continue;
            printf("# ");
            cinst_print(stdout, i);
            switch (i->op)
            {
                case LOAD:
                    if (i->dest->kind == VAR &&
                            i->dest->reg > 0) /* && 
                             i->dest->info.var->loc >= 0) */
                        mips_load(i->dest->reg, i->dest);
                    break;
                case MOVE:
                    {
                        int rs;
                        int rd = i->dest->reg;
                        CType_t type = i->dest->type;
                        if ((type->type == CSTRUCT || type->type == CUNION) && i->dest->kind == VAR)
                        {
                            rs = mips_to_reg(i->src1, reg_v0);
                            /* assert(i->dest->kind == VAR); */
                            printf("\tsw $%d, 4($sp)\n", rs);
                            if (rd < 0) rd = reg_v0;
                            mips_load(rd, i->dest);
                            printf("\tsw $%d, 0($sp)\n", rd);
                            printf("\tli $%d, %d\n", reg_v0, calc_size(type));
                            printf("\tsw $%d, 8($sp)\n", reg_v0);
                            /* don't need to save current registers because
                             * memcpy doesn't interfere with them */
                            printf("\tjal _func_memcpy\n");
                        }
                        else
                        {
                            if (i->dest->reg > 0 && i->src1->kind == IMM)
                            {
                                printf("\tli $%d %d\n", i->dest->reg, i->src1->info.imm);
                            }
                            else
                            {
                                rs = mips_to_reg(i->src1, reg_v0);
                                if (rd > 0)
                                {
                                    if (rd != rs) printf("\tmove $%d $%d\n", rd, rs);
                                }
                                else
                                    rd = rs;
                            }
                            if (i->dest->reg == -1 ||
                                (i->dest->kind == VAR &&
                                 i->dest->info.var->loc > 0))
                                mips_store(rd, i->dest);
                        }
                    }
                    break;
                case BEQ:
                case BNE:
                    {
                        int rs = mips_to_reg(i->src1, reg_v0);
                        int rt;
                        const char *b = i->op == BEQ ? "beq" : "bne";
                        if (i->src2->kind == IMM)
                            printf("\t%s $%d, %d, _L%d\n", b, rs, i->src2->info.imm,
                                                                i->dest->info.imm);
                        else
                        {
                            rt = mips_to_reg(i->src2, reg_v1);
                            printf("\t%s $%d, $%d, _L%d\n", b, rs, rt, i->dest->info.imm);
                        }
                    }
                    break;
                case GOTO:
                    printf("\tj _L%d\n", i->dest->info.imm);
                    break;
                case ARR:
                    {
                        CType_t type = i->src1->type;
                        int arr = mips_to_reg(i->src1, reg_v0);
                        int rd;
                        const char *l;
                        if (type->type == CARR)
                            type = type->rec.arr.elem;
                        else
                            type = type->rec.ref;
                        l = type->type == CCHAR ? "lb" : "lw";
                        if (i->src2->kind == IMM)
                        {
                            int index = i->src2->info.imm;
                            rd = i->dest->reg;
                            if (rd < 0) rd = reg_v1;
                            printf("\t%s $%d, %d($%d)\n", l, rd, index, arr);
                        }
                        else
                        {
                            int index = mips_to_reg(i->src2, reg_v1);
                            rd = i->dest->reg;
                            if (rd < 0) rd = reg_v0;
                            printf("\taddu $%d, $%d, $%d\n", reg_v1, arr, index);
                            printf("\t%s $%d, 0($%d)\n", l, rd, reg_v1);
                        }
                        if (i->dest->reg == -1 || i->dest->kind == VAR)
                            mips_store(rd, i->dest);
                    }
                    break;
                case WARR:
                    {
                        CType_t type = i->wtype;
                        int arr = mips_to_reg(i->dest, reg_v1);
                        const char *s;
                        int rs;
                        /*
                        if (type->type == CARR)
                            type = type->rec.arr.elem;
                        else
                            type = type->rec.ref;
                            */
                        s = type->type == CCHAR ? "sb" : "sw";

                        if (type->type == CSTRUCT || type->type == CUNION)
                        {
                            if (i->src2->kind == IMM)
                                printf("\taddiu $%d, $%d, %d\n", reg_v1, arr, i->src2->info.imm);
                            else
                            {
                                int index = mips_to_reg(i->src2, reg_v0);
                                printf("\taddu $%d, $%d, $%d\n", reg_v1, arr, index);
                            }
                            rs = mips_to_reg(i->src1, reg_v0);
                            printf("\tmove $%d, $%d\n", reg_v0, rs);
                            printf("\tli $a0, %d\n", calc_size(type));
                            mips_memcpy();
                        }
                        else
                        {
                            if (i->src2->kind == IMM)
                            {
                                rs = mips_to_reg(i->src1, reg_v0);
                                if (type->type == CSTRUCT || type->type == CUNION)
                                {
                                    printf("\tmove $%d, $%d\n", reg_v0, rs);
                                    printf("\taddiu $%d, $%d, $%d\n", reg_v1, arr, i->src2->info.imm);
                                    printf("\tli $a0, %d\n", calc_size(type));
                                    mips_memcpy();
                                }
                                else
                                    printf("\t%s $%d, %d($%d)\n", s, rs, i->src2->info.imm, arr);
                            }
                            else
                            {
                                int index = mips_to_reg(i->src2, reg_v0);
                                printf("\taddu $%d, $%d, $%d\n", reg_v0, arr, index);
                                rs = mips_to_reg(i->src1, reg_v1);
                                printf("\t%s $%d, 0($%d)\n", s, rs, reg_v0);
                            }
                        }
                    }
                    break;
                case PUSH:
                    {
                        int rs = mips_to_reg(i->src1, reg_v0);
                        /*
                        if (arg_cnt < 4)
                        {
                            printf("\tmove $%d, $%d\n", arg_cnt + 4, rs);
                        }
                        else
                        {
                        */
                        CType_t type = i->src1->type;
                        if (type && (type->type == CSTRUCT || type->type == CUNION))
                        {
                            printf("\taddiu $%d, $sp, %d\n", reg_v1, i->offset);
                            printf("\tmove $%d, $%d\n", reg_v0, rs);
                            printf("\tli $a0, %d\n", calc_size(type));
                            mips_memcpy();
                        }
                        else
                            printf("\tsw $%d, %d($sp) # push\n", rs, i->offset);
                        /* } 
                        arg_cnt++; */
                    }
                    break;
                case CALL:
                    {
                        COList_t p;
                        int rd = i->dest->reg;
                        int j;
                        memset(used_reg, 0, sizeof used_reg);
                        if (rt->type == CSTRUCT || rt->type == CUNION)
                            used_reg[30] = 1; /* save $fp */
                        for (p = defs; p; p = p->next)
                        {
                            COpr_t opr = p->opr;
                            if (opr->reg != -1 && 
                                (opr->kind == TMP || !(opr->info.var->loc > 0)) &&
                                overlap_with_beg(opr, i->id))
                                used_reg[opr->reg] = 1;
                        }
                        for (j = 0; j < 32; j++)
                            if (used_reg[j])
                                printf("\tsw $%d, %d($sp) # save reg\n", j, save_pos[j]);
                        if (i->bret != -1)
                        {
                            if (i->dest->kind == TMP)
                                printf("\taddiu $fp, $sp, %d\n", i->bret);
                            else
                                mips_load(30, i->dest);
                        }
                        if (i->src1->kind == IMMF)
                            printf("\tjal %s%s\n", 
                                    strcmp(i->src1->info.str, "main") ? "_func_" : "",
                                    i->src1->info.str);
                        else
                            printf("\tjalr $%d\n", mips_to_reg(i->src1, reg_v0));
                        for (j = 0; j < 32; j++)
                            if (used_reg[j])
                                printf("\tlw $%d, %d($sp) # load reg\n", j, save_pos[j]);
                        for (p = defs; p; p = p->next)
                        {
                            COpr_t opr = p->opr;
                            if (opr->reg != -1 &&
                                opr->kind == VAR &&
                                opr->info.var->loc > 0 &&
                                overlap_with_beg(opr, i->id))
                                mips_load(opr->reg, opr);
                        }
                        if (rd != -2)
                        {
                            if (i->bret != -1 && i->dest->kind == VAR)
                            {
                                if (rd == -1) 
                                    rd = mips_to_reg(i->dest, reg_v1);
                                if (reg_v1 != rd)
                                    printf("\tmove $%d, $%d\n", reg_v1, rd);
                                printf("\tli $a0, %d\n", calc_size(i->dest->type));
                                mips_memcpy();
                            }
                            else
                            {
                                if (rd != -1) 
                                    printf("\tmove $%d, $%d\n", rd, reg_v0);
                                if (i->dest->reg == -1 || i->dest->kind == VAR)
                                    mips_store(reg_v0, i->dest);
                            }
                        }
                    }
                    break;
                case RET:
                    {
                        if (i->src1)
                        {
                            if (rt->type == CSTRUCT || rt->type == CUNION)
                            {
                                int rs = mips_to_reg(i->src1, reg_v0);
                                assert(i->src1->kind == VAR || i->src1->kind == TMP);
                                printf("\tsw $%d, 4($sp)\n", rs);
                                printf("\tsw $fp, 0($sp)\n");
                                printf("\tli $%d, %d\n", reg_v0, calc_size(rt));
                                printf("\tsw $%d, 8($sp)\n", reg_v0);
                                printf("\tjal _func_memcpy\n");
                                printf("\tmove $%d, $fp\n", reg_v0);
                            }
                            else
                            {
                                if (i->src1->reg != -1)
                                {
                                    if (i->src1->kind == IMM)
                                        printf("\tli $%d, %d\n", reg_v0, i->src1->info.imm);
                                    else
                                        printf("\tmove $%d, $%d\n", reg_v0, mips_to_reg(i->src1, reg_v1));
                                }
                                else
                                {
                                    mips_to_reg(i->src1, reg_v0);
                                }
                            }
                        }
                        printf("\tj _ret_%s\n", func->name);
                    }
                    break;
                case ADDR:
                    {
                        assert(i->src1->kind == VAR ||
                                i->src1->kind == IMMF);
                        int rd = i->dest->reg;
                        if (rd < 0) rd = reg_v0;
                        if (i->src1->kind == IMMF)
                            printf("\tla $%d, %s\n", rd, i->src1->info.str);
                        else
                        {
                            CVar_t var = i->src1->spill->info.var;
                            if (var->loc > 0)
                                mips_global_addr(rd, var);
                            else
                                printf("\taddiu $%d, $sp, %d\n", rd, var->start);
                            if (i->dest->reg == -1 || i->dest->kind == VAR)
                                mips_store(rd, i->dest);
                        }
                    }
                    break;
                default: flag = 0;
            }
            if (flag) continue;
            flag = 1;
            swap = rimm = 0;
            switch (i->op)
            {
                case MUL: bop = "mul"; rimm = 1; break;
                case DIV: bop = "divu"; rimm = 1; break;
                case MOD: bop = "rem"; rimm = 1; break;
                case ADD: bop = "addu"; rimm = swap = 1; break;
                case SUB: bop = "subu"; rimm = 1; break;
                case SHL: bop = "sllv"; rimm = 1; break;
                case SHR: bop = "srlv"; rimm = 1; break;
                case AND: bop = "and"; rimm = swap = 1; break;
                case XOR: bop = "xor"; rimm = swap = 1; break;
                case OR: bop = "or"; rimm = swap = 1; break;
                case NOR: bop = "nor"; rimm = 1; break;
                case EQ: bop = "seq"; rimm = 1; break;
                case NE: bop = "sne"; rimm = 1; break;
                case LT: bop = "slt"; rimm = 1; break;
                case GT: bop = "sgt"; rimm = 1; break;
                case LE: bop = "sle"; rimm = 1; break;
                case GE: bop = "sge"; rimm = 1; break;
                default: flag = 0;
            }
            if (flag)
            {
                int rs, rt;
                int rd = i->dest->reg;
                if (rd < 0) rd = reg_v0;
                if (rimm)
                {
                    COpr_t t;
                    if (swap && i->src1->kind == IMM)
                    {
                        t = i->src1;
                        i->src1 = i->src2;
                        i->src2 = t;
                    }
                    if (i->src2->kind == IMM && IN_IMM(i->src2->info.imm))
                    {
                        switch (i->op)
                        {
                            case ADD: bop = "addiu"; break;
                            case AND: bop = "andi"; break;
                            case XOR: bop = "xori"; break;
                            case OR: bop = "ori"; break;
                            case SHL: bop = "sll"; break;
                            case SHR: bop = "srl"; break;
                            default: ;
                        }
                        rs = mips_to_reg(i->src1, reg_v0);
                        printf("\t%s $%d, $%d, %d\n",
                                bop, rd, rs, i->src2->info.imm);
                    }
                    else rimm = 0;
                }
                if (!rimm)
                {
                    rs = mips_to_reg(i->src1, reg_v0);
                    rt = mips_to_reg(i->src2, reg_v1);
                    printf("\t%s $%d, $%d, $%d\n", bop, rd, rs, rt);
                }
                if (i->dest->reg == -1 || i->dest->kind == VAR)
                    mips_store(rd, i->dest);
                continue;
            }
            if (i->op == NEG)
            {
                int rs = mips_to_reg(i->src1, reg_v0);
                int rd = i->dest->reg;
                if (rd < 0) rd = reg_v0;
                printf("\tnegu $%d, $%d\n", rd, rs);
                if (i->dest->reg == -1 || i->dest->kind == VAR)
                    mips_store(rd, i->dest);
            }
        }
    }
    mips_func_end();
}

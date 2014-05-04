_func_printf:
	addiu $sp, $sp, -64
	sw $31, 60($sp) #printf
# load pos_0
# load arg_0
# load ch_0
	lb $8, 12($sp)	#ch
# load fmt_0
	lw $9, 64($sp)	#fmt
# load x_0
	lw $10, 4($sp)	#x
# load len_0
	lw $11, 8($sp)	#len
# arg_1 = addr pos_0
	addiu $12, $sp, 68
# goto __L2
	j __L2
__L1:
# t2 = ch_2 == 37
	li $3, 37
	seq $13, $8, $3
# if not (t2) goto __L20
	beqz $13, __L20
# fmt_4 = fmt_1 + 1
	addiu $9, $9, 1
# ch_4 = fmt_4[0]
	lb $8, 0($9)
# t4 = ch_4 == 100
	li $3, 100
	seq $13, $8, $3
# if not (t4) goto __L6
	beqz $13, __L6
# t6 = arg_2[0]
	lw $a0, 0($12)
    li $2, 1
    syscall
# goto __L19
	j __L19
__L6:
# t7 = ch_4 == 99
	li $3, 99
	seq $13, $8, $3
# if not (t7) goto __L8
	beqz $13, __L8
# t9 = arg_2[0]
	lw $a0, 0($12)
    li $2, 11
    syscall
# goto __L19
	j __L19
__L8:
# t10 = ch_4 == 115
	li $3, 115
	seq $13, $8, $3
# if not (t10) goto __L10
	beqz $13, __L10
# t12 = arg_2[0]
	lw $a0, 0($12)
    li $2, 4
    syscall
# goto __L19
	j __L19
__L10:
# x_4 = arg_2[0]
	lw $10, 0($12)
# t14 = x_4 == 0
	li $3, 0
	seq $11, $10, $3
# if not (t14) goto __L12
	beqz $11, __L12
# len_11 = 1
	li $2, 1
	move $11 $2
# goto __L15
	j __L15
__L12:
# len_8 = 0
	li $2, 0
	move $11 $2
# goto __L14
	j __L14
__L13:
# x_7 = x_6 / 10
	li $3, 10
	divu $10, $10, $3
# len_10 = len_9 + 1
	addiu $11, $11, 1
__L14:
# if (x_6) goto __L13
	bnez $10, __L13
__L15:
# len_5 = 4 - len_4
	li $2, 4
	subu $11, $2, $11
# goto __L17
	j __L17
__L16:
# push 48
	li $a0 48
    li $2, 11
    syscall
# len_7 = len_6 - 1
	li $3, 1
	subu $11, $11, $3
__L17:
# if (len_6) goto __L16
	bnez $11, __L16
# t18 = arg_2[0]
	lw $a0, 0($12)
    li $2, 1
    syscall
# fmt_6 = fmt_4 + 2
	addiu $9, $9, 2
__L19:
# arg_4 = arg_2 + 4
	addiu $12, $12, 4
# goto __L21
	j __L21
__L20:
# push ch_2
    move $a0, $8
    li $2, 11
    syscall
__L21:
# fmt_3 = fmt_2 + 1
	addiu $9, $9, 1
__L2:
# ch_2 = fmt_1[0]
	lb $8, 0($9)
# if (ch_2) goto __L1
	bnez $8, __L1
_ret_printf:
	lw $31, 60($sp)
	addiu $sp, $sp, 64
	jr $31
_func___print_int:
    lw $a0, 0($sp)
    li $2, 1
    syscall
    jr $31
_func___print_char:
    lb $a0, 0($sp)
    li $2, 11
    syscall
    jr $31
_func___print_string:
    lw $a0, 0($sp)
    li $2, 4
    syscall
    jr $31
_func_malloc:
    lw $a0, 0($sp)
    li $2, 9
    syscall
    jr $31
_func_memcpy:       # the copied mem must be 4-aligned
    lw $4, 0($sp)   # dest addr
    lw $5, 4($sp)   # src addr
    lw $6, 8($sp)  # size
    j __COND
__LOOP:
    lw $2, 0($5)
    sw $2, 0($4)
    addiu $4, $4, 4
    addiu $5, $5, 4
    addiu $6, $6, -4
__COND:
    bnez $6, __LOOP
    jr $31

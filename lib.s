_func_printf:
	addiu $sp, $sp, -64
	sw $31, 60($sp) #printf
# load fmt_0
	lw $8, 64($sp)	#fmt
# load arg_0
# load ch_0
	lb $9, 12($sp)	#ch
# load x_0
	lw $10, 4($sp)	#x
# load len_0
	lw $11, 8($sp)	#len
# t0 = addr fmt_0
	addiu $12, $sp, 64
# arg_1 = t0 + 4
	addiu $12, $12, 4
# goto __L2
	j __L2
__L1:
# t3 = ch_2 == 37
	li $3, 37
	seq $13, $9, $3
# if not (t3) goto __L20
	beqz $13, __L20
# fmt_4 = fmt_1 + 1
	addiu $8, $8, 1
# ch_4 = fmt_4[0]
	lb $9, 0($8)
# t5 = ch_4 == 100
	li $3, 100
	seq $13, $9, $3
# if not (t5) goto __L6
	beqz $13, __L6
# t7 = arg_2[0]
	lw $13, 0($12)
# push t7
	sw $13, 0($sp) # push
# t6 = call __print_int
	jal _func___print_int
# goto __L19
	j __L19
__L6:
# t8 = ch_4 == 99
	li $3, 99
	seq $13, $9, $3
# if not (t8) goto __L8
	beqz $13, __L8
# t10 = arg_2[0]
	lw $13, 0($12)
# push t10
	sw $13, 0($sp) # push
# t9 = call __print_char
	jal _func___print_char
# goto __L19
	j __L19
__L8:
# t11 = ch_4 == 115
	li $3, 115
	seq $13, $9, $3
# if not (t11) goto __L10
	beqz $13, __L10
# t13 = arg_2[0]
	lw $13, 0($12)
# push t13
	sw $13, 0($sp) # push
# t12 = call __print_string
	jal _func___print_string
# goto __L19
	j __L19
__L10:
# x_4 = arg_2[0]
	lw $10, 0($12)
# t15 = x_4 == 0
	li $3, 0
	seq $11, $10, $3
# if not (t15) goto __L12
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
	li $2, 48
	sw $2, 0($sp) # push
# t17 = call __print_char
	jal _func___print_char
# len_7 = len_6 - 1
	li $3, 1
	subu $11, $11, $3
__L17:
# if (len_6) goto __L16
	bnez $11, __L16
# t19 = arg_2[0]
	lw $13, 0($12)
# push t19
	sw $13, 0($sp) # push
# t18 = call __print_int
	jal _func___print_int
# fmt_6 = fmt_4 + 2
	addiu $8, $8, 2
__L19:
# arg_4 = arg_2 + 4
	addiu $12, $12, 4
# goto __L21
	j __L21
__L20:
# push ch_2
	sw $9, 0($sp) # push
# t20 = call __print_char
	jal _func___print_char
__L21:
# fmt_3 = fmt_2 + 1
	addiu $8, $8, 1
__L2:
# ch_2 = fmt_1[0]
	lb $9, 0($8)
# if (ch_2) goto __L1
	bnez $9, __L1
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

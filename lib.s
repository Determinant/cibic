_func_printf:
	addiu $sp, $sp, -88
	sw $31, 84($sp) #printf
# load fmt_0
# load arg_0
# load len_0
# load ch_0
# load x_0
# t0 = addr fmt_0
	addiu $2, $sp, 88
	sw $2, 80($sp)	#t0
# arg_1 = t0 + 4
	lw $2, 80($sp)	#t0
	addiu $2, $2, 4
	sw $2, 16($sp)	#arg
# goto __L2
	j __L2
__L1:
# t3 = ch_2 == 37
	lb $2, 12($sp)	#ch
	li $3, 37
	seq $2, $2, $3
	sw $2, 76($sp)	#t3
# if not (t3) goto __L20
	lw $2, 76($sp)	#t3
	beqz $2, __L20
# fmt_4 = fmt_1 + 1
	lw $2, 88($sp)	#fmt
	addiu $2, $2, 1
	sw $2, 88($sp)	#fmt
# ch_4 = fmt_4[0]
	lw $2, 88($sp)	#fmt
	lb $3, 0($2)
	sb $3, 12($sp)	#ch
# t5 = ch_4 == 100
	lb $2, 12($sp)	#ch
	li $3, 100
	seq $2, $2, $3
	sw $2, 72($sp)	#t5
# if not (t5) goto __L6
	lw $2, 72($sp)	#t5
	beqz $2, __L6
# t7 = arg_2[0]
	lw $2, 16($sp)	#arg
	lw $3, 0($2)
	sw $3, 68($sp)	#t7
# push t7
	lw $2, 68($sp)	#t7
	sw $2, 0($sp) # push
# t6 = call __print_int
	jal _func___print_int
	sw $2, 64($sp)	#t6
# goto __L19
	j __L19
__L6:
# t8 = ch_4 == 99
	lb $2, 12($sp)	#ch
	li $3, 99
	seq $2, $2, $3
	sw $2, 60($sp)	#t8
# if not (t8) goto __L8
	lw $2, 60($sp)	#t8
	beqz $2, __L8
# t10 = arg_2[0]
	lw $2, 16($sp)	#arg
	lw $3, 0($2)
	sb $3, 56($sp)	#t10
# push t10
	lb $2, 56($sp)	#t10
	sw $2, 0($sp) # push
# t9 = call __print_char
	jal _func___print_char
	sw $2, 52($sp)	#t9
# goto __L19
	j __L19
__L8:
# t11 = ch_4 == 115
	lb $2, 12($sp)	#ch
	li $3, 115
	seq $2, $2, $3
	sw $2, 48($sp)	#t11
# if not (t11) goto __L10
	lw $2, 48($sp)	#t11
	beqz $2, __L10
# t13 = arg_2[0]
	lw $2, 16($sp)	#arg
	lw $3, 0($2)
	sw $3, 44($sp)	#t13
# push t13
	lw $2, 44($sp)	#t13
	sw $2, 0($sp) # push
# t12 = call __print_string
	jal _func___print_string
	sw $2, 40($sp)	#t12
# goto __L19
	j __L19
__L10:
# x_4 = arg_2[0]
	lw $2, 16($sp)	#arg
	lw $3, 0($2)
	sw $3, 4($sp)	#x
# t15 = x_4 == 0
	lw $2, 4($sp)	#x
	li $3, 0
	seq $2, $2, $3
	sw $2, 36($sp)	#t15
# if not (t15) goto __L12
	lw $2, 36($sp)	#t15
	beqz $2, __L12
# len_11 = 1
	li $2, 1
	sw $2, 8($sp)	#len
# goto __L15
	j __L15
__L12:
# len_8 = 0
	li $2, 0
	sw $2, 8($sp)	#len
# goto __L14
	j __L14
__L13:
# x_7 = x_6 / 10
	lw $2, 4($sp)	#x
	li $3, 10
	divu $2, $2, $3
	sw $2, 4($sp)	#x
# len_10 = len_9 + 1
	lw $2, 8($sp)	#len
	addiu $2, $2, 1
	sw $2, 8($sp)	#len
__L14:
# if (x_6) goto __L13
	lw $2, 4($sp)	#x
	bnez $2, __L13
__L15:
# len_5 = 4 - len_4
	li $2, 4
	lw $3, 8($sp)	#len
	subu $2, $2, $3
	sw $2, 8($sp)	#len
# goto __L17
	j __L17
__L16:
# push 48
	li $2, 48
	sw $2, 0($sp) # push
# t17 = call __print_char
	jal _func___print_char
	sw $2, 32($sp)	#t17
# len_7 = len_6 - 1
	lw $2, 8($sp)	#len
	li $3, 1
	subu $2, $2, $3
	sw $2, 8($sp)	#len
__L17:
# if (len_6) goto __L16
	lw $2, 8($sp)	#len
	bnez $2, __L16
# t19 = arg_2[0]
	lw $2, 16($sp)	#arg
	lw $3, 0($2)
	sw $3, 28($sp)	#t19
# push t19
	lw $2, 28($sp)	#t19
	sw $2, 0($sp) # push
# t18 = call __print_int
	jal _func___print_int
	sw $2, 24($sp)	#t18
# fmt_6 = fmt_4 + 2
	lw $2, 88($sp)	#fmt
	addiu $2, $2, 2
	sw $2, 88($sp)	#fmt
__L19:
# arg_4 = arg_2 + 4
	lw $2, 16($sp)	#arg
	addiu $2, $2, 4
	sw $2, 16($sp)	#arg
# goto __L21
	j __L21
__L20:
# push ch_2
	lb $2, 12($sp)	#ch
	sw $2, 0($sp) # push
# t20 = call __print_char
	jal _func___print_char
	sw $2, 20($sp)	#t20
__L21:
# fmt_3 = fmt_2 + 1
	lw $2, 88($sp)	#fmt
	addiu $2, $2, 1
	sw $2, 88($sp)	#fmt
__L2:
# ch_2 = fmt_1[0]
	lw $2, 88($sp)	#fmt
	lb $3, 0($2)
	sb $3, 12($sp)	#ch
# if (ch_2) goto __L1
	lb $2, 12($sp)	#ch
	bnez $2, __L1
_ret_printf:
	lw $31, 84($sp)
	addiu $sp, $sp, 88
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

ifeq ($(mode), release)
	CFLAGS = -O2 -Wall
else
	CFLAGS = -g -Wall -Wextra -DCIBIC_DEBUG
endif

all: cibic

db:
	gdb cibic

cibic: lex.yy.o cibic.tab.o ast.o main.o semantics.o ssa.o mips.o
	mkdir -p bin
	gcc -o bin/cibic lex.yy.o cibic.tab.o ast.o main.o semantics.o ssa.o mips.o
	cp bin/cibic cibic
lex.yy.o: lex.yy.c cibic.tab.c
	gcc -c lex.yy.c
cibic.tab.o: cibic.tab.c
	gcc -c cibic.tab.c
main.o: main.c
#	gcc -c main.c -g -Wall -Wextra
ast.o:	ast.c ast.h
#	gcc -c ast.c -g -Wall -Wextra -DCIBIC_DEBUG
semantics.o: semantics.c semantics.h
#	gcc -c semantics.c -g -Wall -Wextra -DCIBIC_DEBUG
ssa.o:	ssa.c ssa.h
#	gcc -c ssa.c -g -Wall -Wextra -DCIBIC_DEBUG
mips.o: mips.c mips.h
#	gcc -c mips.c -g -Wall -Wextra -DCIBIC_DEBUG
lex.yy.c: cibic.l
	flex cibic.l
cibic.tab.c: cibic.y
	bison -d cibic.y

clean:
	rm -f cibic lex.yy.c cibic.tab.c cibic.tab.h *.o

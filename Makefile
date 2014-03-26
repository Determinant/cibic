all: cibic

run: 
	./cibic --ast

db:
	gdb cibic

cibic: lex.yy.o cibic.tab.o ast.o main.o
	gcc -o cibic lex.yy.o cibic.tab.o ast.o main.o
lex.yy.o: lex.yy.c
	gcc -c lex.yy.c
cibic.tab.o: cibic.tab.c
	gcc -c cibic.tab.c
main.o: main.c
	gcc -c main.c -g -Wall -Wextra
ast.o:	ast.c
	gcc -c ast.c -g -Wall -Wextra -DCIBIC_DEBUG
lex.yy.c: cibic.l
	flex cibic.l
cibic.tab.c: cibic.y
	bison -d cibic.y

clean:
	rm -f cibic lex.yy.c cibic.tab.c *.o

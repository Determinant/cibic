all: cibic

run: 
	./cibic

debug:
	gdb cibic

cibic: lex.yy.o cibic.tab.o ast.o
	gcc -o cibic lex.yy.o cibic.tab.o ast.o
lex.yy.o: lex.yy.c
	gcc -c lex.yy.c -Wall -Wextra
cibic.tab.o: cibic.tab.c
	gcc -c cibic.tab.c -Wall -Wextra
ast.o:	ast.c
	gcc -c ast.c -g -Wall -Wextra
lex.yy.c: cibic.l
	flex cibic.l
cibic.tab.c: cibic.y
	bison -d cibic.y

clean:
	rm -f cibic lex.yy.c cibic.tab.c *.o

all: cibic

run: 
	./cibic

cibic: lex.yy.o cibic.tab.o ast.o
	gcc -o cibic lex.yy.o cibic.tab.o ast.o
lex.yy.o: lex.yy.c
	gcc -c lex.yy.c
cibic.tab.o: cibic.tab.c
	gcc -c cibic.tab.c
ast.o:	ast.c
	gcc -c ast.c -g
lex.yy.c: cibic.l
	flex cibic.l
cibic.tab.c: cibic.y
	bison -d cibic.y

clean:
	rm -f cibic lex.yy.c cibic.tab.c *.o

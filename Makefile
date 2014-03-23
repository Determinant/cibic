all: cibic

run: 
	./cibic

cibic: lex.yy.c cibic.tab.c
	gcc -o cibic lex.yy.c cibic.tab.c
lex.yy.c: cibic.l
	flex cibic.l
cibic.tab.c: cibic.y
	bison -d cibic.y

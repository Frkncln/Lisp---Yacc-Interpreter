
all:  gety

#getz: gety
#	./a.out

gety:getf
	gcc lex.yy.c y.tab.c -w

getf: gpp_lexer.c
	yacc -d gpp_interpreter.y


gpp_lexer.c: gpp_lexer.l
	lex gpp_lexer.l
 


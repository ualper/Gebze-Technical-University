all:	
	lex gpp_interpreter.l
	yacc -d gpp_interpreter.y
	gcc -std=c99 lex.yy.c y.tab.c -w -lm -o gpp_interpreter.out
	
clean:
	rm gpp_interpreter.out
	rm parsed_cpp.txt



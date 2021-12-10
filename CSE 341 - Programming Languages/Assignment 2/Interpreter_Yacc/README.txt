
ATTENTION ! --> PROGRAM WILL HALT AT SYNTAX ERROR.

# This code must be run like this order
# -------------------------------------
# 1 :  yacc -y -d gpp_interpreter.y
# 2 : flex gpp_lexer.l
# 3 : gcc gpp_lib.c lex.yy.c y.tab.c -o GPPINTERPRETER -g -lm
# 4 : ./GPPINTERPRETER
# -------------------------------------

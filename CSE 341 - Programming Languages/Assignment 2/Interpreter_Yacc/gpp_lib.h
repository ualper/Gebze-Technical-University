#ifndef __gpp_lib_h_
#define __gpp_lib_h_

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "y.tab.h"

int yyparse(void);
int yylex(void);
void yyerror(char *);

int opPlus(int, int);
int opMinus(int, int);
int opMultiply(int, int);
int opDivide(int, int);
int opDoubleAsteriks(int, int);

void ImplementArray(int);
void printList();
void Append(int, int *);
void Concatenate(int *, int *);

char *opAnd(char *, char *);
char *opOr(char *, char *);
char *opNot(char *);
char *opEqualStr(char *, char *);
char *opEqualInt(int, int);
char *opLess(int, int);
char *returnString(char *);

#endif
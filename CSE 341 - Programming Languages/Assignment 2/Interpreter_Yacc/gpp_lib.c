#include "gpp_lib.h"
#define SIZE__ARRAY 9999

int array[SIZE__ARRAY];
int current = 0;

int main(void)
{
    for (int i = 0; i < SIZE__ARRAY; ++i)
    {
        array[i] = -999;
    }
    current = 0;

    yyparse();
    return 0;
}

void yyerror(char *s)
{
    fprintf(stderr, "%s\n", s);
}

int opPlus(int op1, int op2)
{
    int temp;
    temp = op1 + op2;
    return temp;
}

int opMimus(int op1, int op2)
{
    int temp;
    temp = op1 - op2;
    return temp;
}

int opMultiply(int op1, int op2)
{
    int temp;
    temp = op1 * op2;
    return temp;
}

int opDoubleAsteriks(int op1, int op2)
{
    int result = 1;
    for (int i = 0; i < op2; ++i)
    {
        result *= op1;
    }
    return result;
}

int opDivide(int op1, int op2)
{
    int temp;
    temp = (int)(op1 / op2);
    return temp;
}

char *opEqualInt(int op1, int op2)
{
    if (op1 == op2)
        return "true";
    return "false";
}

void ImplementArray(int val)
{
    array[current] = val;
    ++current;
}

void Append(int val, int *arr)
{
    array[current] = val;
    ++current;
}

void Concatenate(int *arr1, int *arr2) {}

void printList()
{
    if (current == 0)
    {
        printf("NIL\n");
    }
    else
    {
        printf("( ");
        for (int i = 0; i < current; ++i)
        {
            printf("%d ", array[i]);
        }
        printf(")\n");
        current = 0;
    }
}

char *opAnd(char *op1, char *op2)
{
    if ((strcmp(op1, "true") == 0) && (strcmp(op2, "true") == 0))
        return "true";
    return "false";
}

char *opOr(char *op1, char *op2)
{
    if ((strcmp(op1, "true") == 0) || (strcmp(op2, "true") == 0))
        return "true";
    return "false";
}

char *opNot(char *op1)
{
    if ((strcmp(op1, "true") == 0))
        return "false";
    return "true";
}

char *opEqualStr(char *op1, char *op2)
{
    if ((strcmp(op1, op2) == 0))
        return "true";
    return "false";
}

char *opLess(int a, int b)
{
    if (a <= b)
        return "true";
    return "false";
}

char *returnString(char *str)
{
    return str;
}
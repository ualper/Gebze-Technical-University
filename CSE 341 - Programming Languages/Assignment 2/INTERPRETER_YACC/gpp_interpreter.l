%
{
#include <stdio.h>
#include "y.tab.h"
    int FLAG_QUOTATION = 0; //FLAG_QUOTATION == 1 OPENED QUOTATION
                            //FLAG_QUOTATION == 0 CLOSED QUOTATION
    int CONTROL_LOOP = 0;   //if CONTROL_LOOP == 0 CONTINUE LOOP
                            //if CONTROL_LOOP == 1 BREAK LOOP
    FILE *FP_Out;
    %
}

% %
    "+"
{
    CONTROL_LOOP = 0;
    return OP_PLUS;
}
"-"
{
    CONTROL_LOOP = 0;
    return OP_MINUS;
}
"/"
{
    CONTROL_LOOP = 0;
    return OP_DIV;
}
"*"
{
    CONTROL_LOOP = 0;
    return OP_MULT;
}
"("
{
    CONTROL_LOOP = 0;
    return OP_OP;
}
")"
{
    CONTROL_LOOP = 0;
    return OP_CP;
}
"**"
{
    CONTROL_LOOP = 0;
    return OP_DBLMULT;
}

"and"
{
    CONTROL_LOOP = 0;
    return KW_AND;
}
"or"
{
    CONTROL_LOOP = 0;
    return KW_OR;
}
"not"
{
    CONTROL_LOOP = 0;
    return KW_NOT;
}
"equal"
{
    CONTROL_LOOP = 0;
    return KW_EQUAL;
}
"less"
{
    CONTROL_LOOP = 0;
    return KW_LESS;
}
"nil"
{
    CONTROL_LOOP = 0;
    return KW_NIL;
}

"list"
{
    CONTROL_LOOP = 0;
    return KW_LIST;
}
"append"
{
    CONTROL_LOOP = 0;
    return KW_APPEND;
}
"concat"
{
    CONTROL_LOOP = 0;
    return KW_CONCAT;
}
"set"
{
    CONTROL_LOOP = 0;
    return KW_SET;
}
"deffun"
{
    CONTROL_LOOP = 0;
    return KW_DEFFUN;
}
"defvar"
{
    CONTROL_LOOP = 0;
    return KW_DEFVAR;
}

"for"
{
    CONTROL_LOOP = 0;
    return KW_FOR;
}
"if"
{
    CONTROL_LOOP = 0;
    return KW_IF;
}
"exit"
{
    CONTROL_LOOP = 0;
    return KW_EXIT;
}
"load"
{
    CONTROL_LOOP = 0;
    return KW_LOAD;
}
"true"
{
    CONTROL_LOOP = 0;
    return KW_TRUE;
}
"false"
{
    CONTROL_LOOP = 0;
    return KW_FALSE;
}
"while"
{
    CONTROL_LOOP = 0;
    return KW_WHILE;
}

"\""
{
    CONTROL_LOOP = 0;
    if (FLAG_QUOTATION == 0)
    {
        FLAG_QUOTATION = 1;
        return OP_OC;
    }
    else
    {
        FLAG_QUOTATION = 0;
        return OP_CC;
    }
}
","
{
    CONTROL_LOOP = 0;
    return OP_COMMA;
}
"'("
{
    CONTROL_LOOP = 0;
    return OP_LIST;
}

";;".*
{
    CONTROL_LOOP = 0;
    return COMMENT;
}
[0] | [1 - 9][0 - 9] *
{
    CONTROL_LOOP = 0;
    yylval = atoi(yytext);
    return VALUE;
} //Refular Expression
[a - zA -
    Z_][a - z.A - Z0 - 9_] *
{
    CONTROL_LOOP = 0;
    return IDENTIFIER;
} //Refular Expression

[\t] | [] {} //IGNORE THE WHITE SPACES
           [\n]
{
    if (CONTROL_LOOP == 0)
        CONTROL_LOOP = 1;
    else
        return NEWLINE;
}
.
{
    printf("SYNTAX ERROR %s IS NOT RECOGNIZED\n");
    return 1;
}
% %

    int yywrap()
{
    return 1;
}
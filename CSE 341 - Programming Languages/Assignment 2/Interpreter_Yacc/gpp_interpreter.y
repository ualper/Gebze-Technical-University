%{
    #include "gpp_lib.h"
%}

%union
{
   int int_val;
   char *string_val;
   int* intArr;
}

%token <int_val> VALUE
%token IDENTIFIER COMMENT KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE KW_WHILE 
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_DBLMULT OP_OC OP_COMMA EOL
%type <int_val> EXPI
%type <intArr> EXPLISTI
%type <string_val> EXPB
%type <string_val> IDENTIFIER
%start START

%%

START:{ 
        printf("########## WELCOME THE G++ INTERPRETER ##########\n\n");
        printf("---> "); 
    }
    | START INPUT EOL {
       printf("---> "); 
    }
    | START COMMENT EOL {
       printf("COMMENT\n");       
       printf("---> "); 
    }
    ;

INPUT: EXPI {
        printf("%d\n",$1);
    } 
    | EXPLISTI {
        printList(); 
    } 
    | EXPB {
        printf("%s\n",$1);
    }
    | OP_OP KW_DEFFUN INPUT OP_CP { printf("SYNTAX OK."); }
    ;

EXPI: VALUE { $$ = $1; }

    | OP_OP OP_PLUS EXPI EXPI OP_CP { $$ = itisPLUS($3, $4); }

    | OP_OP OP_MINUS EXPI EXPI OP_CP { $$ = itisMINUS($3, $4); }

    | OP_OP OP_MULT EXPI EXPI OP_CP { $$ = itisMULTI($3, $4); }

    | OP_OP OP_DIV EXPI EXPI OP_CP { $$ = itisDIVIDE($3, $4); }

    | OP_OP OP_DBLMULT EXPI EXPI OP_CP { $$ = itisDBLMULT($3, $4); }

    | OP_OP KW_SET IDENTIFIER EXPI OP_CP { $$ = $4; }

    | OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP INPUT OP_CP { printf("SYNTAX OK."); }

    | EXPLISTI

    | KW_EXIT { 
        printf("BYE.\n");
        exit(0); 
    }        
    ;

IDENTIFIERS: IDENTIFIERS IDENTIFIER | IDENTIFIER;

EXPLISTI: 
    OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP { listConcat($3, $4); }

    | OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP 

    | OP_OP KW_APPEND EXPI EXPLISTI OP_CP { listAppend($3, $4); }

    | OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP { $$ = $4; }

    | LISTVALUES 
    ;


LISTVALUES: OP_OC OP_OP OP_CP 

    | OP_OP KW_LIST OP_CP

    | OP_OC OP_OP VALUES OP_CP 

    | OP_OP KW_LIST VALUES OP_CP
    ;

VALUES: VALUES VALUE { 
        fillTheArray($2); 
    }
    | VALUE { 
        fillTheArray($1); 
    }
    ;
    
EXPB: 
    OP_OP KW_AND EXPB EXPB OP_CP { $$ = itisAND($3, $4); }

    | OP_OP KW_OR EXPB EXPB OP_CP { $$ = itisOR($3, $4); }

    | OP_OP KW_NOT EXPB OP_CP { $$ = itisNOT($3); }

    | OP_OP KW_EQUAL EXPB EXPB OP_CP { $$ = itisEQUAL_STR($3, $4); }

    | OP_OP KW_EQUAL EXPI EXPI OP_CP { $$ = itisEQUAL_INT($3, $4); }

    | OP_OP KW_LESS EXPI EXPI OP_CP { $$ = itisLESS($3, $4); }

    | OP_OP KW_IF EXPB EXPLISTI OP_CP { 
        if($3 == "false")
            $$ = "false";
        else {
            printList();
            $$ = " ";
        }    
    }

    | OP_OP KW_WHILE EXPB EXPLISTI OP_CP { 
        if($3 == "false")
            $$ = "false";
        else {
            printList();
            $$ = " ";
        }    
    }

    | OP_OP KW_FOR EXPB EXPLISTI OP_CP { 
        if($3 == "false")
            $$ = "false";
        else {
            printList();
            $$ = " ";
        }    
    }

    | OP_OP KW_LOAD IDENTIFIER OP_CP { $$ = "load";}

    | KW_TRUE { $$ = "true"; }

    | KW_FALSE { $$ = "false"; }
    ;
%%
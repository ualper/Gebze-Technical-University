%{ 
#include<stdio.h> 
#include<math.h>

extern FILE *yyin;
extern int temp;

FILE *FP_Out;

int flagBool=0;
int flagError=0;
int flagExit=0;
int flagNill=0;
int flagPrint=1;
int isList=0;
int Arr[999];
int index1=0;
int index2=0;
int withoutResult=0;

%} 

%token KW_AND
%token KW_OR
%token KW_NOT   
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_LIST
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_DEFVAR
%token KW_FOR
%token KW_WHILE
%token KW_IF
%token KW_EXIT 
%token KW_LOAD
%token KW_TRUE
%token KW_FALSE
%token OP_PLUS
%token OP_MINUS
%token OP_DIV
%token OP_MULT
%token OP_OP
%token OP_CP
%token OP_DBLMULT
%token OP_OC
%token OP_CC
%token OP_COMMA
%token OP_LIST
%token COMMENT
%token VALUE
%token IDENTIFIER
%token NEWLINE
%start START

%% 

START: | INPUT{ 
	if(flagError == 0 && flagExit ==0){
		printf("Syntax OK.\n");
		fprintf(FP_Out, "Syntax OK.\n");

		if(!withoutResult){
			int res = $$;

			if(isList == 1){

				printf("Result: ");
				fprintf(FP_Out, "Result: ");
			    
			    printf("(");
			    fprintf(FP_Out, "(");

			    for(int i=0;i<index1;++i){
			        if(i == (index1-1)){
			            printf("%d", Arr[i]);
			            fprintf(FP_Out, "%d", Arr[i]);
			        }
			        else{
			            printf("%d ",Arr[i]);
			            fprintf(FP_Out, "%d ", Arr[i]);
			        }
			    }
			    printf(") \n\n");
			    fprintf(FP_Out, ") \n\n");

			    index1=0;
			    
			    index2=0;
			    isList=0;			
			}
			else if(flagBool == 1){
				if(flagNill){
					printf("Result: NIL \n\n");
					fprintf(FP_Out, "Result: NIL \n\n");
					flagNill=0;
				}else{
				    if(res == 1){
					    printf("Result: T \n\n");
					    fprintf(FP_Out, "Result: T \n\n");
				    }
				    else{
				        printf("Result: NIL \n\n");
				        fprintf(FP_Out, "Result: NIL \n\n");
				    }
				    //flagBool=0;		
				}
				flagBool=0;
			}
			else{
				if(flagPrint){
					printf("Result: %d\n\n", res);
					fprintf(FP_Out, "Result: %d\n\n", res);	
				}
				else{
					flagPrint=1;	
					printf("\n");
					fprintf(FP_Out, "\n");
				}
			}
		}
		else{
			withoutResult=0;
			printf("\n");
			fprintf(FP_Out, "\n");
		}
	}
	return 0;};

INPUT: EXPI | EXPLISTI | EXPB{ flagBool=1; } | EXIT;

EXPI: OP_OP OP_PLUS EXPI EXPI OP_CP { $$=$3+$4; }
	| OP_OP OP_MINUS EXPI EXPI OP_CP { $$=$3-$4; }		
	| OP_OP OP_MULT EXPI EXPI OP_CP { $$=$3*$4; } 
	| OP_OP OP_DIV EXPI EXPI OP_CP { $$=$3/$4; } 	
	| OP_OP OP_DBLMULT EXPI EXPI OP_CP { $$=pow($3,$4); } 
	| IDENTIFIER { $$=1; flagPrint=0;}
	| VALUE { $$=$1; }	
	| OP_OP IDENTIFIER EXPLISTI OP_CP { 
	    isList=1;
	    $$=$3; 
	}	
	| OP_OP KW_SET IDENTIFIER EXPI OP_CP { $$=$4; }	
	| OP_OP KW_DEFFUN IDENTIFIER IDLIST EXPLISTI OP_CP{
	    isList=1;
	    $$=$5;
	}	
	| OP_OP KW_IF EXPB EXPLISTI OP_CP { 
	    isList=1;
	    $$=$3; 
	    if($$ == 0){ 
	        index1=0;
	        Arr[0]=NULL;
	    }
	}	
	| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{ 
	    isList=1;
	    $$=$3;
	    if($$ == 1){ 
	        index1=index2;
	    }
	    else{
	        index1 -= index2;
	        for(int i=0;i<index1;++i){
	            Arr[i]=Arr[index2+i];
	        }
	    }
	}	
	| OP_OP KW_WHILE EXPB EXPLISTI OP_CP{ 
	    isList=1;
	    $$=$3; 
	    if($$ == 0){
	        index1=0;
	        Arr[0]=NULL;
	    }
	}
	| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP{
	    isList=1;
	}
	| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP { $$=$4; } 	
	| OP_OP KW_LIST VALUES OP_CP{
	    isList=1;
	    $$=1; 
	}
	| OP_OP KW_LOAD OP_OC IDENTIFIER OP_CC OP_CP { $$=1; flagPrint=0; } /*?*/
	| COMMENT { 
	    printf("COMMENT\n"); 
	    fprintf(FP_Out, "COMMENT\n");
	    withoutResult=1;
	}
	| OP_OP KW_EXIT OP_CP {
		withoutResult=1;
	}
;

EXPLISTI: OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP{
		    isList=1;
		    $$=1; 
		}
	| OP_OP KW_APPEND EXPI EXPLISTI OP_CP{
	    $$=1;

	    for(int i=index1-1; i>-1; --i)
	    	Arr[i+1] = Arr[i];
	    Arr[0] = $3;
	
	    isList=1;
	    ++index1;
	}
	| LISTVALUE{$$=1;}
;

EXPB: OP_OP KW_AND EXPB EXPB OP_CP { $$=$3&&$4; } 
	| OP_OP KW_OR EXPB EXPB OP_CP { $$=$3||$4; } 
	| OP_OP KW_NOT EXPB OP_CP { $$=!$3; } 
	| OP_OP KW_EQUAL EXPB EXPB OP_CP { $$=($3==$4); } 
	| OP_OP KW_EQUAL EXPI EXPI OP_CP { $$=($3==$4); } 
	| BinaryValue{$$=$1;};
	| OP_OP KW_LESS EXPI EXPI OP_CP { $$=($3<$4); } /*????????*/
;

LISTVALUE: OP_LIST VALUES OP_CP{
	    isList=1;
	    if(index2==0)
	        index2=index1;
	}
	| OP_LIST OP_CP {
	    isList=1;
	    $$ = index1 = 0;
	}
	| KW_NIL{$$=0; flagBool=1; flagNill=1; };
;

VALUES: VALUES VALUE  {
	    Arr[index1++]=$2;
	}
	| VALUE {
	    Arr[index1++]=$1;
	}
;

BinaryValue: KW_TRUE { $$=1; }
	| KW_FALSE { $$=0; }
;

IDLIST: OP_OP IDENT_LIST OP_CP;

IDENT_LIST: IDENT_LIST IDENTIFIER | IDENTIFIER;

EXIT: NEWLINE { withoutResult=1; flagExit=1; return 0; };

%%

int main(int argc, char *argv[]){ 
    FP_Out = fopen ("parsed_cpp.txt", "w");

    if(argc == 1){
        yyin = stdin;
		while(flagExit == 0)	
			yyparse();        
    }
    else if(argc == 2){
        yyin = fopen(argv[1], "r"); 

        if(yyin == NULL){
            printf("File not opened.\n");
            return -1;
        }

		while(flagExit == 0){
			yyparse();        
		}
    }
    else
        printf("You entered wrong thing.\n");

    return 0;
}

int yyerror(const char * ch) 
{ 
    flagError=1;
    flagExit=1; /* Exit at Error*/
	printf("\nSYNTAX_ERROR Expression is not recognized\n"); 
	printf(FP_Out, "\nSYNTAX_ERROR Expression is not recognized\n"); 
}

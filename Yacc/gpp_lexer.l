
%{
#include "stdio.h"
#include "y.tab.h"
int count =0;
%}


/*synax tokens for G++ lexer*/
%%
"and"      return KW_AND    ;           /*and */
"or"       return KW_OR     ;           /*or */
"not"      return KW_NOT    ;           /*nto */
"equal"    return KW_EQUAL  ;            /*equal */
"less"     return KW_LESS   ;           /*less */
"nil"      return KW_NIL    ;           /*nil */
"list"     return KW_LIST   ;           /*list */
"append"   return KW_APPEND ;           /*append */
"concat"   return KW_CONCAT ;           /*concat */
"set"      return KW_SET    ;           /*set */
"deffun"   return KW_DEFFUN ;           /*deffun */
"defvar"   return KW_DEFVAR ;           /*defvar */
"for"      return KW_FOR    ;           /*for */
"if"       return KW_IF     ;           /*if */
"exit"     return KW_EXIT   ;           /*exit */
"load"     return KW_LOAD   ;           /*load */
"disp"     return KW_DISP   ;           /*disp */
"true"     return KW_TRUE   ;           /*true */
"false"    return KW_FALSE  ;           /*false */
"+"        return OP_PLUS   ;           /*plus */
"-"        return OP_MINUS  ;           /*minus */
"/"        return OP_DIV    ;           /*division */
"*"        return OP_MULT   ;           /*multiplier */
"**"       return OP_DBLMULT;          /*double multiplier */
","        return OP_COMMA  ;           /*comma */
"\""        {    count += 1;
            if(count % 2 == 1) return OP_OC;    /*open quote */
            else return OP_CC;  /*close quote  */
        }   
"("    return OP_OP ;            /*open paranthesis */
")"    return OP_CP  ;           /*close paranthesis */


";;"+([. + - / *]|[A-Za-z]|[0-9]|[ \t])*         return  COMMENT;        /* expression for comment token*/ 





 /* ";"+[a-zA-Z0-9]+                  yyerror("47ERROR INVALID-TOKEN");

 [a-zA-Z0-9]+";"                 yyerror("49ERROR INVALID-TOKEN");

 [a-zA-Z0-9]+";"+[a-zA-Z0-9]+          yyerror("51ERROR INVALID-TOKEN");





  [0-9][0-9]*                                yyerror("72ERROR-INVALID VALUE (zero leading)");
                                regular expression for 0 digit  
 [0-9]+[a-zA-Z]+[a-zA-Z0-9]+                           yyerror("74ERROR INVALID-IDENTIFIER");   */
 
 
 

[a-zA-Z_][a-zA-Z0-9_]*			{
									yylval.string=strdup(yytext);
									return IDENTIFIER;
								}  
 
 

[0]|[1-9][0-9]*                      {	
									yylval.number=atoi(yytext);
									return VALUE;
								}                        /*regular expression for invalid integer value */  

[a-zA-Z0-9\/.+-]+				{
									yylval.string=strdup(yytext);
									return FILENAME;
								}  


  


^\n             yyterminate();  /* terminating for twice entering blank space*/


[ \t]+  ;	
[ \n]  ;

.   yyerror("ERROR unexpected input\n");

%%

/*
int main(int argc, char *argv[])
{
    extern FILE *yyin, *yyout; 
  
   
    
    
    if(argc > 1 ){
        
        yyin = fopen(argv[1], "r"); 
    
    }
    else{
            
            printf("There is no file name on commandline \nEnter your input here please\n ENTERED   for REPL mode\nEnter twice for exit\n"); 
            
    }
        
   
    yyout = fopen("parsed_cpp.txt", "w"); 
  
    yylex();

   return 0;

}
int yywrap(){return(1);}

int yyerror(char *msg) { 
  printf("SYNTAX_ERROR : Expression cannot recognized , check it again\n");
  exit(0); 
}
*/


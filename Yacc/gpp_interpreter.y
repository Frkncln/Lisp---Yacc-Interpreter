//>1801042628 Furkan Celen
//>Start compile with 'make' command

//>Write if you want to run program in REPL mode './a.out'

//> If you want to run with FILENAME write  './a.out filename.txt'

%{
	#include <string.h>
	#include <stdio.h>
	

	void stringup(char str[]){
   		int ch = 0;
   
   		while (str[ch] != '\0') {
      		if (str[ch] >= 'a' && str[ch] <= 'z') {
         		str[ch] = str[ch] - 32;
      		}
      		ch++;
   		}
	}


	void copy(int k[], int l[], int size){
		int i;

		for(i = 0; i < size && i < 999; i++)
			k[i] = l[i];
	}	


	int strcheck(char *str, char strlist[1000][200], int strnum){
		int i;
	    
	    for( i = 0; i < strnum; i++ )
	        if (strcmp(str, strlist[i]) == 0 )
	            return i;

	    return -1;
	}

	
	struct f
	{ 
	   char type[1000][200]; 
	   int values[1000]; 
	   int count;
	};

	struct f types = {"", 0, 0};
	
	int i = 2;
	int z = 0;
	int y = 0;
	int sum = 0;

%}


%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_APPEND
%token OP_MULT OP_OP OP_CP OP_OC OP_CC OP_COMMA COMMENT IDENTIFIER VALUE
%token KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV OP_DBLMULT
%token KW_CONCAT KW_SET KW_DEFFUN KW_DEFVAR KW_FOR KW_IF KW_EXIT KW_LOAD
%token CUT FILENAME

%start START

%union{
	struct{
		int flag;
        int number;
        char *string;
        int values[1000];
  };
}


%%

START: | START INPUT {printf("\n%d-> ",i);i=i+1;y=0;z=0;sum=0;};

INPUT:
		EXPI{		
				if($<flag>$ == 0){
					printf("Syntax:Ok\nResult: %d\n", $<number>1);
					 $<number>$ = $<number>1; $<flag>$ = 0;
				}
				else if($<flag>$ == 1){
					printf("Syntax:Ok\nResult: %s\n", $<string>1);
					 $<string>$ = $<string>1; $<flag>$ = 1;
				}
				else if($<flag>$ == 2){
					if($<number>$ == 1) printf("Syntax:Ok\nResult: True\n");
					   else printf("Syntax:Ok\nResult: False\n");
					
					$<number>$ = $<number>1;
					$<flag>$ = 2;
					
				}
				else if($<flag>$ == 3){
					int j;
					printf("Syntax:Ok\nResult: ");
					printf("(");
					
					for(j = 0; j < z; j += 1){
						printf("%d", $<values>1[j]);
    					if(j != z-1) 
    						printf(" ");
    				}
    				printf(")\n");
    				
    				copy($<values>$, $<values>1, z);
					
					$<flag>$ = 3;
				}
			}
		| EXPLISTI {		
						printf("Syntax:Ok\nResult: ");	
						if($<flag>$ == 0){
							printf("%d\n", $<number>1); $<number>$ = $<number>1; $<flag>$ = 0;
						}
						else if($<flag>$ == 1){
							printf("%s\n", $<string>1); $<string>$ = $<string>1; $<flag>$ = 1;
						}
						else if($<flag>$ == 2){
							if($<number>$ == 1) 
								printf("T\n");
							else 
								printf("NIL\n");
							$<number>$ = $<number>1;
							$<flag>$ = 2;
						}
						else if($<flag>$ == 3)
						{
							int j;
							printf("(");
							
							for(j = 0; j < z; j += 1){
								printf("%d", $<values>1[j]);
            					if(j != z-1) printf(" ");
            				}
            				
            				printf(")\n");
            				
            				copy($<values>$, $<values>1, z);
							$<flag>$ = 3;
						}
					}//explisti end
		| COMMENT {$<flag>$ = 5;};

LISTVALUE:
		OP_OP KW_LIST VALUES OP_CP{			
										copy($<values>$, $<values>3, y);
										$<flag>$ = 3;	
									}
		| CUT OP_OP VALUES OP_CP {			
								copy($<values>$, $<values>3, y);
									$<flag>$ = 3;
								 	};

VALUES:
		VALUES VALUE{	
						$<values>$[y] = $<number>2;
						y =y+1;
						$<flag>$ = 3;	
						
					 }
		| VALUES IDENTIFIER{
								stringup($<string>2);
								int flg = strcheck($<string>2, types.type, types.count);
								if(flg != -1){
									$<number>2 = types.values[flg];
									$<flag>$ = 0;
								}
								else{
									printf(" That variable %s has no value ,exiting now,try again later\n", $<string>2);
									exit(0);
								}
								$<values>$[y] = $<number>2;
								y += 1;
								
								$<flag>$ = 3;
						 }
		| VALUE {$<values>$[y]=$<number>1;y=y+1;$<flag>$=3;};

IDENTIFIERS:
		IDENTIFIERS IDENTIFIER{ 
								stringup($<string>1);
								$<string>$ = $<string>1;
								strcat($<string>$, " ");
								strcat($<string>$, $<string>2);
								$<flag>$ = 1;
							  }
		| IDENTIFIER {			
						stringup($<string>1);
						int flg = strcheck($<string>1, types.type, types.count);
						if(flg == -1){
							strcpy(types.type[types.count], $<string>1);
							types.values[types.count] = -1;
							types.count = types.count +1;
						}
						$<string>$ = $<string>1; $<flag>$ = 1;
					 };

EXPI:
		OP_OP OP_PLUS EXPI EXPI OP_CP {$<number>$ = $<number>3 + $<number>4; $<flag>$ = 0;}
		| OP_OP OP_MINUS EXPI EXPI OP_CP {$<number>$ = $<number>3 - $<number>4; $<flag>$ = 0;}
		| OP_OP OP_MULT EXPI EXPI OP_CP {$<number>$ = $<number>3 * $<number>4; $<flag>$ = 0;}
		| OP_OP OP_DIV EXPI EXPI OP_CP {
										if($<number>4 == 0){
											printf("Math Error : Can not divide by zero\n");
											exit(0);
										}
										else{
											$<number>$ = $<number>3 / $<number>4;
											$<flag>$ = 0;
										}
									}
		| OP_OP OP_DBLMULT EXPI EXPI OP_CP {
											int x = $<number>3;
											int j;
											for(j = 1;j<$<number>4;j++)
												x *=$<number>3;
											$<number>$ = x;
											$<flag>$ = 0;
										   }
		| OP_OP IDENTIFIER EXPLISTI OP_CP {stringup($<string>2); $<flag>$ = 1;$<string>$ = $<string>2; }
		| OP_OP IDENTIFIER EXPI OP_CP {stringup($<string>2);$<flag>$ = 1; $<string>$ = $<string>2; }
		| OP_OP KW_SET IDENTIFIER EXPI OP_CP{
											stringup($<string>3);
											int flg = strcheck($<string>3, types.type, types.count);
											if(flg == -1){
												strcpy(types.type[types.count], $<string>3);
												types.values[types.count]=$<number>4;
												types.count=types.count+1;
											}
											else{
												types.values[flg] = $<number>4;
											}

											$<number>$ = $<number>4;
											$<flag>$ = 0;
										}
		| OP_OP KW_SET IDENTIFIER EXPLISTI OP_CP{
													copy($<values>$, $<values>4, z);
													$<flag>$ = 3;
												}
		| OP_OP KW_IF EXPB EXPLISTI OP_CP{
											if($<number>3 == 1){
												copy($<values>$, $<values>4, z);
												$<flag>$ = 3;
											}
											else{
												$<number>$ = 0;
												$<flag>$ = 3;
											}
										 }
		| OP_OP KW_IF EXPB EXPLISTI EXPLISTI OP_CP{
													if($<number>3 == 1){
														copy($<values>$, $<values>4, z);
														$<flag>$ = 3;
													}
													else{
														copy($<values>$, $<values>5, z);
														$<flag>$ = 3;
													}
												 }
		| OP_OP KW_IF EXPB EXPI OP_CP{
										if($<number>3 == 1){
										   $<number>$ = $<number>4;
										    $<flag>$ = 0;
										}
										else{
										   $<number>$ = 0;
										   $<flag>$ = 3;
										}
									 }
		| OP_OP KW_IF EXPB EXPI EXPI OP_CP{
											if($<number>3 == 1){
												$<number>$ = $<number>4;;
												$<flag>$ = 0;
											}
											else{
												$<number>$ = $<number>5;
												$<flag>$ = 0;
											}
										 }
		| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP {
																copy($<values>$, $<values>8, z);
																$<flag>$ = 3;
															 }
		| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP{
												stringup($<string>3);
												strcpy(types.type[types.count], $<string>3);
												types.values[types.count] =$<number>4;
												types.count =types.count+1;
												$<string>$ =$<string>3;
												$<flag>$ =1;
										  		}
		| OP_OP KW_DEFVAR IDENTIFIER EXPLISTI OP_CP{
												stringup($<string>3);
												$<string>$ =$<string>3;
												$<flag>$ =1;
										  		}
		| OP_OP KW_LOAD OP_OC FILENAME OP_CC OP_CP {
														FILE* fp =fopen($<string>4, "r");
														$<flag>$ =2;
														$<number>$ =0;
														
														if(fp == NULL) 
															printf("File named %s does not exist,check and try again later!\n", $<string>4);
											    		else{
											    			printf(";; Loaded file that named %s\n", $<string>4);
											    			$<number>$ = 1;
											    		}
												   }
		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPI OP_CP {
																		stringup($<string>3);
																		strcpy(types.type[types.count], $<string>3);
																		types.values[types.count] = -1;
																		types.count =types.count + 1;
																		$<string>$ =$<string>3;
																		$<flag>$ =1;
																	}
		| OP_OP KW_DEFFUN IDENTIFIER OP_OP IDENTIFIERS OP_CP EXPLISTI OP_CP {
																			stringup($<string>3);
																			strcpy(types.type[types.count], $<string>3);
																			types.values[types.count] = -1;
																			types.count =types.count +1;
																			$<string>$ =$<string>3;
																			$<flag>$ = 1 ;
																		}
		| OP_OP KW_DEFFUN IDENTIFIER EXPLISTI OP_CP {
														stringup($<string>3);
														strcpy(types.type[types.count], $<string>3);
														types.values[types.count] = -1;
														types.count = types.count +1;
														$<string>$ = $<string>3;
														$<flag>$ = 1;
													}
		| OP_OP KW_EXIT OP_CP {
			printf("Exiting...\n"); exit(1);
		}
		| OP_OP KW_DISP EXPI OP_CP {
										if($<flag>3 == 0){
											$<number>$ = $<number>3; 
											$<flag>$ = 0;
										}
										else if($<flag>3 == 1){
											$<string>$ = $<string>3; 
											$<flag>$ = 1;
										}
										else if($<flag>3 == 2){
											$<number>$ = $<number>3; 
											$<flag>$ = 2;
										}
										else if($<flag>3 == 3){
											copy($<values>$,$<values>3,z); 
											$<flag>$ = 3;
										}
									}
		| OP_OP KW_DISP EXPLISTI OP_CP {
										copy($<values>$, $<values>3, z); 
										$<flag>$ = 3;
									   }
		| EXPB {$<number>$ = $<number>1;
				 $<flag>$ = $<flag>1;
				};

EXPB:
		OP_OP KW_AND EXPB EXPB OP_CP {$<number>$ = $<number>3 && $<number>4; $<flag>$ = 2;}
		| OP_OP KW_OR EXPB EXPB OP_CP {$<number>$ = $<number>3 || $<number>4;
		  								$<flag>$ = 2;
		  								}
		| OP_OP KW_NOT EXPB OP_CP {if($<number>3 == 0) $<number>$ = 1;
									 else $<number>$ = 0;
								 		$<flag>$ = 2;
								 	}
		| OP_OP KW_EQUAL EXPB EXPB OP_CP {
		
									if($<number>3 == $<number>4){ $<number>$ = 1;}
										else {$<number>$ = 0;}
									
									$<flag>$ = 2;}
		| OP_OP KW_LESS EXPB EXPB OP_CP {if($<number>3 < $<number>4) $<number>$ = 1;
											 else $<number>$ = 0; 
											 $<flag>$ = 2;
											}
		| VALUE {$<number>$ = $<number>1;
				 $<flag>$ = 0;
				}	
		| KW_TRUE {$<number>$ = 1;
					 $<flag>$ = 2;
				  }
		| KW_FALSE {$<number>$ = 0;
					 $<flag>$ = 2;
					}
		| KW_NIL {$<number>$ = 0;
				 $<flag>$ = 2;
				}
		| IDENTIFIER {
						stringup($<string>1);
						int flg = strcheck($<string>1, types.type, types.count);
						if(flg != -1){
							$<number>$ = types.values[flg];
							$<flag>$ = 0;
						}
						else{
							printf("That variable named %s has no value\n", $<string>1);
							exit(0);
						}
					 };

EXPLISTI:
		OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {
													int j, x = 0;
													for(j = sum - z; j < sum; j++, x++){
														$<values>3[j] = $<values>4[x];
													}
													copy($<values>$, $<values>3, sum);
													z = sum;
													$<flag>$ = 3;
												 }
		| OP_OP KW_APPEND EXPLISTI EXPLISTI OP_CP {
													int j, x = 0;
													for(j = sum - z; j < sum; j++, x++){
														$<values>3[j] = $<values>4[x];
													}
													
													copy($<values>$, $<values>3, sum);
													z = sum;
													$<flag>$ = 3;
													
												  }
		| OP_OP KW_APPEND EXPI EXPLISTI OP_CP {
													int j;
													for(j = 0; j < z; j++){
														$<values>$[j+1] = $<values>4[j];
													}
													$<values>$[0] = $<number>3;
													z =z+1;
													$<flag>$ = 3;
													
											  }
		| LISTVALUE {
						copy($<values>$, $<values>1, y);
						$<flag>$ = 3;
						z = y;
						sum =sum + y;
						y = 0;
					};

%%


int main(int argc, char *argv[])
{
	extern FILE *yyin, *yyout; 
  
   
    
    
    if(argc > 1 ){
    	
    	yyin = fopen(argv[1], "r"); 
    
    }
    else{
    		
    	printf("There is no file name on commandline \nEnter your input here please\n ENTERED   for REPL mode\nEnter twice for exit\n"); 
    		
    }

 	printf("1-> ");
  	yyparse(); 
 } 

int yyerror(char *msg) { 
  printf("%s SYNTAX_ERROR : Expression cannot recognized , check it again\n",msg);
  exit(0); 
}

int yywrap(){
	return 1;
} 

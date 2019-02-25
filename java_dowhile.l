%option yylineno

%{
	#include <stdio.h>
	#include "y.tab.h"
	#include <math.h>
%}

%%

"bool" {return BOOLEAN;}
"break" {return BREAK;}
"byte" {return BYTE;}
"char" {return CHAR;}
"class" {return CLASS;}
"continue" {return CONTINUE;}
"do" {return DO;}
"double" {return DOUBLE;}
"else" {return ELSE;}
"final" {return FINAL;}
"float" {return FLOAT;}
"for" {return FOR;}
"if" {return IF;}
"import" {return IMPORT;}
"int" {return INT;}
"long" {return LONG;}
"new" {return NEW;}
"package" {return PACKAGE;}
"private" {return PRIVATE;}
"protected" {return PROTECTED;}
"public" {return PUBLIC;}
"return" {return RETURN;}
"short" {return SHORT;}
"static" {return STATIC;}
"this" {return THIS;}
"void" {return VOID;}
"while" {return WHILE;}
"true"|"false" {return BOOLLIT;}
[A-Za-z$_][A-Za-z$_0-9]* {return IDENTIFIER;}
[0-9]+ {return NUM_LITERAL;}
\"(\\.|[^\\"])*\" {return LITERAL;}
 \/\*\*(\\.|[^\\"])*\*\/ {return COMMENT;}
"++" {return OP_INC;}
"--" {return OP_DEC;}
"+=" {return ASS_ADD;}
"-=" {return ASS_SUB;}
"*=" {return ASS_MUL;}
"/=" {return ASS_DIV;}
"%=" {return ASS_MOD;}
"<=" {return OP_LE;}
">=" {return OP_GE;}
"==" {return OP_EQ;}
"!=" {return OP_NE;}
"&&" {return OP_LAND;}
"||" {return OP_LOR;}
[;.,+-/%<>!&(){}=]|"|"|"*" {return yytext[0];}
[ \t\n] ;  
%%

int yywrap(void)
{
	return 1;
}

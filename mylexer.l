%{
    #include <assert.h>
    #include <stdio.h>
    #include <string.h>
    #include <math.h>
    #include "myanalyzer.tab.h"
    #include "cgen.h"

    int lineNum = 1;
%}

ID      [a-zA-Z][a-zA-Z0-9_]*
DIGIT   [0-9]

NUMBER  {DIGIT}{DIGIT}*
DECIMAL ("."{NUMBER})|({NUMBER}("."{NUMBER}?)?)
REAL    {DECIMAL}([eE][+-]?{NUMBER})?

SPEC    ("\""|"\\"|"\r"|"\t"|"\n")
PUNCT   ("-"|"+"|"*"|"/"|":"|"_"|"$"|"!"|"#"|"@"|"~"|"^"|"("|")"|",")
STRING  ("\"")([a-zA-Z0-9]|" "|{SPEC}|{PUNCT})*("\"")

%x comment

%%

int                     { printf("Token KEYWORD_INT:\t\t%s\n", yytext); return KW_INT;}
real                    { printf("Token KEYWORD_REAL:\t\t%s\n", yytext); return KW_REAL;}
string                  { printf("Token KEYWORD_STRING:\t\t%s\n", yytext); return KW_STRING;}
bool                    { printf("Token KEYWORD_BOOL:\t\t%s\n", yytext); return KW_BOOL;}
var                     { printf("Token KEYWORD_VAR:\t\t%s\n", yytext); return KW_VAR;}
const                   { printf("Token KEYWORD_CONST:\t\t%s\n", yytext); return KW_CONST;}
if                      { printf("Token KEYWORD_IF:\t\t%s\n", yytext); return KW_IF;}
else                    { printf("Token KEYWORD_ELSE:\t\t%s\n", yytext); return KW_ELSE;}
for                     { printf("Token KEYWORD_FOR:\t\t%s\n", yytext); return KW_FOR;}
while                   { printf("Token KEYWORD_WHILE:\t\t%s\n", yytext); return KW_WHILE;}
break                   { printf("Token KEYWORD_BREAK:\t\t%s\n", yytext); return KW_BREAK;}
continue                { printf("Token KEYWORD_CONTINUE:\t\t%s\n", yytext); return KW_CONTINUE;}
func                    { printf("Token KEYWORD_FUNC:\t\t%s\n", yytext); return KW_FUNC;}
nil                     { printf("Token KEYWORD_NIL:\t\t%s\n", yytext); return KW_NIL;}
and                     { printf("Token AND_OP:\t\t\t%s\n", yytext); return KW_AND;}
or                      { printf("Token OR_OP:\t\t\t%s\n", yytext); return KW_OR;}
not                     { printf("Token NOT_OP:\t\t\t%s\n", yytext); return KW_NOT;}
return                  { printf("Token KEYWORD_RETURN:\t\t%s\n", yytext); return KW_RETURN;}
begin                   { printf("Token KEYWORD_BEGIN:\t\t%s\n", yytext); return KW_BEGIN;}
true                    { printf("Token CONST_TRUE:\t\t%s\n", yytext); return KW_TRUE;}
false                   { printf("Token CONST_FALSE:\t\t%s\n", yytext); return KW_FALSE;}

readString              { printf("Token FN_readString:\t%s\n", yytext); return FN_readString; }
readInt                 { printf("Token FN_readInt:\t\t%s\n", yytext); return FN_readInt; }
readReal                { printf("Token FN_readReal:\t\t%s\n", yytext); return FN_readReal; }
writeString             { printf("Token FN_writeString:\t%s\n", yytext); return FN_writeString; }
writeInt                { printf("Token FN_writeInt:\t\t%s\n", yytext); return FN_writeInt; }
writeReal               { printf("Token FN_writeReal:\t\t%s\n", yytext); return FN_writeReal; }

{ID}                    { printf("Token IDENTIFIER:\t\t%s\n", yytext); yylval.str = strdup(yytext); return IDENTIFIER;}
{NUMBER}                { printf("Token CONST_INT:\t\t%s\n", yytext); yylval.str = strdup(yytext); return CONST_INT;}
{REAL}                  { printf("Token CONST_REAL:\t\t%s\n", yytext); yylval.str = strdup(yytext); return CONST_REAL;}
{STRING}                { printf("Token CONST_STRING:\t\t%s\n", yytext); yylval.str = strdup(yytext); return CONST_STRING;}

"+"                     { printf("Token PLUS_OP:\t\t\t+\n"); return '+';}
"-"                     { printf("Token MINUS_OP:\t\t\t-\n"); return '-';}
"*"                     { printf("Token MULTIPLY_OP:\t\t*\n"); return '*';}
"/"                     { printf("Token DIVIDE_OP:\t\t/\n"); return '/';}
"%"                     { printf("Token REMAINDER_OP:\t\t%%\n"); return '%';}
"**"                    { printf("Token POWER_OP:\t\t\t**\n"); return POW_OP;}
"=="                    { printf("Token EQUAL_OP:\t\t\t==\n"); return EQ_OP;}
"!="                    { printf("Token NOT_EQUAL_OP:\t\t!=\n"); return NEQ_OP;}
"<"                     { printf("Token LESS_OP:\t\t\t<\n"); return '<';}
"<="                    { printf("Token LESS_EQUAL_OP:\t\t<=\n"); return LE_OP;}
">"                     { printf("Token GREATER_OP:\t\t>\n"); return '>';}
">="                    { printf("Token GREATER_EQUAL_OP:\t\t<=\n"); return GE_OP;}
"="                     { printf("Token ASSIGN_OP:\t\t=\n"); return ASSIGN;}

";"                     { printf("Token SEMICOLON:\t\t;\n"); return ';';}
"("                     { printf("Token LEFT_PARENTHESIS:\t\t(\n"); return '(';}
")"                     { printf("Token RIGHT_PARENTHESIS:\t)\n"); return ')';}
","                     { printf("Token COMMA:\t\t\t,\n"); return ',';}
"["                     { printf("Token LEFT_BRACKET:\t\t[\n"); return '[';}
"]"                     { printf("Token RIGHT_BRACKET:\t\t]\n"); return ']';}
"{"                     { printf("Token LEFT_CURLY_BRACKET:\t{\n"); return '{';}
"}"                     { printf("Token RIGHT_CURLY_BRACKET:\t}\n"); return '}';}

"//"[^\n]*              /* eat line comments */

"/*"                    { BEGIN(comment); }
<comment>[^*\n]*        /* eat anything that's not a '*' */
<comment>"*"+[^*/\n]*   /* eat up '*'s not followed by '/'s */
<comment>\n             ++lineNum;
<comment>"*"+"/"        { BEGIN(INITIAL); }

<<EOF>> 	return EOF;

[ \r\t]+     /* skip whitespace */
\n          ++lineNum;
.           { printf("Line %d Lexical Error: Unrecognized literal %s\n", lineNum, yytext); }

%%
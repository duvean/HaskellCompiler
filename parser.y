%code requires { #include "ast.h" }
%debug
%{

#include "ast.h"
#include "token_queue.h"
#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
using namespace std;

ProgramNode* root = nullptr;
extern int yylex(void);
int get_next_token(void);
#define yylex get_next_token
void yyerror(const char *s);
%}

%union {
    ProgramNode* program;
    DeclNode* decl;
    DeclListNode* decl_list;
    ExprNode* expr;
    char* str;
    int intVal;
    double floatVal;
    char* strVal;
}

%type <program> program
%type <decl> decl func_signature func_definition var_decl data_decl binding
%type <decl_list> decl_list decl_list_opt binding_list binding_list_opt
%type <decl_list> opt_where decl_block constr_list param_list
%type <expr> expr expr_list expr_list_opt expr_list_tail
%type <expr> type_expr
%type <expr> case_branch_list_opt case_branch_list case_branch
%type <expr> do_block do_stmt_list do_stmt
%type <expr> pattern pattern_list

/* ==== Терминалы ==== */
%token KW_LET KW_WHERE KW_DO KW_OF KW_IN KW_CASE KW_IF KW_THEN KW_ELSE
%token KW_DATA KW_TYPE KW_RETURN
%token KW_TRUE KW_FALSE
%token <str> ID ID_CAP
%token <str> DEC_LITERAL HEX_LITERAL OCT_LITERAL FLOAT
%token <strVal> CHAR_LITERAL STRING_LITERAL
%token RIGHT_ARROW LEFT_ARROW FAT_ARROW DOUBLE_COLON
%token EQUALS PIPE COLON COMMA SEMICOLON BACKSLASH
%token LEFT_PAREN RIGHT_PAREN LEFT_BRACKET RIGHT_BRACKET LEFT_BRACE RIGHT_BRACE
%token PLUS MINUS ASTERISK SLASH CARET DOUBLE_ASTERISK DOUBLE_CARET
%token LESS GREATER LESS_EQUAL GREATER_EQUAL DOUBLE_EQUALS NOT_EQUALS
%token DOUBLE_AMPERSAND DOUBLE_PIPE DOLLAR DOT
%token SINGLELINE_COMMENT DOUBLE_PLUS DOUBLE_DOT DOUBLE_BANG DOT_AMPERSAND_DOT DOT_PIPE_DOT DOUBLE_GREATER_EQUAL DOUBLE_GREATER EQUAL_DOUBLE_LESS AT_SIGN TILDE BANG PERCENT KW_DEFAULT KW_CLASS KW_INSTANCE KW_DERIVING KW_IMPORT KW_MODULE KW_FOREIGN KW_INFIX KW_INFIXR KW_AS KW_HIDING KW_QUALIFIED KW_NEWTYPE KW_EXPORT KW_CCALL KW_PRINT KW_GETLINE KW_INT KW_INTEGER KW_CHAR KW_STRING KW_FLOAT KW_BOOL BACKTICK KW_INFIXL AMPERSAND OPERATOR LBRACE RBRACE

/* ==== Приоритеты ==== */
%left DOT
%right DOLLAR
%left DOUBLE_PIPE
%left DOUBLE_AMPERSAND
%nonassoc DOUBLE_EQUALS NOT_EQUALS LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left ASTERISK SLASH
%left DOUBLE_BANG
%right CARET DOUBLE_ASTERISK DOUBLE_CARET
%right COLON
%right RIGHT_ARROW LEFT_ARROW FAT_ARROW EQUALS DOUBLE_COLON
%nonassoc KW_IF KW_THEN KW_ELSE
%nonassoc KW_RETURN
%nonassoc KW_LET
/* Маркер для завершения do-оператора: самая низкая приоритетность.
   Правило do_stmt: expr будет иметь этот приоритет, поэтому при появлении
   оператора с более высоким приоритетом бизон предпочтёт шифт (продолжить expr).
*/
%nonassoc DO_STMT_TERM

%start program

%%

program: decl_list { root = ProgramNode::create($1); $$ = root; }

decl_list:
      decl_list decl { $$ = DeclListNode::addDecl($1, $2); }
    | decl           { $$ = DeclListNode::create($1); }
    ;

decl_list_opt:
      decl_list  { $$ = $1; }
    | /* void */ { $$ = nullptr; }
    ;

decl_block:
      LEFT_BRACE decl_list_opt RIGHT_BRACE { $$ = $2; }
    ;

opt_where:
      KW_WHERE decl_block
    | /* void */
    ;

decl:
      func_signature
    | func_definition
    | var_decl
    | data_decl
    ;

/* --- Объявление переменной --- */
var_decl:
      KW_LET LEFT_BRACE binding_list_opt RIGHT_BRACE { $$ = DeclNode::createLetBlock($3); }
    ;

binding_list:
      binding_list binding { $$ = DeclListNode::addDecl($1, $2); }
    | binding              { $$ = DeclListNode::create($1); }
    ;

binding_list_opt:
      binding_list { $$ = $1; }
    | /* void */   { $$ = nullptr; }
    ;

binding:
      ID EQUALS expr SEMICOLON { $$ = DeclNode::createVarDecl($1, $3); }
      // Здесь могут быть и func_definition с 1+ аргументами, если они допускаются в let/where
    ;

/* --- Объявление функции --- */
func_definition:
      ID param_list EQUALS expr opt_where { $$ = DeclNode::createFuncDef($1, $2, $4, $5); }
    ;

func_signature:
      ID DOUBLE_COLON type_expr SEMICOLON { $$ = DeclNode::createFuncSignature($1, $3); }
    ;

type_expr:
    | KW_INT
    | KW_FLOAT
    | KW_CHAR
    | KW_STRING
    | KW_BOOL
    | ID_CAP               // Конструктор типа (напр., Int, Maybe)
    | ID                   // Переменная типа (напр., a, b)
    | type_expr RIGHT_ARROW type_expr // Функциональный тип (напр., Int -> Int)
    | LEFT_PAREN type_expr RIGHT_PAREN
    ;

/* --- Определение типа данных --- */
data_decl:
      KW_DATA ID_CAP EQUALS constr_list { $$ = DeclNode::createDataDecl($2, $4); }
    ;

constr_list:
      constr_list PIPE ID_CAP
    | ID_CAP
    ;












/* --- Выражения --- */
expr:
      DEC_LITERAL { $$ = ExprNode::createLiteral($1); }
    | HEX_LITERAL { $$ = ExprNode::createLiteral($1); }
    | OCT_LITERAL { $$ = ExprNode::createLiteral($1); }
    | FLOAT       { $$ = ExprNode::createLiteral($1); }
    | KW_TRUE     { $$ = ExprNode::createLiteral("true"); }
    | KW_FALSE    { $$ = ExprNode::createLiteral("false"); }
	| CHAR_LITERAL { $$ = ExprNode::createLiteral($1); }
	| STRING_LITERAL { $$ = ExprNode::createLiteral($1); }
    | ID          { $$ = ExprNode::createVarRef($1); }
    | ID_CAP      { $$ = ExprNode::createVarRef($1); }
    | LEFT_BRACKET expr_list_opt RIGHT_BRACKET { $$ = ExprNode::createArrayExpr($2); }
    | LEFT_PAREN expr_list_opt RIGHT_PAREN     { $$ = ExprNode::createTupleExpr($2); }
    | LEFT_PAREN expr RIGHT_PAREN              { $$ = $2; }
	
	/* применение функций: f x */
	| expr expr { $$ = ExprNode::createFuncCall($1, $2); }

    /* арифметика */
    | expr PLUS expr { $$ = ExprNode::createBinaryExpr("+", $1, $3); }
    | expr MINUS expr
    | expr ASTERISK expr
    | expr SLASH expr
	| expr DOUBLE_BANG expr { $$ = ExprNode::createBinaryExpr("!!", $1, $3); }
    | expr CARET expr
    | expr DOUBLE_ASTERISK expr
    | expr DOUBLE_CARET expr

    /* логика и сравнение */
    | expr DOUBLE_EQUALS expr
    | expr NOT_EQUALS expr
    | expr LESS expr
    | expr GREATER expr
    | expr LESS_EQUAL expr
    | expr GREATER_EQUAL expr
    | expr DOUBLE_AMPERSAND expr
    | expr DOUBLE_PIPE expr

    /* функциональные операторы */
    | expr COLON expr
    | expr DOLLAR expr
    | expr DOT expr
    | expr DOUBLE_COLON type_expr  /* аннотация типа */

    /* лямбда */
    | BACKSLASH expr RIGHT_ARROW expr { $$ = ExprNode::createLambda($2, $4); }

    /* if / case */
    | KW_IF expr KW_THEN expr KW_ELSE expr %prec KW_IF { $$ = ExprNode::createIfExpr($2, $4, $6); }
    | KW_CASE expr KW_OF LEFT_BRACE case_branch_list_opt RIGHT_BRACE { $$ = ExprNode::createCaseExpr($2, $5); }

    /* let ... in ... */
    | KW_LET decl_list KW_IN expr %prec KW_LET { $$ = ExprNode::createLetInExpr($2, $4); }

    /* do-нотация */
    | KW_DO do_block { $$ = ExprNode::createDoExpr($2); }

    /* return */
    | KW_RETURN expr %prec KW_RETURN { $$ = ExprNode::createReturnExpr($2); }
    ;

expr_list_opt:
      expr_list  { $$ = $1; }
    | /* void */ { $$ = nullptr; }
    ;

expr_list:
      expr COMMA expr_list_tail {
            ExprNode* list = ExprNode::createExprList($1);
            $$ = ExprNode::addExprToList(list, $3);
        }
    ;
    
expr_list_tail:
      expr { $$ = ExprNode::createExprList($1); }
    | expr COMMA expr_list_tail { 
          ExprNode* list = ExprNode::createExprList($1);
          $$ = ExprNode::addExprToList(list, $3); 
      }
    ;



/* --- Сопоставление с образцом (Patterns) --- */
pattern:
	  ID							                        { $$ = ExprNode::createVarPattern($1); } // Переменная или Wildcard (если ID = "_")
	| DEC_LITERAL				                      { $$ = ExprNode::createLiteralPattern($1); }
	| KW_TRUE					                        { $$ = ExprNode::createLiteralPattern("true"); }
	| ID_CAP					                        { $$ = ExprNode::createConstructorPattern($1, nullptr); } // Конструктор без аргументов
  | LEFT_PAREN RIGHT_PAREN                  { $$ = nullptr; }
	| LEFT_PAREN pattern_list RIGHT_PAREN		  { $$ = ExprNode::createTuplePattern($2); } // Образец кортежа
	| LEFT_BRACKET pattern_list RIGHT_BRACKET	{ $$ = ExprNode::createListPattern($2); } // Образец списка
	| ID_CAP pattern							            { $$ = ExprNode::createConstructorPattern($1, $2); } // Just x
	| pattern COLON pattern				            { $$ = ExprNode::createConsPattern($1, $3); } // x : xs
	| LEFT_PAREN pattern RIGHT_PAREN	        { $$ = $2; } // Группировка
	;

pattern_list:
	pattern COMMA pattern_list		{ $$ = ExprNode::addPatternToList($1, $3); }
	| pattern						{ $$ = ExprNode::createPatternList($1); }
	;



/* --- Список параметров функции --- */
param_list:
      param_list pattern { $$ = DeclListNode::addParamToList($1, $2); }
    | pattern            { $$ = DeclListNode::createParamList($1); }
    ;

/* --- Ветки case --- */
case_branch_list_opt:
      case_branch_list
    | /* void (case x of {}) */
    ;

case_branch_list:
      case_branch
    | case_branch_list SEMICOLON case_branch
    ;

case_branch:
      pattern RIGHT_ARROW expr
    ;

/* --- do-блок --- */
do_block:
      LEFT_BRACE do_stmt_list RIGHT_BRACE
    ;

do_stmt_list:
      do_stmt_list SEMICOLON do_stmt
    | do_stmt
    ;

do_stmt:
      expr LEFT_ARROW expr       /* x <- action  — однозначно */
    | expr %prec DO_STMT_TERM    /* действие-выражение: reduce только если lookahead имеет НИЖ. приоритет */
    ;

%%

void yyerror(const char *s) {
    extern int yylineno;
    extern char *yytext;
    fprintf(stderr, "Parser error: %s at line %d, near token '%s'\n", s, yylineno, yytext);
}

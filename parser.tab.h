
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton interface for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* "%code requires" blocks.  */

/* Line 1676 of yacc.c  */
#line 1 "parser.y"
 #include "ast.h" 


/* Line 1676 of yacc.c  */
#line 44 "parser.tab.h"

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     KW_LET = 258,
     KW_WHERE = 259,
     KW_DO = 260,
     KW_OF = 261,
     KW_IN = 262,
     KW_CASE = 263,
     KW_IF = 264,
     KW_THEN = 265,
     KW_ELSE = 266,
     KW_DATA = 267,
     KW_TYPE = 268,
     KW_RETURN = 269,
     KW_TRUE = 270,
     KW_FALSE = 271,
     ID = 272,
     ID_CAP = 273,
     DEC_LITERAL = 274,
     HEX_LITERAL = 275,
     OCT_LITERAL = 276,
     FLOAT = 277,
     CHAR_LITERAL = 278,
     STRING_LITERAL = 279,
     RIGHT_ARROW = 280,
     LEFT_ARROW = 281,
     FAT_ARROW = 282,
     DOUBLE_COLON = 283,
     EQUALS = 284,
     PIPE = 285,
     COLON = 286,
     COMMA = 287,
     SEMICOLON = 288,
     BACKSLASH = 289,
     LEFT_PAREN = 290,
     RIGHT_PAREN = 291,
     LEFT_BRACKET = 292,
     RIGHT_BRACKET = 293,
     LEFT_BRACE = 294,
     RIGHT_BRACE = 295,
     PLUS = 296,
     MINUS = 297,
     ASTERISK = 298,
     SLASH = 299,
     CARET = 300,
     DOUBLE_ASTERISK = 301,
     DOUBLE_CARET = 302,
     LESS = 303,
     GREATER = 304,
     LESS_EQUAL = 305,
     GREATER_EQUAL = 306,
     DOUBLE_EQUALS = 307,
     NOT_EQUALS = 308,
     DOUBLE_AMPERSAND = 309,
     DOUBLE_PIPE = 310,
     DOLLAR = 311,
     DOT = 312,
     SINGLELINE_COMMENT = 313,
     DOUBLE_PLUS = 314,
     DOUBLE_DOT = 315,
     DOUBLE_BANG = 316,
     DOT_AMPERSAND_DOT = 317,
     DOT_PIPE_DOT = 318,
     DOUBLE_GREATER_EQUAL = 319,
     DOUBLE_GREATER = 320,
     EQUAL_DOUBLE_LESS = 321,
     AT_SIGN = 322,
     TILDE = 323,
     BANG = 324,
     PERCENT = 325,
     KW_DEFAULT = 326,
     KW_CLASS = 327,
     KW_INSTANCE = 328,
     KW_DERIVING = 329,
     KW_IMPORT = 330,
     KW_MODULE = 331,
     KW_FOREIGN = 332,
     KW_INFIX = 333,
     KW_INFIXR = 334,
     KW_AS = 335,
     KW_HIDING = 336,
     KW_QUALIFIED = 337,
     KW_NEWTYPE = 338,
     KW_EXPORT = 339,
     KW_CCALL = 340,
     KW_PRINT = 341,
     KW_GETLINE = 342,
     KW_INT = 343,
     KW_INTEGER = 344,
     KW_CHAR = 345,
     KW_STRING = 346,
     KW_FLOAT = 347,
     KW_BOOL = 348,
     BACKTICK = 349,
     KW_INFIXL = 350,
     AMPERSAND = 351,
     OPERATOR = 352,
     LBRACE = 353,
     RBRACE = 354,
     APPLY_PREC = 355,
     DO_STMT_TERM = 356
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 1676 of yacc.c  */
#line 20 "parser.y"

    ProgramNode* program;
    DeclNode* decl;
    DeclListNode* decl_list;
    ExprNode* expr;
    char* str;
    int intVal;
    double floatVal;
    char* strVal;



/* Line 1676 of yacc.c  */
#line 175 "parser.tab.h"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif

extern YYSTYPE yylval;



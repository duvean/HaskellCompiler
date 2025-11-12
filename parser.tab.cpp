
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 3 "parser.y"


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


/* Line 189 of yacc.c  */
#line 91 "parser.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

/* "%code requires" blocks.  */

/* Line 209 of yacc.c  */
#line 1 "parser.y"
 #include "ast.h" 


/* Line 209 of yacc.c  */
#line 119 "parser.tab.c"

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

/* Line 214 of yacc.c  */
#line 20 "parser.y"

    ProgramNode* program;
    DeclNode* decl;
    DeclListNode* decl_list;
    ExprNode* expr;
    char* str;
    int intVal;
    double floatVal;
    char* strVal;



/* Line 214 of yacc.c  */
#line 250 "parser.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 262 "parser.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  25
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1098

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  102
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  109
/* YYNRULES -- Number of states.  */
#define YYNSTATES  185

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   356

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     8,    10,    12,    13,    17,    20,
      21,    23,    25,    27,    29,    31,    36,    39,    41,    43,
      44,    49,    55,    60,    61,    63,    65,    67,    69,    71,
      73,    75,    79,    83,    88,    92,    94,    96,    98,   100,
     102,   104,   106,   108,   110,   112,   114,   118,   122,   126,
     128,   131,   135,   139,   143,   147,   151,   155,   159,   163,
     167,   171,   175,   179,   183,   187,   191,   195,   199,   203,
     207,   211,   216,   223,   230,   234,   237,   240,   242,   243,
     247,   249,   253,   255,   257,   259,   261,   263,   266,   270,
     273,   277,   280,   284,   288,   292,   294,   297,   299,   301,
     302,   306,   309,   313,   317,   321,   323,   327,   329,   331
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     103,     0,    -1,   104,    -1,   104,   108,    -1,   108,    -1,
     104,    -1,    -1,    39,   105,    40,    -1,     4,   106,    -1,
      -1,   114,    -1,   113,    -1,   109,    -1,   116,    -1,   112,
      -1,     3,    39,   111,    40,    -1,   110,   112,    -1,   112,
      -1,   110,    -1,    -1,    17,    29,   119,   132,    -1,    17,
     125,    29,   119,   107,    -1,    17,    28,   115,    33,    -1,
      -1,    88,    -1,    92,    -1,    90,    -1,    91,    -1,    93,
      -1,    18,    -1,    17,    -1,   115,    25,   115,    -1,    35,
     115,    36,    -1,    12,    18,    29,   117,    -1,   117,    30,
      18,    -1,    18,    -1,    19,    -1,    20,    -1,    21,    -1,
      22,    -1,    15,    -1,    16,    -1,    23,    -1,    24,    -1,
      17,    -1,    18,    -1,    37,   120,    38,    -1,    35,   120,
      36,    -1,    35,   119,    36,    -1,   118,    -1,   119,   118,
      -1,   119,    41,   119,    -1,   119,    42,   119,    -1,   119,
      43,   119,    -1,   119,    44,   119,    -1,   119,    61,   119,
      -1,   119,    45,   119,    -1,   119,    46,   119,    -1,   119,
      47,   119,    -1,   119,    52,   119,    -1,   119,    53,   119,
      -1,   119,    48,   119,    -1,   119,    49,   119,    -1,   119,
      50,   119,    -1,   119,    51,   119,    -1,   119,    55,   119,
      -1,   119,    54,   119,    -1,   119,    31,   119,    -1,   119,
      56,   119,    -1,   119,    57,   119,    -1,   119,    28,   115,
      -1,    34,   119,    25,   119,    -1,     9,   119,    10,   119,
      11,   119,    -1,     8,   119,     6,    39,   126,    40,    -1,
     109,     7,   119,    -1,     5,   129,    -1,    14,   119,    -1,
     121,    -1,    -1,   119,    32,   122,    -1,   119,    -1,   119,
      32,   122,    -1,    17,    -1,    19,    -1,    15,    -1,    16,
      -1,    18,    -1,    35,    36,    -1,    35,   124,    36,    -1,
      37,    38,    -1,    37,   124,    38,    -1,    18,   123,    -1,
     123,    31,   123,    -1,    35,   123,    36,    -1,   123,    32,
     124,    -1,   123,    -1,   125,   123,    -1,   123,    -1,   127,
      -1,    -1,   128,    33,   127,    -1,   128,    33,    -1,   123,
      25,   119,    -1,    39,   130,    40,    -1,   130,    33,   131,
      -1,   131,    -1,   119,    26,   119,    -1,   119,    -1,    33,
      -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    82,    82,    85,    86,    90,    91,    95,    99,   100,
     104,   105,   106,   107,   108,   113,   117,   118,   122,   123,
     127,   133,   137,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   154,   158,   159,   175,   176,   177,   178,
     179,   180,   181,   182,   183,   184,   185,   186,   187,   190,
     192,   195,   196,   197,   198,   199,   200,   201,   202,   205,
     206,   207,   208,   209,   210,   213,   214,   217,   218,   219,
     220,   223,   226,   227,   230,   233,   236,   240,   241,   245,
     252,   253,   263,   264,   265,   266,   267,   268,   269,   270,
     271,   272,   273,   274,   278,   279,   286,   287,   292,   293,
     297,   298,   302,   307,   311,   312,   316,   317,   321,   322
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "KW_LET", "KW_WHERE", "KW_DO", "KW_OF",
  "KW_IN", "KW_CASE", "KW_IF", "KW_THEN", "KW_ELSE", "KW_DATA", "KW_TYPE",
  "KW_RETURN", "KW_TRUE", "KW_FALSE", "ID", "ID_CAP", "DEC_LITERAL",
  "HEX_LITERAL", "OCT_LITERAL", "FLOAT", "CHAR_LITERAL", "STRING_LITERAL",
  "RIGHT_ARROW", "LEFT_ARROW", "FAT_ARROW", "DOUBLE_COLON", "EQUALS",
  "PIPE", "COLON", "COMMA", "SEMICOLON", "BACKSLASH", "LEFT_PAREN",
  "RIGHT_PAREN", "LEFT_BRACKET", "RIGHT_BRACKET", "LEFT_BRACE",
  "RIGHT_BRACE", "PLUS", "MINUS", "ASTERISK", "SLASH", "CARET",
  "DOUBLE_ASTERISK", "DOUBLE_CARET", "LESS", "GREATER", "LESS_EQUAL",
  "GREATER_EQUAL", "DOUBLE_EQUALS", "NOT_EQUALS", "DOUBLE_AMPERSAND",
  "DOUBLE_PIPE", "DOLLAR", "DOT", "SINGLELINE_COMMENT", "DOUBLE_PLUS",
  "DOUBLE_DOT", "DOUBLE_BANG", "DOT_AMPERSAND_DOT", "DOT_PIPE_DOT",
  "DOUBLE_GREATER_EQUAL", "DOUBLE_GREATER", "EQUAL_DOUBLE_LESS", "AT_SIGN",
  "TILDE", "BANG", "PERCENT", "KW_DEFAULT", "KW_CLASS", "KW_INSTANCE",
  "KW_DERIVING", "KW_IMPORT", "KW_MODULE", "KW_FOREIGN", "KW_INFIX",
  "KW_INFIXR", "KW_AS", "KW_HIDING", "KW_QUALIFIED", "KW_NEWTYPE",
  "KW_EXPORT", "KW_CCALL", "KW_PRINT", "KW_GETLINE", "KW_INT",
  "KW_INTEGER", "KW_CHAR", "KW_STRING", "KW_FLOAT", "KW_BOOL", "BACKTICK",
  "KW_INFIXL", "AMPERSAND", "OPERATOR", "LBRACE", "RBRACE", "APPLY_PREC",
  "DO_STMT_TERM", "$accept", "program", "decl_list", "decl_list_opt",
  "decl_block", "opt_where", "decl", "let_block", "binding_list",
  "binding_list_opt", "binding", "func_definition", "func_signature",
  "type_expr", "data_decl", "constr_list", "basic_expr", "expr",
  "expr_list_opt", "expr_list", "expr_list_tail", "pattern",
  "pattern_list", "param_list", "case_branch_list_opt", "case_branch_list",
  "case_branch", "do_block", "do_stmt_list", "do_stmt", "semicolon_opt", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   102,   103,   104,   104,   105,   105,   106,   107,   107,
     108,   108,   108,   108,   108,   109,   110,   110,   111,   111,
     112,   113,   114,   115,   115,   115,   115,   115,   115,   115,
     115,   115,   115,   116,   117,   117,   118,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   118,   119,
     119,   119,   119,   119,   119,   119,   119,   119,   119,   119,
     119,   119,   119,   119,   119,   119,   119,   119,   119,   119,
     119,   119,   119,   119,   119,   119,   119,   120,   120,   121,
     122,   122,   123,   123,   123,   123,   123,   123,   123,   123,
     123,   123,   123,   123,   124,   124,   125,   125,   126,   126,
     127,   127,   128,   129,   130,   130,   131,   131,   132,   132
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     0,     3,     2,     0,
       1,     1,     1,     1,     1,     4,     2,     1,     1,     0,
       4,     5,     4,     0,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     4,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     3,     1,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     4,     6,     6,     3,     2,     2,     1,     0,     3,
       1,     3,     1,     1,     1,     1,     1,     2,     3,     2,
       3,     2,     3,     3,     3,     1,     2,     1,     1,     0,
       3,     2,     3,     3,     3,     1,     3,     1,     1,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     0,     2,     4,    12,    14,    11,
      10,    13,    19,     0,    84,    85,    82,    86,    83,    23,
       0,     0,     0,    97,     0,     1,     3,     0,    18,     0,
      17,     0,    91,    30,    29,    23,    24,    26,    27,    25,
      28,     0,     0,     0,     0,     0,    40,    41,    44,    45,
      36,    37,    38,    39,    42,    43,     0,    78,    78,     0,
      49,   109,    87,     0,     0,    89,    95,     0,     0,     0,
      96,    16,    15,    35,    33,     0,    23,    22,     0,    75,
       0,     0,    76,     0,     0,     0,    77,     0,     0,     0,
      23,     0,   108,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    50,    20,     0,    93,    88,    90,    92,     9,     0,
      32,    31,   107,     0,   105,     0,     0,     0,     0,    48,
      47,    46,    74,    70,    67,    51,    52,    53,    54,    56,
      57,    58,    61,    62,    63,    64,    59,    60,    66,    65,
      68,    69,    55,    94,     0,    21,    34,     0,     0,   103,
      99,     0,    71,    80,    79,     6,     8,   106,   104,     0,
       0,    98,     0,     0,     0,     5,     0,     0,    73,   101,
      72,    81,     7,   102,   100
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,     5,   176,   166,   155,     6,    59,    28,    29,
       8,     9,    10,    41,    11,    74,    60,   122,    85,    86,
     164,    66,    64,    24,   170,   171,   172,    79,   123,   124,
     112
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -95
static const yytype_int16 yypact[] =
{
       5,   -30,     9,  1056,    32,     5,   -95,   -95,   -95,   -95,
     -95,   -95,    22,    11,   -95,   -95,   -95,   211,   -95,    82,
     944,   957,    18,    14,  1061,   -95,   -95,    17,    22,     8,
     -95,    31,    14,   -95,   -95,    82,   -95,   -95,   -95,   -95,
     -95,    -9,    12,   944,   944,   944,   -95,   -95,   -95,   -95,
     -95,   -95,   -95,   -95,   -95,   -95,   944,   944,   944,    43,
     -95,   405,   -95,   -21,    16,   -95,    -6,    19,   211,   944,
      14,   -95,   -95,   -95,    29,   -13,    82,   -95,   944,   -95,
     216,   264,  1046,   452,   358,    24,   -95,   499,    23,   944,
      82,   944,   -95,   944,   944,   944,   944,   944,   944,   944,
     944,   944,   944,   944,   944,   944,   944,   944,   944,   944,
     944,   -95,   -95,   211,   -95,   -95,   -95,    14,   164,    44,
     -95,    38,   546,    -2,   -95,    25,   944,   944,   944,   -95,
     -95,   -95,   640,    38,  1000,   875,   875,   909,   909,   967,
     967,   967,   781,   781,   781,   781,   781,   781,   828,   734,
     687,   687,   967,   -95,    33,   -95,   -95,   944,   944,   -95,
     211,   311,  1023,   593,   -95,     5,   -95,   640,   -95,   -12,
      35,   -95,    63,   944,   944,     5,    37,   944,   -95,   211,
    1046,   -95,   -95,   640,   -95
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -95,   -95,   -94,   -95,   -95,   -95,    -4,     2,   -95,   -95,
      -8,   -95,   -95,   -32,   -95,   -95,   -14,   -15,    40,   -95,
     -73,    -3,   -16,   -95,   -95,   -77,   -95,   -95,   -95,   -55,
     -95
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_int16 yytable[] =
{
      23,    26,     7,    75,    30,    61,    67,     7,     1,    12,
      68,   113,    76,   177,    32,   114,    76,     2,    63,    68,
      71,    70,     3,   120,    77,    68,   113,    13,    80,    81,
      82,   158,    25,    14,    15,    16,    17,    18,   159,    27,
      31,    83,    84,    87,   121,    68,    20,   111,    72,    73,
      89,    78,   115,    21,   118,    22,    65,   116,   133,   119,
     130,   131,   156,    76,   160,   117,   111,   111,   111,   111,
     111,   175,   165,   111,   132,   178,   134,   182,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   179,   153,    88,    33,
      34,   181,   184,   168,   111,     0,     0,     0,   111,     0,
       0,   161,   162,   163,     0,     0,     0,    35,   111,     0,
     111,   111,   111,   111,   111,   111,   111,   111,   111,   111,
     111,   111,   111,   111,   111,   111,   111,   111,   111,     0,
       0,     0,   167,     0,     0,     0,     0,   111,   111,   111,
       0,     0,     0,   111,     0,     0,     0,   169,   180,   163,
       0,     0,   183,     0,     0,     0,   111,     7,   154,   111,
      36,    26,    37,    38,    39,    40,   169,     7,     0,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    90,     0,     0,    91,     0,     0,     0,    57,
       0,    58,     0,     0,     0,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   125,     0,     0,   110,    14,    15,    16,    17,
      18,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,     0,     0,    90,     0,    21,    91,    22,     0,
       0,    57,     0,    58,     0,     0,     0,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,   126,     0,     0,   110,     0,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    90,     0,     0,    91,     0,     0,     0,    57,
       0,    58,     0,     0,     0,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     108,   109,   173,     0,     0,   110,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    90,
       0,     0,    91,     0,     0,     0,    57,     0,    58,     0,
       0,     0,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,     0,
       0,     0,   110,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    90,     0,     0,    91,
     128,     0,     0,    57,   129,    58,     0,     0,     0,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,   106,   107,   108,   109,     0,     0,     0,   110,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    90,     0,     0,    91,     0,    92,     0,
      57,     0,    58,     0,     0,     0,    93,    94,    95,    96,
      97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
     107,   108,   109,     0,     0,     0,   110,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,   127,     0,     0,
      90,     0,     0,    91,     0,     0,     0,    57,     0,    58,
       0,     0,     0,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
       0,     0,     0,   110,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,     0,    90,     0,     0,
      91,   128,     0,     0,    57,     0,    58,     0,     0,     0,
      93,    94,    95,    96,    97,    98,    99,   100,   101,   102,
     103,   104,   105,   106,   107,   108,   109,     0,     0,     0,
     110,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,     0,   157,     0,    90,     0,     0,    91,     0,     0,
       0,    57,     0,    58,     0,     0,     0,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,   107,   108,   109,     0,     0,     0,   110,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    90,     0,     0,    91,   174,     0,     0,    57,     0,
      58,     0,     0,     0,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,     0,     0,     0,   110,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    90,     0,
       0,    91,     0,     0,     0,    57,     0,    58,     0,     0,
       0,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,     0,     0,
       0,   110,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,     0,     0,     0,    90,     0,     0,    91,     0,
       0,     0,    57,     0,    58,     0,     0,     0,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,     0,     0,     0,     0,   110,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
       0,     0,    90,     0,     0,    91,     0,     0,     0,    57,
       0,    58,     0,     0,     0,    93,    94,    95,    96,    97,
      98,    99,   100,   101,   102,   103,   104,   105,   106,     0,
       0,     0,     0,     0,     0,   110,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,     0,     0,     0,    90,
       0,     0,    91,     0,     0,     0,    57,     0,    58,     0,
       0,     0,    93,    94,    95,    96,    97,    98,    99,    -1,
      -1,    -1,    -1,    -1,    -1,     0,     0,     0,     0,     0,
       0,     0,   110,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,     0,     0,     0,    90,     0,     0,    91,
       0,     0,     0,    57,     0,    58,     0,     0,     0,    93,
      94,    95,    96,    97,    98,    99,   100,   101,   102,   103,
     104,   105,     0,     0,     0,     0,     0,     0,     0,   110,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
       0,     0,     0,    90,     0,     0,    91,     0,     0,     0,
      57,     0,    58,     0,     0,     0,     0,     0,    95,    96,
      97,    98,    99,     0,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,     0,     0,   110,    90,     0,     0,
      91,     0,     0,     0,    57,     0,    58,     1,     0,    42,
       0,     0,    43,    44,    97,    98,    99,     0,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,     0,
     110,     0,    14,    15,    16,    17,    18,     0,    56,    57,
       0,    58,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    21,    62,    22,    90,     0,     0,    91,     0,
       0,     0,    57,     0,    58,     0,     0,     0,     0,     0,
       0,     0,    97,    98,    99,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,     0,     0,     0,    90,     0,
       0,    91,     0,     0,     0,    57,     0,    58,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,     0,     0,
       0,    90,     0,     0,     0,     0,     0,     0,    57,     0,
      58,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    14,    15,    16,    17,    18,    14,    15,    16,    17,
      18,    57,     0,    58,    19,    20,     0,     0,     0,     0,
      69,    21,     0,    22,     0,     0,    21,     0,    22
};

static const yytype_int16 yycheck[] =
{
       3,     5,     0,    35,    12,    20,    22,     5,     3,    39,
      31,    32,    25,    25,    17,    36,    25,    12,    21,    31,
      28,    24,    17,    36,    33,    31,    32,    18,    43,    44,
      45,    33,     0,    15,    16,    17,    18,    19,    40,    17,
      29,    56,    57,    58,    76,    31,    29,    61,    40,    18,
       7,    39,    36,    35,    69,    37,    38,    38,    90,    30,
      36,    38,    18,    25,    39,    68,    80,    81,    82,    83,
      84,   165,    39,    87,    89,    40,    91,    40,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,    33,   113,    58,    17,
      18,   174,   179,   158,   118,    -1,    -1,    -1,   122,    -1,
      -1,   126,   127,   128,    -1,    -1,    -1,    35,   132,    -1,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,    -1,
      -1,    -1,   157,    -1,    -1,    -1,    -1,   161,   162,   163,
      -1,    -1,    -1,   167,    -1,    -1,    -1,   160,   173,   174,
      -1,    -1,   177,    -1,    -1,    -1,   180,   165,     4,   183,
      88,   175,    90,    91,    92,    93,   179,   175,    -1,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    28,    -1,    -1,    31,    -1,    -1,    -1,    35,
      -1,    37,    -1,    -1,    -1,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,     6,    -1,    -1,    61,    15,    16,    17,    18,
      19,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    28,    -1,    35,    31,    37,    -1,
      -1,    35,    -1,    37,    -1,    -1,    -1,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    10,    -1,    -1,    61,    -1,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    28,    -1,    -1,    31,    -1,    -1,    -1,    35,
      -1,    37,    -1,    -1,    -1,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    57,    11,    -1,    -1,    61,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    28,
      -1,    -1,    31,    -1,    -1,    -1,    35,    -1,    37,    -1,
      -1,    -1,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    -1,
      -1,    -1,    61,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    28,    -1,    -1,    31,
      32,    -1,    -1,    35,    36,    37,    -1,    -1,    -1,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    57,    -1,    -1,    -1,    61,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    28,    -1,    -1,    31,    -1,    33,    -1,
      35,    -1,    37,    -1,    -1,    -1,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    -1,    -1,    -1,    61,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    -1,    -1,
      28,    -1,    -1,    31,    -1,    -1,    -1,    35,    -1,    37,
      -1,    -1,    -1,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      -1,    -1,    -1,    61,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    28,    -1,    -1,
      31,    32,    -1,    -1,    35,    -1,    37,    -1,    -1,    -1,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    -1,    -1,    -1,
      61,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    -1,    26,    -1,    28,    -1,    -1,    31,    -1,    -1,
      -1,    35,    -1,    37,    -1,    -1,    -1,    41,    42,    43,
      44,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    -1,    -1,    -1,    61,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    28,    -1,    -1,    31,    32,    -1,    -1,    35,    -1,
      37,    -1,    -1,    -1,    41,    42,    43,    44,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      57,    -1,    -1,    -1,    61,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    28,    -1,
      -1,    31,    -1,    -1,    -1,    35,    -1,    37,    -1,    -1,
      -1,    41,    42,    43,    44,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    -1,    -1,
      -1,    61,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    -1,    -1,    -1,    28,    -1,    -1,    31,    -1,
      -1,    -1,    35,    -1,    37,    -1,    -1,    -1,    41,    42,
      43,    44,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    61,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    28,    -1,    -1,    31,    -1,    -1,    -1,    35,
      -1,    37,    -1,    -1,    -1,    41,    42,    43,    44,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    -1,
      -1,    -1,    -1,    -1,    -1,    61,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    28,
      -1,    -1,    31,    -1,    -1,    -1,    35,    -1,    37,    -1,
      -1,    -1,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    61,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    -1,    -1,    -1,    28,    -1,    -1,    31,
      -1,    -1,    -1,    35,    -1,    37,    -1,    -1,    -1,    41,
      42,    43,    44,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    61,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    28,    -1,    -1,    31,    -1,    -1,    -1,
      35,    -1,    37,    -1,    -1,    -1,    -1,    -1,    43,    44,
      45,    46,    47,    -1,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    61,    28,    -1,    -1,
      31,    -1,    -1,    -1,    35,    -1,    37,     3,    -1,     5,
      -1,    -1,     8,     9,    45,    46,    47,    -1,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    -1,
      61,    -1,    15,    16,    17,    18,    19,    -1,    34,    35,
      -1,    37,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    35,    36,    37,    28,    -1,    -1,    31,    -1,
      -1,    -1,    35,    -1,    37,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    -1,    -1,    -1,    28,    -1,
      -1,    31,    -1,    -1,    -1,    35,    -1,    37,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    -1,    -1,
      -1,    28,    -1,    -1,    -1,    -1,    -1,    -1,    35,    -1,
      37,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    15,    16,    17,    18,    19,    15,    16,    17,    18,
      19,    35,    -1,    37,    28,    29,    -1,    -1,    -1,    -1,
      29,    35,    -1,    37,    -1,    -1,    35,    -1,    37
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,    12,    17,   103,   104,   108,   109,   112,   113,
     114,   116,    39,    18,    15,    16,    17,    18,    19,    28,
      29,    35,    37,   123,   125,     0,   108,    17,   110,   111,
     112,    29,   123,    17,    18,    35,    88,    90,    91,    92,
      93,   115,     5,     8,     9,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    34,    35,    37,   109,
     118,   119,    36,   123,   124,    38,   123,   124,    31,    29,
     123,   112,    40,    18,   117,   115,    25,    33,    39,   129,
     119,   119,   119,   119,   119,   120,   121,   119,   120,     7,
      28,    31,    33,    41,    42,    43,    44,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      61,   118,   132,    32,    36,    36,    38,   123,   119,    30,
      36,   115,   119,   130,   131,     6,    10,    25,    32,    36,
      36,    38,   119,   115,   119,   119,   119,   119,   119,   119,
     119,   119,   119,   119,   119,   119,   119,   119,   119,   119,
     119,   119,   119,   124,     4,   107,    18,    26,    33,    40,
      39,   119,   119,   119,   122,    39,   106,   119,   131,   123,
     126,   127,   128,    11,    32,   104,   105,    25,    40,    33,
     119,   122,    40,   119,   127
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{


    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1455 of yacc.c  */
#line 82 "parser.y"
    { root = ProgramNode::create((yyvsp[(1) - (1)].decl_list)); (yyval.program) = root; ;}
    break;

  case 3:

/* Line 1455 of yacc.c  */
#line 85 "parser.y"
    { (yyval.decl_list) = DeclListNode::addDecl((yyvsp[(1) - (2)].decl_list), (yyvsp[(2) - (2)].decl)); ;}
    break;

  case 4:

/* Line 1455 of yacc.c  */
#line 86 "parser.y"
    { (yyval.decl_list) = DeclListNode::create((yyvsp[(1) - (1)].decl)); ;}
    break;

  case 5:

/* Line 1455 of yacc.c  */
#line 90 "parser.y"
    { (yyval.decl_list) = (yyvsp[(1) - (1)].decl_list); ;}
    break;

  case 6:

/* Line 1455 of yacc.c  */
#line 91 "parser.y"
    { (yyval.decl_list) = nullptr; ;}
    break;

  case 7:

/* Line 1455 of yacc.c  */
#line 95 "parser.y"
    { (yyval.decl_list) = (yyvsp[(2) - (3)].decl_list); ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 113 "parser.y"
    { (yyval.decl) = DeclNode::createLetBlock((yyvsp[(3) - (4)].decl_list)); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 117 "parser.y"
    { (yyval.decl_list) = DeclListNode::addDecl((yyvsp[(1) - (2)].decl_list), (yyvsp[(2) - (2)].decl)); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 118 "parser.y"
    { (yyval.decl_list) = DeclListNode::create((yyvsp[(1) - (1)].decl)); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 122 "parser.y"
    { (yyval.decl_list) = (yyvsp[(1) - (1)].decl_list); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 123 "parser.y"
    { (yyval.decl_list) = nullptr; ;}
    break;

  case 20:

/* Line 1455 of yacc.c  */
#line 127 "parser.y"
    { (yyval.decl) = DeclNode::createVarDecl((yyvsp[(1) - (4)].str), (yyvsp[(3) - (4)].expr)); ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 133 "parser.y"
    { (yyval.decl) = DeclNode::createFuncDef((yyvsp[(1) - (5)].str), (yyvsp[(2) - (5)].decl_list), (yyvsp[(4) - (5)].expr), (yyvsp[(5) - (5)].decl_list)); ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 137 "parser.y"
    { (yyval.decl) = DeclNode::createFuncSignature((yyvsp[(1) - (4)].str), (yyvsp[(3) - (4)].expr)); ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 154 "parser.y"
    { (yyval.decl) = DeclNode::createDataDecl((yyvsp[(2) - (4)].str), (yyvsp[(4) - (4)].decl_list)); ;}
    break;

  case 36:

/* Line 1455 of yacc.c  */
#line 175 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral((yyvsp[(1) - (1)].str)); ;}
    break;

  case 37:

/* Line 1455 of yacc.c  */
#line 176 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral((yyvsp[(1) - (1)].str)); ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 177 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral((yyvsp[(1) - (1)].str)); ;}
    break;

  case 39:

/* Line 1455 of yacc.c  */
#line 178 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral((yyvsp[(1) - (1)].str)); ;}
    break;

  case 40:

/* Line 1455 of yacc.c  */
#line 179 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral("true"); ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 180 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral("false"); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 181 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral((yyvsp[(1) - (1)].strVal)); ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 182 "parser.y"
    { (yyval.expr) = ExprNode::createLiteral((yyvsp[(1) - (1)].strVal)); ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 183 "parser.y"
    { (yyval.expr) = ExprNode::createVarRef((yyvsp[(1) - (1)].str)); ;}
    break;

  case 45:

/* Line 1455 of yacc.c  */
#line 184 "parser.y"
    { (yyval.expr) = ExprNode::createVarRef((yyvsp[(1) - (1)].str)); ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 185 "parser.y"
    { (yyval.expr) = ExprNode::createArrayExpr((yyvsp[(2) - (3)].expr)); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 186 "parser.y"
    { (yyval.expr) = ExprNode::createTupleExpr((yyvsp[(2) - (3)].expr)); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 187 "parser.y"
    { (yyval.expr) = (yyvsp[(2) - (3)].expr); ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 192 "parser.y"
    { (yyval.expr) = ExprNode::createFuncCall((yyvsp[(1) - (2)].expr), (yyvsp[(2) - (2)].expr)); ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 195 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("+", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 196 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("-", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 197 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("*", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 199 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("!!", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 205 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("==", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 206 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("!=", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 207 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("<", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 208 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr(">", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 209 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("<=", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 210 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr(">=", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 213 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("||", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 214 "parser.y"
    { (yyval.expr) = ExprNode::createBinaryExpr("&&", (yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 223 "parser.y"
    { (yyval.expr) = ExprNode::createLambda((yyvsp[(2) - (4)].expr), (yyvsp[(4) - (4)].expr)); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 226 "parser.y"
    { (yyval.expr) = ExprNode::createIfExpr((yyvsp[(2) - (6)].expr), (yyvsp[(4) - (6)].expr), (yyvsp[(6) - (6)].expr)); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 227 "parser.y"
    { (yyval.expr) = ExprNode::createCaseExpr((yyvsp[(2) - (6)].expr), (yyvsp[(5) - (6)].expr)); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 230 "parser.y"
    { (yyval.expr) = ExprNode::createLetInExpr((yyvsp[(1) - (3)].decl), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 233 "parser.y"
    { (yyval.expr) = ExprNode::createDoExpr((yyvsp[(2) - (2)].expr)); ;}
    break;

  case 76:

/* Line 1455 of yacc.c  */
#line 236 "parser.y"
    { (yyval.expr) = ExprNode::createReturnExpr((yyvsp[(2) - (2)].expr)); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 240 "parser.y"
    { (yyval.expr) = (yyvsp[(1) - (1)].expr); ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 241 "parser.y"
    { (yyval.expr) = nullptr; ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 245 "parser.y"
    {
            ExprNode* list = ExprNode::createExprList((yyvsp[(1) - (3)].expr));
            (yyval.expr) = ExprNode::addExprToList(list, (yyvsp[(3) - (3)].expr));
        ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 252 "parser.y"
    { (yyval.expr) = ExprNode::createExprList((yyvsp[(1) - (1)].expr)); ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 253 "parser.y"
    { 
          ExprNode* list = ExprNode::createExprList((yyvsp[(1) - (3)].expr));
          (yyval.expr) = ExprNode::addExprToList(list, (yyvsp[(3) - (3)].expr)); 
      ;}
    break;

  case 82:

/* Line 1455 of yacc.c  */
#line 263 "parser.y"
    { (yyval.expr) = ExprNode::createVarPattern((yyvsp[(1) - (1)].str)); ;}
    break;

  case 83:

/* Line 1455 of yacc.c  */
#line 264 "parser.y"
    { (yyval.expr) = ExprNode::createLiteralPattern((yyvsp[(1) - (1)].str)); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 265 "parser.y"
    { (yyval.expr) = ExprNode::createLiteralPattern("True"); ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 266 "parser.y"
    { (yyval.expr) = ExprNode::createLiteralPattern("False"); ;}
    break;

  case 86:

/* Line 1455 of yacc.c  */
#line 267 "parser.y"
    { (yyval.expr) = ExprNode::createConstructorPattern((yyvsp[(1) - (1)].str), nullptr); ;}
    break;

  case 87:

/* Line 1455 of yacc.c  */
#line 268 "parser.y"
    { (yyval.expr) = nullptr; ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 269 "parser.y"
    { (yyval.expr) = ExprNode::createTuplePattern((yyvsp[(2) - (3)].expr)); ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 270 "parser.y"
    { (yyval.expr) = nullptr; ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 271 "parser.y"
    { (yyval.expr) = ExprNode::createListPattern((yyvsp[(2) - (3)].expr)); ;}
    break;

  case 91:

/* Line 1455 of yacc.c  */
#line 272 "parser.y"
    { (yyval.expr) = ExprNode::createConstructorPattern((yyvsp[(1) - (2)].str), (yyvsp[(2) - (2)].expr)); ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 273 "parser.y"
    { (yyval.expr) = ExprNode::createConsPattern((yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 274 "parser.y"
    { (yyval.expr) = (yyvsp[(2) - (3)].expr); ;}
    break;

  case 94:

/* Line 1455 of yacc.c  */
#line 278 "parser.y"
    { (yyval.expr) = ExprNode::addPatternToList((yyvsp[(1) - (3)].expr), (yyvsp[(3) - (3)].expr)); ;}
    break;

  case 95:

/* Line 1455 of yacc.c  */
#line 279 "parser.y"
    { (yyval.expr) = ExprNode::createPatternList((yyvsp[(1) - (1)].expr)); ;}
    break;

  case 96:

/* Line 1455 of yacc.c  */
#line 286 "parser.y"
    { (yyval.decl_list) = DeclListNode::addParamToList((yyvsp[(1) - (2)].decl_list), (yyvsp[(2) - (2)].expr)); ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 287 "parser.y"
    { (yyval.decl_list) = DeclListNode::createParamList((yyvsp[(1) - (1)].expr)); ;}
    break;



/* Line 1455 of yacc.c  */
#line 2316 "parser.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 325 "parser.y"


void yyerror(const char *s) {
    extern int yylineno;
    extern char *yytext;
    fprintf(stderr, "Parser error: %s at line %d, near token '%s'\n", s, yylineno, yytext);
}


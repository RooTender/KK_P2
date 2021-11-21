/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_P_TAB_H_INCLUDED
# define YY_YY_P_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    KW_PROGRAM = 258,
    KW_BEGIN = 259,
    KW_END = 260,
    KW_USES = 261,
    KW_VAR = 262,
    KW_CONST = 263,
    KW_IF = 264,
    KW_THEN = 265,
    KW_ELSE = 266,
    KW_CHAR = 267,
    KW_STRING = 268,
    KW_INTEGER = 269,
    KW_REAL = 270,
    KW_FOR = 271,
    KW_TO = 272,
    KW_DO = 273,
    KW_FUNCTION = 274,
    KW_PROCEDURE = 275,
    KW_ARRAY = 276,
    KW_RECORD = 277,
    KW_OF = 278,
    KW_DOWNTO = 279,
    ASSIGN = 280,
    LE = 281,
    RANGE = 282,
    IDENT = 283,
    STRING_CONST = 284,
    FLOAT_CONST = 285,
    INTEGER_CONST = 286,
    CHARACTER_CONST = 287,
    NEG = 288
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 14 "p.y"

	char s[ MAX_STR_LEN + 1 ];
	int i;
	double d;

#line 97 "p.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_P_TAB_H_INCLUDED  */

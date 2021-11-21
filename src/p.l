%option yylineno
%{
#include <stdio.h> /* printf() */
#include <string.h> /* strcpy */
#include <stdlib.h> /* atoi(), atof() */
#include "common.h" /* MAX_STR_LEN */
#include "p.tab.h" /* deklaracja symboli końcowych */

int process_token(const char *text, const char *TokenType,
                  const char *TokenVal, const int TokenID); 

int comm_beg = 0; /* wiersz rozpoczęcia komentarza */
%}


/* deklaracja dodatkowych warunków początkowych analizatora leksykalnego */
/* (nie deklarujemy domyślnego warunku początkowego INITIAL */
/* ..................... */

%x  ST_COMMENT_1 ST_COMMENT_2 ST_TEXT_CONST
d [0-9]

%%


 /* Wykrywanie napisów z użyciem warunków początkowych */
<INITIAL>\'       { BEGIN(ST_TEXT_CONST); yymore(); }
<ST_TEXT_CONST>\' { BEGIN(INITIAL); return process_token(yytext, "STRING_CONST", yytext, STRING_CONST);  }
<ST_TEXT_CONST>.  { yymore(); }
<ST_TEXT_CONST>\n { printf("Brak zamknięcia napisu otwartego w linii %d\n", yylineno); BEGIN(INITIAL); }

 /* Wykrywanie komentarzy z użyciem warunków początkowych */
<INITIAL>"(*" { BEGIN(ST_COMMENT_1); comm_beg = yylineno; }
<INITIAL>"*)" { printf("Nieoczekiwane zamknięcie komentarza w linii %d\n", yylineno); }
<ST_COMMENT_1>"*)" { comm_beg = 0; BEGIN(INITIAL); }
<ST_COMMENT_1>.|\n ;

<INITIAL>"{" { BEGIN(ST_COMMENT_2); comm_beg = yylineno; }
<INITIAL>"}" { printf("Nieoczekiwane zamknięcie komentarza w linii %d\n", yylineno); }
<ST_COMMENT_2>"}" { comm_beg = 0; BEGIN(INITIAL); }
<ST_COMMENT_2>.|\n ;

 /* Wykrywanie słów kluczowych BEZ uwzględniania wielkości liter */
(?i:PROGRAM)  return process_token(yytext, "KW_PROGRAM", "", KW_PROGRAM);
(?i:BEGIN)    return process_token(yytext, "KW_BEGIN", "", KW_BEGIN);
(?i:END)      return process_token(yytext, "KW_END", "", KW_END);
(?i:USES)     return process_token(yytext, "KW_USES", "", KW_USES);
(?i:VAR)      return process_token(yytext, "KW_VAR", "", KW_VAR);
(?i:CONST)    return process_token(yytext, "KW_CONST", "", KW_CONST);
(?i:IF) 	    return process_token(yytext, "KW_IF", "", KW_IF);
(?i:THEN) 	  return process_token(yytext, "KW_THEN", "", KW_THEN);
(?i:ELSE) 	  return process_token(yytext, "KW_ELSE", "", KW_ELSE);
(?i:CHAR) 	  return process_token(yytext, "KW_CHAR", "", KW_CHAR);
(?i:INTEGER)  return process_token(yytext, "KW_INTEGER", "", KW_INTEGER);
(?i:REAL)     return process_token(yytext, "KW_REAL", "", KW_REAL);
(?i:FOR) 	    return process_token(yytext, "KW_FOR", "", KW_FOR);
(?i:TO) 	    return process_token(yytext, "KW_TO", "", KW_TO);
(?i:DO) 	    return process_token(yytext, "KW_DO", "", KW_DO);
(?i:DOWNTO)   return process_token(yytext, "KW_DOWNTO", "", KW_DOWNTO);
(?i:ARRAY) 	  return process_token(yytext, "KW_ARRAY", "", KW_ARRAY);
(?i:RECORD)   return process_token(yytext, "KW_RECORD", "", KW_RECORD);
(?i:OF) 	    return process_token(yytext, "KW_OF", "", KW_OF);

 /* Usuwanie białych znaków */
[[:space:]]  ;

 /* Wykrywanie stałych tekstowych bez użycia mechanizmu warunków początkowych
(\"[^\"]*\")|(\'[^\']*\') return process_token(yytext, "STRING_CONST", yytext, STRING_CONST); */
 /* Te backslashe nie są potrzebne, ale Daciuk o tym nie wie i jest uparty nawet jak mu pokażesz [^"] czy [^'] działa na luzie */

 /* Wykrywanie symboli końcowych jednoznakowych */
[[:punct:]]  return process_token(yytext, yytext, "", *yytext);

 /* Wykrywanie identyfikatorów */
[[:alpha:]_][[:alnum:]_]*  return process_token(yytext, "IDENT", yytext, IDENT);

 /* Wykrywanie liczb */
({d}+\.{d}+)([eE](\+|-)?{d}{1,2})?  return process_token(yytext, "FLOAT_CONST", "", FLOAT_CONST);
{d}+\./[^.]([eE](\+|-)?{d}{1,2})?   return process_token(yytext, "FLOAT_CONST", "", FLOAT_CONST); /* Wg daciuka to jest bullshit. Myli się, sprawdzone. Na oddawanie jednak trzeba wywalić tą linijkę*/
{d}+                                return process_token(yytext, "INTEGER_CONST", "", INTEGER_CONST);

 /* Wykrywanie operatorów wieloznakowych */
":="  return process_token(yytext, "ASSIGN", "", ASSIGN);
"<="  return process_token(yytext, "LE", "", LE);
".."  return process_token(yytext, "RANGE", "", RANGE);

 /* Wykrywanie dyrektywy dołączania plików */
\{\$I\ [A-Za-z_]+[[:alnum:]_]*.inc\}  printf("Przetwarzanie dyrektywy INCLUDE\n");


%%

/* Nazwa:	strnncpy
 * Cel:		Kopiuje zadaną liczbę znaków napisu umieszczając na końcu
 *		znak o kodzie 0.
 * Parametry:	dst		- (o) napis, do którego ma nastąpić kopiowanie;
 *		src		- (i) napis, który ma być kopiowany;
 *		n		- (i) maksymalna liczba znaków do kopiowania.
 * Zwraca:	dst.
 * Uwagi:	strncpy nie umieszcza znaku o kodzie 0 za kopiowanym napisem.
 *		Napis docelowy musi mieć miejsce na dodatkowy n+1 znak.
 */
char *
strnncpy(char *dst, const char *src, const int n)
{
  if (n > 0) {
    strncpy(dst, src, n);
  }
  dst[n] = '\0';
  return dst;
}/*strnncpy*/

/* Nazwa:       process_token
 * Cel:         Wypisanie informacji o wykrytym elemencie i przekazanie
 *              tej informacji wyżej z ewentualną wartością elementu, jeśli
 *              takowa występuje.
 * Parametry:   text            - (i) tekst, do którego nastąpiło dopasowanie;
 *              TokenType       - (i) napis będący tekstową reprezentacją
 *                                      nazwy typu elementu;
 *              TokenVal        - (i) wartość elementu, o ile występuje;
 *              TokenID         - (i) identyfikator typu elementu zadeklarowany
 *                                      za pomocą dyrektywy %token
 *                                      w pliku p.y lub kod pojedynczego
 *					znaku (w pliku analizatora składniowego
 *					występuje ujęty w apostrofy).
 * Zwraca:      Identyfikator typu elementu (TokenID).
 * Uwagi:       Informacja o wykrytym elemencie zwracana jest
 *              w trzech kolumnach. W pierwszej wypisywany jest tekst,
 *              do którego nastąpiło dopasowanie, w drugiej - typ elementu,
 *              w trzeciej - wartość elementu (o ile występuje).
 */
int process_token(const char *text, const char *TokenType,
                  const char *TokenVal, const int TokenID)
{
  int l;
  printf("%-20.20s%-15s %s\n", text, TokenType, TokenVal);
  switch (TokenID) {

  case INTEGER_CONST:
    yylval.i = atoi(TokenVal); break;

  case FLOAT_CONST:
    yylval.d = atof(TokenVal); break;

  case IDENT:
    strncpy(yylval.s, TokenVal, MAX_STR_LEN); break;

  case STRING_CONST:
    l = strlen(text);
    strnncpy(yylval.s, TokenVal+1, l - 2 <= MAX_STR_LEN ? l - 1 : MAX_STR_LEN);
    break;

  case CHARACTER_CONST:
    yylval.i = TokenVal[1]; break;

  }
  return(TokenID);
}/*process_token*/


int yywrap( void )
{ /* funkcja wywoływana po napotkaniu końca strumienia wejściowego */

	/* sprawdzenie, czy bieżący warunek początkowy automatu YY_START */
	/* różni się od warunku początkowego INITIAL. Jeżeli tak, */
	/* to oznacza to niezamknięty komentarz lub stałą tekstową - */
	/* wypisujemy informację o błędzie.*/
	/* ..................... */

  if ((YYSTATE == ST_COMMENT_1) || (YYSTATE == ST_COMMENT_2)) {
      printf("Brak zamknięcia komentarza otwartego w linii %d\n", comm_beg);
  }

	return( 1 ); /* koniecznie, by analiza nie rozpoczęła się od nowa */
}


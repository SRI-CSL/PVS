%{
/*
 DOCUMENTATION INFORMATION                                 module: MU CALCULUS
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S750
 file      : lex.l
 unit-title: Lexical Rules
 ref.      : 
 author(s) : Copyright (c) 1992-1997 G.L.J.M. Janssen
 date      :  1-MAY-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/


/*
****************************************
***** Declarations and definitions *****
****************************************
*/

#include  <stdio.h>
#include  <stdlib.h>

#include "alloc.h"
#include "bdd_fns.h"
#include "mu.h"
#include "y.tab.h"

#define DOMAIN_KW	DOMAIN_SYM
#define LET_KW		LET_SYM
#define MU_KW		MU_SYM
#define NU_KW		NU_SYM
#define WRITE_KW	WRITE_SYM
#define REACH_KW	REACH_SYM

#define NOT_KW		NOT_SYM
#define AND_KW		AND_SYM
#define OR_KW		OR_SYM
#define IMPLIES_KW	IMPLIES_SYM
#define IMPLIED_BY_KW	IMPLIED_BY_SYM
#define EQUIV_KW	EQUIV_SYM
#define XOR_KW		XOR_SYM

#define FALSE_KW	ZERO
#define TRUE_KW		POSINT

static void comment (void);
static void lexerr (char *mess);
static void read_string (void);
static char *ill_char_mess (int ch);
%}

/* space or tab */
BLANK		[ \t]
/* 1 or more spaces or tabs */
BLANKS 		({BLANK}+)
/* 1 or more spaces, tabs or newlines */
WS		([ \t\n\r]+)
/* a newline */
EOL		("\n")
/* Binary digits with don't care: */
BINX        	(0|1|x|X)
/* Binary digits: */
BIN        	(0|1)
/* Octal digits: */
OCT        	[0-7]
/* Decimal digits: */
DIG        	[0-9]
/* Hexadecimal digits: */
HEX        	[0-9a-fA-F]
/* Binary digit sequence with don't cares: */
BINXS        	({BINX}+)
/* Binary digit sequence: */
BINS        	({BIN}+)
/* Octal digit sequence: */
OCTS        	({OCT}+)
/* Decimal digit sequence: */
DIGS        	({DIG}+)
/* No-leading-zeroes decimal digit sequence: */
NLZDIGS        	([1-9]{DIG}*)
/* Hexadecimal digit sequence: */
HEXS        	({HEX}+)
/* a letter */
LET		[_A-Za-z$@]
/* not the first char in identifier: */
LETDIG		[_A-Za-z0-9$]
/* an identifier */
ID		({LET}{LETDIG}*(\[{DIGS}\](\[{DIGS}\])?)?)

/*
*****************
***** Start *****
*****************
*/

%%

%{
/* 1 and 2 letter special symbols (case-sensitive): */
%}

A					return A_SYM;
D					return D_SYM;
E					return E_SYM;
L					return L_SYM;
V					return OR_SYM;

%{
/* Reserved Words: */
%}

[mM][uU]				return MU_KW;
[nN][uU]				return NU_KW;
[oO][rR]				return OR_KW;
[aA][nN][dD]				return AND_KW;
[lL][eE][tT]				return LET_KW;
[nN][oO][tT]				return NOT_KW;
[xX][oO][rR]				return XOR_KW;
[eE][qQ][uU][iI][vV]			return EQUIV_KW;
[iI][mM][pP][lL][iI][eE][sS]		return IMPLIES_KW;
[iI][mM][pP][lL][iI][eE][dD][_][bB][yY]	return IMPLIED_BY_KW;
[tT][rR][uU][eE]			return TRUE_KW;
[fF][aA][lL][sS][eE]			return FALSE_KW;
[wW][rR][iI][tT][eE]			return WRITE_KW;
[dD][oO][mM][aA][iI][nN]		return DOMAIN_KW;

%{
/* Predefined Terms; For now, treat as reserved words: */
%}

"one_of"				return ONE_OF_PF;
"none_of"				return NONE_OF_PF;
"Reachable"				return REACH_PF;

^"#"						{
  int c;

  while (((c = input ()) != '\n') && (c > 0));
  /* Here: c == \n or c <= 0 */

  if (c <= 0) {
    lexerr ("Unexpected end-of-file in comment");
  }
  unput (c);
						}

"/*"						{
/* * start comment * */
	comment ();				}

{ID}						{
/* identifier */
	yylval.str = COPY_STRING_1 (yytext, yyleng);
	return IDENTIFIER;			}

"\""						{
/* start string */
        read_string ();
	/* Probably no need to make copy. */
/*	yylval.str = COPY_STRING_1 (yytext, yyleng);*/
	yylval.str = yytext;
	return STRING;				}

"0"						{
	yylval.ival = 0;
	return ZERO;				}

{NLZDIGS}					{
	yylval.ival = atol (yytext);
  	return POSINT;				}

%{
/* Operator symbols: */
%}

[~!]						{
/* invertor operator */
	return NOT_SYM;				}

"><"						{
/* logical operator XOR */
	return XOR_SYM;				}

"*"						{
/* logical operator XOR */
	return XOR_SYM;				}

"->"						{
/* logical operator IMPLIES */
	return IMPLIES_SYM;			}

"<-"						{
/* logical operator IMPLIED_BY */
	return IMPLIED_BY_SYM;			}

"<->"						{
/* logical operator EQUIV */
	return EQUIV_SYM;			}

"=="						{
/* logical operator EQUIV */
	return EQUIV_SYM;			}

"+"						{
/* logical operator OR */
	return OR_SYM;				}

[&^]						{
/* logical operator AND */
	return AND_SYM;				}

['|=]	return yytext[0];

%{
/* Brackets: */
%}
[(){}\[\]]	return yytext[0];

%{
/* Punctuation: */
%}
[:;,./?] 	return yytext[0];

":="						{
/* assign/becomes */
	return ASSIGN;				}

{WS}						{
/* unimportant white space or new_line */	}

.						{
	lexerr (ill_char_mess (yytchar));	}

%%

/*
********************************
***** End of lexical rules *****
********************************
*/

static char *nice_char_desc (int c, char *buf)
{
  register char *p;

  p = buf;

  if (c >= 0200) {		/* real Meta chars: M- */
    *p++ = 'M';
    *p++ = '-';
    c -= 0200;
  }

  if (c < 040) {
    if (c == 033) {
      *p++ = 'E';
      *p++ = 'S';
      *p++ = 'C';
    }
    else
    if (c == 011) {
      *p++ = 'T';
      *p++ = 'A';
      *p++ = 'B';
    }
    else
    if (c == 012) {
      *p++ = 'L';
      *p++ = 'F';
      *p++ = 'D';
    }
    else
    if (c == 015) {
      *p++ = 'R';
      *p++ = 'E';
      *p++ = 'T';
    }
    else {			/* other control chars: ^ */
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  }
  else
  if (c == 0177) {		/* DEL char: C-? */
    *p++ = 'D';
    *p++ = 'E';
    *p++ = 'L';
  }
  else
  if (c == ' ') {
    *p++ = 'S';
    *p++ = 'P';
    *p++ = 'C';
  }
  else
    *p++ = c;

  *p = '\0';

  return buf;  
}

static char *ill_char_mess (int ch)
{
  static char buf[80];
  char tem[6];

  strcpy  (buf, "illegal character: '");
  strcat  (buf, nice_char_desc (ch, tem));
  sprintf (buf + strlen (buf), "' (0x%x)", ch);
  strcat  (buf, ", skipped");
  return buf;
}

static void lexerr (char *mess)
{
  fprintf (stderr, "Lexical error in line %d: %s\n", yylineno, mess);
}

static void comment (void)
{
  int c = input ();

  do {
    while (c > 0 && c != '*') c = input ();

    if (c == '*' && (c = input()) == '/')
      break;

    if (c <= 0) {
      lexerr ("Unexpected end-of-file in comment");
      unput (c);
      break;
    }
  } while (1);
}

static int read_escape (void)
{
  int c = input ();    /* read the char following the escape character \ */

  switch (c) {
  case 'b':			/* backspace */
    return '\b';
  case 'e':			/* escape */
    return 033;
  case 'f':			/* form feed */
    return '\f';
  case 'n':			/* real newline */
    return '\n';
  case 'r':			/* return */
    return '\r';
  case 't':			/* tab */
    return '\t';
  case '\n':			/* escaped newline */
    return -1;

  case '0':			/* octal notation */
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    {
      /* First octal digit already read. */
      int i = c - '0';        /* most significant digit */
      int count = 1;          /* already read 1 digit, at most 3 digits */

      /* Read at most another 2 digits: */
      while (((c = input ()) >= '0') && (c <= '7') && ((++count) <= 3))	{
	i *= 8;
	i += c - '0';
      }
      /* always read 1 char ahead, so push it back: */
      unput (c);
      return i;
    }

  default:
    return c;
  }
}

/* Leaves result string without the bracketing " in yytext, yyleng. */
static void read_string (void)
{
  int c;
  char *p;

  /* Already seen opening " of string, *yytext == '"', yyleng == 1 */
  p = yytext;
  yyleng--;

  while (    (c = input ()) >= 0
	 && ((c != '\"') || ((c = input ()) == '\"'))) {
    /* To include a " in a string you may write it twice. */

    if (c == '\\')
      c = read_escape ();

    /* c is -1 if \ newline has just been seen. */
    if (c >= 0) {
      yyleng++;
      *p++ = c;
    }
  }

  /* Here: have read the closing " and the char immediately following. */
  unput (c);
  /* Note: did not put closing " in yytext. */

  /* Must insert trailing \0 though: */
  *p = '\0';
}

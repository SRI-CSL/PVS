%{
/*
 DOCUMENTATION INFORMATION                               module: BDD EXAMPLE
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Apollo DN3000, IBM RS/6000
 file      : lex.l
 unit-title: Lexical Rules
 ref.      : 
 author(s) : Copyright (c) 1990-1997 G.L.J.M. Janssen
 date      :  2-MAY-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/


/*
****************************************
***** Declarations and definitions *****
****************************************
*/

#include  <stdio.h>
#include "alloc.h"
#include "hash.h"
#include "bdd.h"
#include "y.tab.h"

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
/* Hexadecimal digit sequence: */
HEXS        	({HEX}+)
/* a letter, first char of identifier */
LET		[_A-Za-z$@]
/* not the first char in identifier: */
LETDIG		[_A-Za-z0-9$]
/* an identifier */
ID		((({LET}{LETDIG}*)|{DIG}{LETDIG}+)(\[{DIGS}\](\[{DIGS}\])?)?)

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
E!					return ONE_OF_SYM;
V					return OR_SYM;
X					return DONTCARE_SYM;

%{
/* Reserved Words: */
%}

[gG][cC]				return GC_SYM;
[oO][rR]				return OR_SYM;
[aA][nN][dD]				return AND_SYM;
[cC][uU][tT]				return CUT_SYM;
[cC]2[vV]				return C2V_SYM;
[iI][nN]"?"				return IN_SYM;
[iI][nN][vV]				return INV_INPUTS_SYM;
[iI][tT][eE]				return ITE_SYM;
[iI][tT][eE][cC]			return ITEC_SYM;
[lL][eE][tT]				return LET_SYM;
[mM][iI][nN]				return MINIMIZE_SYM;
[nN][oO][tT]				return NOT_SYM;
[tT][oO][pP]				return TOP_SYM;
[sS][uU][pP]				return SUPPORT_SYM;
[sS][eE][tT]				return SET_SYM;
[vV]2[cC]				return V2C_SYM;
[xX][oO][rR]				return XOR_SYM;
[eE][vV][aA][lL]			return EVAL_SYM;
[fF][rR][eE][eE]			return FREE_SYM;
[mM][iI][nN]_[X]			return MINIMIZE_X_SYM;
[fF][rR][eE][eE][zZ][eE]		return FREEZE_SYM;
[hH][aA][sS]_X"?"			return HAS_X_SYM;
[iI][nN][vV]1				return INV_INPUT_SYM;
[oO][nN][eE]_[oO][fF]			return ONE_OF_SYM;
[oO][nN]_[sS][eE][tT]			return ON_SET_SYM;
[oO][fF][fF]_[sS][eE][tT]		return OFF_SET_SYM;
[X]_[sS][eE][tT]			return X_SET_SYM;
[rR][eE][pP][lL][aA][cC][eE]_X		return REPLACE_X_SYM;
[rR][eE][pP][lL][aA][cC][eE]_[cC]	return REPLACE_C_SYM;
[iI][nN][tT][eE][rR][sS][eE][cC][tT]	return INTERSECT_SYM;
[cC][uU][bB][eE]"?"			return CUBE_SYM;
[sS][uU][bB][sS][tT]			return SUBST_SYM;
[sS][uU][bB][sS][tT]2			return SUBST2_SYM;
[eE][qQ][uU][iI][vV]			return EQUIV_SYM;
[iI][mM][pP][lL][iI][eE][sS]		return IMPLIES_SYM;
[iI][mM][pP][lL][iI][eE][dD]_[bB][yY]	return IMPLIED_BY_SYM;
[tT][rR][uU][eE]			return TRUE_SYM;
[fF][aA][lL][sS][eE]			return FALSE_SYM;
[wW][hH][iI][lL][eE]			return WHILE_SYM;
[uU][nN][aA][tT][eE]_[iI][nN]		return UNATE_IN_SYM;
[wW][rR][iI][tT][eE]			return WRITE_SYM;
[sS][uU][bB][sS][eE][tT]		return SUBSET_SYM;
[tT][rR][aA][nN][sS]			return TRANS_SYM;
[pP][aA][tT][hH]			return PATH_SYM;
[mM][tT][eE][rR][mM][sS]		return MTERMS_SYM;
[cC][oO][rR][eE]			return CORE_SYM;
[pP][rR][iI][mM][eE]			return PRIME_SYM;
reorder					return REORDER_SYM;

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
	/* Make copy of string to be sure yacc's lookahead
           will not clutter it.
	*/
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
/* logical truth value FALSE */
	return FALSE_SYM;			}

"1"						{
/* logical truth value TRUE */
	return TRUE_SYM;			}

%{
/* Operator symbols: */
%}

[~!]						{
/* invertor operator */
	return NOT_SYM;			}

"'"						{
/* invertor operator */
	return POSTFIX_NOT_SYM;		}

[&^]						{
/* logical operator AND */
	return AND_SYM;			}

"+"						{
/* logical operator OR */
	return OR_SYM;			}

"*"						{
/* logical operator XOR */
	return XOR_SYM;			}

"><"						{
/* logical operator XOR */
	return XOR_SYM;			}

"->"						{
/* logical operator IMPLIES */
	return IMPLIES_SYM;			}

"<-"						{
/* logical operator IMPLIED_BY */
	return IMPLIED_BY_SYM;		}

":-"						{
/* logical operator IMPLIED_BY */
	return IMPLIED_BY_SYM;		}

"<->"						{
/* logical operator EQUIV */
	return EQUIV_SYM;			}

"=="						{
/* logical operator EQUIV */
	return EQUIV_SYM;			}

[=?|]	return yytext[0];

%{
/* Brackets: */
%}

[()\[\]]	return yytext[0];

%{
/* Punctuation: */
%}

":="						{
/* assign/becomes */
	return ASSIGN;				}

[:;,./]	return yytext[0];

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
    while (c >= 0 && c != '*') c = input ();

    if (c == '*' && (c = input()) == '/')
      break;

    if (c < 0) {
      lexerr ("Unexpected end-of-file in comment");
      break;
    }
  } while (1);
}

static int read_escape (void)
{
  int c = input ();    /* read the char following the escape character \ */

  switch (c) {
  case 'a':			/*  */
    return '\a';
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
  p = yytext;			/* start putting chars here */
  yyleng = 0;			/* no char seen so far */

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

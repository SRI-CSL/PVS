/*
 * MONA
 * Copyright (C) 1997-2000 BRICS.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the  Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
 * USA.
 */

%a 40235 /* size of transitions accepted */
%o 6000  /* size of output slots accepted */

%option nounput
%option never-interactive

%{
#include <stdio.h>
#include <string.h>
#include "untyped.h"
#include "parser.h"
#include "deque.h"
#include "lib.h"
#include "printline.h"      
#include "env.h"
#include "../BDD/bdd.h"

extern SymbolTable symbolTable;
extern AutLib lib;
extern Options options;
extern YYLTYPE yylloc;    

void copyString(char*);
int get_next_char(char*);  
void loadFile(char *s);
void yyerror(const char *s);

#define MYBUFFER_SIZE 1024  

class FileInfo {
public:
  unsigned lineno; /* linenumber */
  unsigned pos; /* position in line number lineno */
  int prevnl; /* previous char was '\n' */
  char *filename; /* name of the file */
  unsigned number; /* number of the file in the include-chain */
  unsigned next; /* pointer into inputbuffer - where to read next char */
  unsigned bufferused; /* number of chars read into inputbuffer */
  char inputbuffer[MYBUFFER_SIZE]; /* buffer containing inputfile blocks */
  FILE *handle; /* file handle */
};

Deque<FileInfo> loadStack; /* stack of file info */

Deque<Deque<char *> *> dependencies; /* list of dependencies */
Deque<FileSource *> source; /* list of source-file text */ 
Deque<char *> fileNames; /* list of file names */
Deque<char> currentLine; /* contains the current line being parsed */

Deque<char *> currentDir;
char *file = "";
unsigned pos = 0;
int prevnl = 0;
int filenumber = 0;

char mybuffer[MYBUFFER_SIZE]; /* input buffer used in YY_INPUT */
int next = 0;
int bufferused = 0;

#define SETPOS(p) \
 yylloc.first_line = yylineno; yylloc.first_column = pos-yyleng-p+1

#define YY_INPUT(buf,result,max_size) result = get_next_char(buf)
%}

/* Definitions */

name		[a-zA-Z0-9_'@\$]+
whitespace	[ \t\n\f]+
comment		#.*
numeral		[0-9]+

/* Rules */

%x CCOMMENT

%%

"/*"		BEGIN(CCOMMENT);
<CCOMMENT>{
 [^*\n]*	/* ignore */
 "*"+[^*/\n]*	/* ignore */
 \n		/* ignore */
 <<EOF>>	yyerror("end-of-file in comment\n");
 "*"+"/"	BEGIN(INITIAL);
}

"%"             SETPOS(0); return tokMODULO;
"&"             SETPOS(0); return tokAND;
"("             SETPOS(0); return tokLPAREN;
")"             SETPOS(0); return tokRPAREN;
"*"             SETPOS(0); return tokSTAR;
"+"             SETPOS(0); return tokPLUS;
","             SETPOS(0); return tokCOMMA;
"-"             SETPOS(1); return tokMINUS;
"->"            SETPOS(0); return tokARROW;
"."             SETPOS(1); return tokDOT;
"..."           SETPOS(0); return tokINTERVAL;
"/"             SETPOS(1); return tokSLASH;
":"             SETPOS(0); return tokCOLON;
";"             SETPOS(0); return tokSEMICOLON;
"<"             SETPOS(1); return tokLESS;
"<="            SETPOS(1); return tokLESSEQ;
"<=>"           SETPOS(0); return tokBIIMPL;
"="             SETPOS(1); return tokEQUAL;
"=>"            SETPOS(0); return tokIMPL;
">"             SETPOS(1); return tokGREATER;
">="            SETPOS(0); return tokGREATEREQ;
"["             SETPOS(0); return tokLBRACKET;
"\\"            SETPOS(0); return tokSETMINUS;
"]"             SETPOS(0); return tokRBRACKET;
"^"             SETPOS(0); return tokUP;
"{"             SETPOS(0); return tokLBRACE;
"|"             SETPOS(0); return tokOR;
"}"             SETPOS(0); return tokRBRACE;
"~"             SETPOS(1); return tokNOT;
"~="            SETPOS(0); return tokNOTEQUAL;
"m2l-str"       SETPOS(0); return tokM2LSTR;
"m2l-tree"      SETPOS(0); return tokM2LTREE;
"empty"         SETPOS(1); return tokEMPTY;
"universe"      SETPOS(1); return tokUNIVERSE;
"union"         SETPOS(1); return tokUNION;
"inter"         SETPOS(1); return tokINTER;
"true"          SETPOS(1); return tokTRUE;
"false"         SETPOS(1); return tokFALSE;
"in"            SETPOS(1); return tokIN;
"notin"         SETPOS(1); return tokNOTIN;
"sub"           SETPOS(1); return tokSUB;
"all0"          SETPOS(1); return tokALL0;
"ex0"           SETPOS(1); return tokEX0;
"all1"          SETPOS(1); return tokALL1;
"ex1"           SETPOS(1); return tokEX1;
"all2"          SETPOS(1); return tokALL2;
"ex2"           SETPOS(1); return tokEX2;
"var0"          SETPOS(1); return tokVAR0;
"var1"          SETPOS(1); return tokVAR1;
"var2"          SETPOS(1); return tokVAR2;
"let0"          SETPOS(1); return tokLET0;
"let1"          SETPOS(1); return tokLET1;
"let2"          SETPOS(1); return tokLET2;
"guide"         SETPOS(1); return tokGUIDE;
"const"         SETPOS(1); return tokCONST;
"pred"          SETPOS(1); return tokPRED;
"macro"         SETPOS(1); return tokMACRO;
"assert"        SETPOS(1); return tokASSERT;
"root"          SETPOS(1); return tokUNIVROOT;
"tree"          SETPOS(1); return tokTREE;
"ws1s"          SETPOS(1); return tokWS1S;
"ws2s"          SETPOS(1); return tokWS2S;
"min"           SETPOS(1); return tokMIN;
"max"           SETPOS(1); return tokMAX;
"where"         SETPOS(1); return tokWHERE;
"defaultwhere1" SETPOS(1); return tokDEFAULT1;
"defaultwhere2" SETPOS(1); return tokDEFAULT2;
"include"       SETPOS(1); return tokINCLUDE;
"import"        SETPOS(1); return tokIMPORT;
"export"        SETPOS(1); return tokEXPORT;
"prefix"        SETPOS(1); return tokPREFIX;
"in_state_space" SETPOS(1); return tokINSTATESPACE;
"execute"       SETPOS(1); return tokEXECUTE;
"lastpos"       SETPOS(1); return tokLASTPOS;
"allpos"        SETPOS(1); return tokALLPOS;
"type"          SETPOS(1); return tokTYPE;
"sometype"      SETPOS(1); return tokSOMETYPE;
"variant"       SETPOS(1); return tokVARIANT;
"succ"          SETPOS(1); return tokSUCC;
"const_tree"    SETPOS(1); return tokCONSTTREE;
"tree_root"     SETPOS(1); return tokTREEROOT;
"restrict"      SETPOS(1); return tokRESTRICT;
"verify"        SETPOS(1); return tokVERIFY;
\"[^\"\n]*\"    {SETPOS(1); yytext[strlen(yytext)-1] = 0;  
		 copyString(yytext); return tokSTRING;}
{numeral}       SETPOS(1); copyString(yytext); return tokINT;
{name}          SETPOS(1); copyString(yytext); return tokNAME;
{whitespace}    /* ignore */
{comment}       /* ignore */
.               yyerror("illegal character");

%%     

/* Subroutines */

void copyString(char *s)
{
  char *str = new char[strlen(s)+1];
  strcpy(str, s);
  str = symbolTable.insertString(str);
  yylval.string = str;
}

void yyerror(const char *s)
{ 
  cout << "Error in file '" << file
       << "' near line " << yylineno << ": " << s << "\n"
       << "Execution aborted\n";
  exit(-1);
}

int yywrap() {
  unsigned i;

  /* pop current directory */
  delete[] currentDir.pop_back();

  /* dependency info */
  if (options.separateCompilation) {
    Deque<char *> *d = dependencies.pop_back();
    lib.openDir(file, d);
    delete d; 
  }

  /* close file */
  fclose(yyin);

  /* done? */
  if (loadStack.size() == 0)
    return -1;

  /* return to previous file */
  FileInfo f = loadStack.pop_back();
  yylineno = f.lineno;
  pos = f.pos;
  prevnl = f.prevnl;
  file = f.filename;
  filenumber = f.number;
  next = f.next;
  bufferused = f.bufferused;
  strncpy(mybuffer, f.inputbuffer, MYBUFFER_SIZE);
  yyin = f.handle;
  
  /* get currentline contents */
  currentLine.reset(); 
  for (i = 0; i < strlen(source.get(filenumber-1)->top()); i++)
    currentLine.push_back((source.get(filenumber-1)->top())[i]);
  delete[] source.get(filenumber-1)->pop_back();

  return 0;
}

void loadFile(char *s) {
  unsigned i,n;
  char *line;
  char *ss;

  /* find current directory */
  if (!currentDir.empty() && s[0] != '/') {
    ss = new char[strlen(currentDir.top())+strlen(s)+1];
    strcpy(ss, currentDir.top()); 
  }
  else {
    ss = new char[strlen(s)+1];
    ss[0] = 0;
  }
  strcat(ss, s);
  unsigned t = strlen(ss);
  while (t > 0 && ss[t-1] != '/')
    t--;
  char *cd = new char[t+1];
  strncpy(cd, ss, t);
  cd[t] = 0;
  currentDir.push_back(cd);

  if (filenumber > 0) {
    /* save status for current file on stack */
    FileInfo f;
    f.lineno = yylineno;
    f.pos = pos;
    f.prevnl = prevnl;
    f.filename = file;
    f.number = filenumber;
    f.next = next;
    f.bufferused = bufferused;
    strncpy(f.inputbuffer, mybuffer, MYBUFFER_SIZE);
    f.handle = yyin;
    loadStack.push_back(f);
    
    /* store current line source */
    line = new char[sizeof(char)*currentLine.size()+1]; 
    line[currentLine.size()] = '\0';
    for (n = 0; n < currentLine.size(); n++) 
      line[n] = currentLine.get(n);
    source.get(filenumber-1)->push_back(line);      
  }

  /* store info on new file in list */
  file = new char[strlen(ss)+1];
  strcpy(file, ss);
  delete[] ss;
  file = symbolTable.insertString(file);
  fileNames.push_back(file);
  source.push_back(new FileSource);

  /* open file */
  for (i = 0; i+1 < loadStack.size(); i++)
    if (strcmp(loadStack.get(i).filename, file) == 0)
      yyerror("cyclic include");
  if (!(yyin = fopen(file, "r"))) {
    cout << "Unable to open file '" << file << "'\n"
	 << "Execution aborted\n";
    exit(-1);
  }
  currentLine.reset(); 
  filenumber++;
  pos = 0;
  prevnl = 0;
  yylineno = 1;
  next = bufferused = 0;

  /* find dependencies */
  if (options.separateCompilation) {
    dependencies.push_back(new Deque<char *>);
    for (Deque<Deque<char *> *>::iterator d = dependencies.begin();
	 d != dependencies.end(); d++)
      (*d)->push_back(file);
  }
}

int get_next_char(char *buf) {
  char *line; 
  unsigned n;
  int c;
  
  if (next == bufferused) {
    /* get a new chunk from the file */
    bufferused = fread(mybuffer, 1, MYBUFFER_SIZE, yyin);
    next = 0;
  }
  if (next == bufferused || mybuffer[next] == '\n') {
    /* move to next line */
    line = new char[currentLine.size()+1]; 
    line[currentLine.size()] = '\0'; 
    for (n = 0; n < currentLine.size(); n++) 
      line[n] = currentLine.get(n); 
    source.get(filenumber-1)->push_back(line);      
    currentLine.reset(); 
  }
  if (next < bufferused) {
    /* end-of-file not reached yet */
    c = mybuffer[next++]; 
    if (prevnl) {
      prevnl = 0;
      pos = 0;
    }
    pos++; 
    if (c == '\n') 
      prevnl = 1; 
    else
      currentLine.push_back(c); 
    buf[0] = c;
    return 1;
  }
  else {
    pos++; 
    return YY_NULL; 
  }
}

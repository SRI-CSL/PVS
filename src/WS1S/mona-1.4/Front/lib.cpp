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

#include <fstream.h>
#include <sys/stat.h>
#include <time.h>
#include <string.h>
#include "lib.h"
#include "symboltable.h"
#include "env.h"
#include "../GTA/gta.h"

extern SymbolTable symbolTable;
extern Options options;
extern Guide guide;

int 
AutLib::Dir::compare(AutLib::Dir::File *a, AutLib::Dir::File *b)
{
  if (a->hashvalue < b->hashvalue)
    return -1;
  else if (a->hashvalue > b->hashvalue)
    return 1;

  return strcmp(a->descriptor, b->descriptor);
}

// AUTOMATON FILE

AutLib::Dir::File::File(char *name, Signature *sign, Deque<SSSet> *statespaces)
{
  char *x = new char[10000];
  char tmp[1000];
  unsigned len;

  // add name and signature to descriptor
  sprintf(x, "%s^", name);
  sign->dump(x+strlen(x));
  len = strlen(x);

  if (options.mode == TREE) {
    // add guide
    sprintf(tmp, "^%i", guide.numSs);
    strcpy(x+len, tmp);
    len += strlen(tmp);
    for (unsigned i = 0; i < guide.numSs; i++) {
      sprintf(tmp, "_%i_%i", guide.muLeft[i], guide.muRight[i]);
      strcpy(x+len, tmp);
      len += strlen(tmp);
    }

    // add universes
    sprintf(tmp, "^%i", guide.numUnivs);
    strcpy(x+len, tmp);
    len += strlen(tmp);
    for (unsigned i = 0; i < guide.numUnivs; i++) {
      sprintf(tmp, "_%s_%i", guide.univPos[i], guide.numUnivSS[i]);
      strcpy(x+len, tmp);
      len += strlen(tmp);
      for (unsigned j = 0; j < guide.numUnivSS[i]; j++) {
	sprintf(tmp, "_%i", guide.univSS[i][j]);
      strcpy(x+len, tmp);
      len += strlen(tmp);
      }
    }

    // add state spaces
    for (Deque<SSSet>::iterator i = statespaces->begin();
	 i != statespaces->end(); i++) {
      sprintf(x+(len++), "^");
      for (unsigned j = 0; j < guide.numSs; j++)
	if ((*i)[j]) {
	  sprintf(tmp, "_%d", j);
	  strcpy(x+len, tmp);
	  len += strlen(tmp);
	}
    }
  }
  
  // make hash value
  hashvalue = 0;
  char *t = x;
  while (*t)
    hashvalue = (hashvalue << 1) + *t++;

  descriptor = new char[len+1];
  strcpy(descriptor, x);

  delete[] x;
  filenumber = 0; // set later
}

AutLib::Dir::File::File(istream &s)
{
  char x[1000];
  s >> x >> filenumber;
  hashvalue = 0;
  char *t = x;
  while (*t)
    hashvalue = (hashvalue << 1) + *t++;
  descriptor = new char[strlen(x)+1];
  strcpy(descriptor, x);
}

void
AutLib::Dir::File::store(ostream &s)
{
  s << descriptor << " " << filenumber << "\n";
}

AutLib::Dir::File::~File()
{
  delete[] descriptor;
}

// DIRECTORY

AutLib::Dir::Dir(char *name, char *src, Deque<char*> *dependencies)
{
  dirname = name;
  sourcename = src;
  libname = new char[strlen(dirname)+5];
  sprintf(libname, "%s/LIB", dirname);

  // make sure directory is created
  struct stat buf;
  if (stat(dirname, &buf))
    if (mkdir(dirname, S_IWRITE | S_IREAD | S_IEXEC)) {
      cout << "Unable to create directory '" << dirname << "'\n"
	   << "Execution aborted\n";
      exit(-1);
    }

  // if src newer than LIB then remove all files
  if (stat(libname, &buf) == 0) {
    struct stat buf2;
    for (Deque<char *>::iterator i = dependencies->begin();
	 i != dependencies->end(); i++) {
      stat(*i, &buf2);
      if (difftime(buf.st_mtime, buf2.st_mtime) <= 0) {
	char t[500];
	sprintf(t, "/bin/rm %s/*%s %s/LIB", 
		dirname, 
		(options.mode == TREE) ? ".gta" : ".dfa",
		dirname);
	system(t);
	break;
      }
    }
  }

  // read LIB file
  ifstream s(libname);
  if (s) {
    unsigned n;
    s >> nextFilenumber;
    s >> n;
    while (n-- && !s.eof())
      files.push_back(new File(s));
  }
  else
    nextFilenumber = 1;
}

char *
AutLib::Dir::getFileName(char *name, Signature *sign, Deque<SSSet> *statespaces)
{
  // find/create filename + number
  File *a = new File(name, sign, statespaces);
  Deque<File *>::iterator b;
  for (b = files.begin(); b != files.end(); b++)
    if (compare(a, *b) == 0) {
      delete a;
      a = *b;
      break;
    }
  if (b == files.end()) {
    files.push_back(a);
    a->filenumber = nextFilenumber++;
  }
  char *t = new char[strlen(dirname)+20];
  sprintf(t, "%s/%i%s", dirname, a->filenumber, 
	  (options.mode == TREE) ? ".gta" : ".dfa");
  return t;
}

AutLib::Dir::~Dir()
{
  // store to $MONALIB/source/LIB
  ofstream s(libname);
  s << nextFilenumber << "\n"
    << files.size() << "\n";
  Deque<File *>::iterator i;
  for (i = files.begin(); i != files.end(); i++) {
    (*i)->store(s);
    delete *i;
  }
  delete[] dirname;
  delete[] libname;
}

// AUTOMATON LIBRARY

AutLib::AutLib()
{
  char *s = getenv("MONALIB");
  if (s) {
    monalib = new char[strlen(s)+2];
    strcpy(monalib, s);
  }
  else {
    monalib = new char[3];
    strcpy(monalib, ".");
  }
  strcat(monalib, "/");
}

AutLib::~AutLib()
{
  // store changes + clean up
  Deque<Dir *>::iterator i;
  for (i = dirs.begin(); i != dirs.end(); i++)
    delete *i;
  delete[] monalib;
}

void 
AutLib::openDir(char *src, Deque<char *> *dependencies)
{
  // read <$MONALIB>/<src>.lib/LIB if not already done
  Deque<Dir *>::iterator i;
  for (i = dirs.begin(); i != dirs.end(); i++)
    if ((*i)->sourcename == src)
      return;

  char *s = src + strlen(src);
  while (s > src && *(s-1) != '/')
    s--;
  char *d = new char[strlen(s)+strlen(monalib)+1];
  strcpy(d, monalib);
  strcat(d, s);
  if (strlen(s) > 5 && strcmp(s+strlen(s)-5, ".mona") == 0)
    d[strlen(d)-5] = 0;
  strcat(d, ".lib");
  dirs.push_back(new Dir(d, src, dependencies));
}

char *
AutLib::getFileName(char *name, char *origin, Signature *sign, 
		    Deque<SSSet> *statespaces)
{
  // get or make filenumber + create filename
  Deque<Dir *>::iterator i;
  for (i = dirs.begin(); i != dirs.end(); i++)
    if ((*i)->sourcename == origin)
      return (*i)->getFileName(name, sign, statespaces);
  invariant(false);
  return 0;
}

bool 
AutLib::fileExists(char *filename)
{
  // check file exists
  ifstream s(filename);
  return s != 0;
}

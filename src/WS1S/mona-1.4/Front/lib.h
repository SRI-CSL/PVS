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

#ifndef __LIB_H
#define __LIB_H

#include <iostream.h>
#include "ident.h"
#include "signature.h"
#include "symboltable.h"
#include "deque.h"

class AutLib {

  class Dir {
  public:
    
    class File {
    public:
      char *descriptor;    // unique identifier (predname+sign.+guide+univs.)
      unsigned hashvalue;  // hashed descriptor
      unsigned filenumber; // automaton file number
      
      File(char *name, Signature *sign, Deque<SSSet> *statespaces);
      File(istream &s);
      ~File();

      void store(ostream &s);
    };
    
    char *sourcename;        // source file name
    char *dirname;           // $MONALIB/source.lib
    char *libname;           // dirname/LIB
    unsigned nextFilenumber; // next fresh filenumber 
    Deque<File *> files;
    
    Dir(char *name, char *src, Deque<char *> *dependencies);
    ~Dir(); 

    int compare(AutLib::Dir::File *a, AutLib::Dir::File *b);
    char *getFileName(char *name, Signature *sign, Deque<SSSet> *statespaces);
    void remove(unsigned filenumber);
  };

public:
  char *monalib; // value of $MONALIB environment variable
  Deque<Dir *> dirs; 

  AutLib();
  ~AutLib();

  void openDir(char *src, Deque<char *> *dependencies);
  char *getFileName(char *name, char *origin, Signature *sign, 
		    Deque<SSSet> *statespaces); 
  bool fileExists(char *filename);
};

#endif

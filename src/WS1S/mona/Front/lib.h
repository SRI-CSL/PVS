//
// lib.h - handles LIB files used for separate compilation
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

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

    char *getFileName(char *name, Signature *sign, Deque<SSSet> *statespaces);
    void remove(unsigned filenumber);
  };

  char *monalib; // value of $MONALIB environment variable
  Deque<Dir *> dirs; 

public:
  AutLib();
  ~AutLib();

  void openDir(char *src, Deque<char *> *dependencies);
  char *getFileName(char *name, char *origin, Signature *sign, 
		    Deque<SSSet> *statespaces); 
  bool fileExists(char *filename);
};

#endif

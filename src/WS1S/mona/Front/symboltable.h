//
// symboltable.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __SYMBOLTABLE_H
#define __SYMBOLTABLE_H

#include "deque.h"
#include "ast.h"
#include "printline.h"
#include "ident.h"

class Name {
public:
  Name() {} // dummy
  Name(char *s, Pos p) :
    str(s), pos(p) {}
  Name(const Name& n) :
    str(n.str), pos(n.pos) {}

  char *str;
  Pos pos;
};

class NameList: public DequeGC<Name*> {};

enum MonaTypeTag { 
  Varname0, Varname1, Varname2,
  Parname0, Parname1, Parname2, ParnameU,
  Univname, Predname, Constname, Statespacename
};

class SymbolTable {

  class Entry {
  public:
    Entry() {} // dummy
    Entry(char *s, MonaTypeTag t, Ident i) :
      string(s), monaTypeTag(t), ident(i) {}
    virtual ~Entry() {};

    char       *string;
    MonaTypeTag monaTypeTag;
    Ident       ident;
  };

  class VarEntry: public Entry {
  public:
    VarEntry(char *s, MonaTypeTag t, Ident i, IdentList *u, bool f) :
      Entry(s, t, i), universes(u), restriction(NULL), fresh(f)  {}
    ~VarEntry() 
    {delete universes; delete restriction;}

    IdentList *universes; // always sorted set!
    ASTForm   *restriction;
    bool       fresh; // (only non-fresh can get default restriction)
  };

  class ConstEntry: public Entry {
  public:
    ConstEntry(char *s, Ident i, int v) :
      Entry(s, Constname, i), value(v) {}

    int value;
  };

  class PredEntry: public Entry {
  public:
    PredEntry(char *s, Ident i) :
      Entry(s, Predname, i) {}
  };

  class StateSpaceEntry: public Entry {
  public:
    StateSpaceEntry(char *s, Ident i, int v) :
      Entry(s, Statespacename, i), number(v) {}

    int number;
  };

  class UnivEntry: public Entry {
  public:
    UnivEntry(char *s, Ident i, int n, char *p) :
      Entry(s, Univname, i), number(n), pos(p) {}

    int   number;
    char *pos;
  };

  Ident  insert(Entry*);
  void   remove(int idx); // must be last-in-first-out order
  Entry &lookup(Name*);
  void   check(Name*);
  int    hash(char*);

  Deque<char*>  *symbols;          // hashtable of symbols
  Deque<Entry*> *declarationTable; // hashtable String->Entry
  Deque<int>     localStack;       // stack of hashtable indexes (-1 sentinel) 
  Deque<Entry*>  identMap;         // map Ident->Entry
  IdentList      allUnivIds;       // all universe Idents, sorted
  IdentList      allRealUnivIds;   // allUnivIds except dummy
  unsigned       size;             // hashtable array size

public:
   SymbolTable(int size);
  ~SymbolTable();

  char       *insertString(char *);

  Ident       insertPred(Name*);
  Ident       insertVar(Name*, MonaTypeTag, IdentList *univs, 
			bool local = false, bool fresh = false);
  Ident       insertUniv(Name*, char *pos, bool dummy = false);
  Ident       insertConst(Name*, int value);
  Ident       insertStatespace(Name*, int number);

  Ident       insertFresh(MonaTypeTag t, IdentList *univs = NULL);

  void        openLocal();
  void        closeLocal();
  
  Ident            lookupIdent(Name*);
  char            *lookupSymbol(Ident);
  MonaTypeTag      lookupType(Ident);
  MonaTypeTag      lookupType(Name*);
  int              lookupOrder(Ident);
  int              lookupValue(Name*);
  int              lookupNumber(Name*);
  int              lookupNumber(Ident ident);
  int              lookupDepth(Ident);
  IdentList       *lookupUnivIdents(NameList&);
  IdentList       *lookupUnivs(Ident);
  ASTForm         *lookupRestriction(Ident);
  int              lookupUnivNumber(Ident);
  char            *lookupPos(Ident);
  ASTForm         *getRestriction(Ident, Ident *formal);
  ASTForm         *getDefault1Restriction(Ident *formal);
  ASTForm         *getDefault2Restriction(Ident *formal);

  IdentList  *allUnivs();
  IdentList  *allRealUnivs();
  void        updateUnivs(Ident, IdentList *univs);
  void        updateRestriction(Ident, ASTForm *);
  void        updateStateSpaces(Ident, Deque<unsigned> *statespaces);

  void        checkUniqueness(NameList&);

  void        setDefaultRestriction(MonaTypeTag, ASTForm *, Ident);

  void        dump(); // dump contents

  unsigned  noIdents;       // total number of identifiers
  Ident     defaultIdent1;  // formal parameter for defaultwhere1
  Ident     defaultIdent2;  // formal parameter for defaultwhere2
  ASTForm  *defaultRestriction1; // default restriction for first order variables
  ASTForm  *defaultRestriction2; // default restriction for second order variables
};

#endif

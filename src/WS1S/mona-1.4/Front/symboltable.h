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
  Varname0, Varname1, Varname2, VarnameTree,
  Parname0, Parname1, Parname2, ParnameU,
  Univname, Predname, Constname, Statespacename,
  Typename
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
      Entry(s, t, i), universes(u), restriction(NULL), implicit(f) {}
    ~VarEntry() 
    {delete universes; delete restriction;}

    IdentList *universes; // always sorted set!
    ASTForm   *restriction;
    bool       implicit; // (only explicit variables can get default restriction)
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
      Entry(s, Statespacename, i), number(v), type(-1) {}

    int number;
    Ident type; // -1 if not a type root
  };

  class UnivEntry: public Entry {
  public:
    UnivEntry(char *s, Ident i, int n, char *p) :
      Entry(s, Univname, i), number(n), pos(p) {}
    UnivEntry(char *s, Ident i, int n, Ident t) :
      Entry(s, Univname, i), number(n), pos(0), type(t) {}

    int   number;
    char *pos;
    Ident type; /* only applicable if using types */
  };

  class TypeEntry: public Entry {
  public:
    TypeEntry(char *s, Ident i) :
      Entry(s, Typename, i), variants(0), number(-1), reached(false) {}
    ~TypeEntry() {delete variants;}

    ASTVariantList *variants;
    int number; 
    bool reached;
    IdentList statespaces;
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
  IdentList      statespaceIds;    // all statespace IDs in order
  unsigned       size;             // hashtable array size

public:
   SymbolTable(int size);
  ~SymbolTable();

  char  *insertString(char *);

  Ident  insertPred(Name*);
  Ident  insertVar(Name*, MonaTypeTag, IdentList *univs, 
		   bool local = false, bool implicit = false);
  Ident  insertUniv(Name*, char *pos, bool dummy = false);
  Ident  insertUniv(Name*, Ident type);
  Ident  insertConst(Name*, int value);
  Ident  insertStatespace(Name*);
  Ident  insertType(Name*);
  void   setTypeVariants(Ident, ASTVariantList*);
  void   setTypeNumber(Ident, int);
  void   setTypeReachable(Ident);
  void   setSSType(Ident, Ident);

  Ident  insertFresh(MonaTypeTag t, IdentList *univs = NULL, bool implicit = true);

  void   openLocal();
  void   closeLocal();
  
  Ident           lookupIdent(Name*);
  char           *lookupSymbol(Ident);
  MonaTypeTag     lookupType(Ident);
  MonaTypeTag     lookupType(Name*);
  Ident           lookupStatespaceId(int number);
  int             lookupOrder(Ident);
  int             lookupValue(Name*);
  int             lookupNumber(Name*);
  int             lookupNumber(Ident ident);
  int             lookupDepth(Ident);
  IdentList      *lookupUnivIdents(NameList&);
  IdentList      *lookupUnivs(Ident);
  ASTForm        *lookupRestriction(Ident);
  bool            lookupImplicit(Ident);
  int             lookupUnivNumber(Ident);
  char           *lookupPos(Ident);
  Ident           lookupUnivType(Ident);
  ASTVariantList *lookupTypeVariants(Ident);
  int             lookupTypeNumber(Ident);
  bool            lookupTypeReachable(Ident);
  IdentList      *lookupTypeStatespaces(Ident);
  Ident           lookupSSType(Ident);
  bool            exists(char*);
  ASTForm        *getRestriction(Ident, Ident *formal);
  ASTForm        *getDefault1Restriction(Ident *formal);
  ASTForm        *getDefault2Restriction(Ident *formal);

  IdentList  *allUnivs();
  IdentList  *allRealUnivs();
  void        updateRestriction(Ident, ASTForm *);
  void        updateStateSpaces(Ident, Deque<unsigned> *statespaces);
  void        updateUnivPos(Ident, char *pos);
  void        addTypeStatespace(Ident, char *ssname);

  void        checkUniqueness(NameList&);

  void        setDefaultRestriction(MonaTypeTag, ASTForm *, Ident);

  void        dump(); // dump contents

  unsigned  noIdents;       // total number of identifiers
  int       noSS;           // number of state spaces
  Ident     defaultIdent1;  // formal parameter for defaultwhere1
  Ident     defaultIdent2;  // formal parameter for defaultwhere2
  ASTForm  *defaultRestriction1; // default restriction for first order variables
  ASTForm  *defaultRestriction2; // default restriction for second order variables
};

#endif

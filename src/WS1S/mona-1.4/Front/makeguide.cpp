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

#include "untyped.h"

extern "C" {
#include "../Mem/mem.h"
}

extern SymbolTable symbolTable;
extern int numTypes;

Guide_Declaration *guide_declaration = 0;

// GTA GUIDE CONSTRUCTION

void
MonaUntypedAST::makeGTAGuide()
{
  IdentList *univs = symbolTable.allUnivs();

  if (numTypes > 0) { 
    // convert types to guide
    if (!univs || univs->empty()) {
      cout << "Error: Types declared but no universes (or trees)\n"
	   << "Execution aborted\n";
      exit(-1);
    }
    if (univs->size() == 1) {
      // boolean and universe state space must be different
      Name name = Name("<dummy>", dummyPos);
      symbolTable.insertUniv(&name, NULL, true); 
      delete univs;
      univs = symbolTable.allUnivs();
    }
    guide_declaration = new Guide_Declaration(new GuideFuncList, dummyPos);
    typedUnivs2guide(univs->size(), univs, 0, "");
  }

  if (guide_declaration) { // guide is declared
    if (!univs || univs->empty()) {
      cout << "Error: Guide declared but no universes\n"
	   << "Execution aborted\n";
      exit(-1);
    }

    // fill tables
    unsigned numUnivs = univs->size();
    char **univPos = (char **) mem_alloc(sizeof(char *)*numUnivs);
    char **univName = (char **) mem_alloc(sizeof(char *)*numUnivs);
    IdentList::iterator id;
    int i;
    for (id = univs->begin(), i = 0; id != univs->end(); id++, i++) {
      univName[i] = symbolTable.lookupSymbol(*id);
      univPos[i] = symbolTable.lookupPos(*id);
    }
    unsigned numSs = guide_declaration->funcList->size();
    SsId *muLeft = (SsId *) mem_alloc(sizeof(SsId)*numSs);
    SsId *muRight = (SsId *) mem_alloc(sizeof(SsId)*numSs);
    char **ssName = (char **) mem_alloc(sizeof(char *)*numSs);
    SsKind *ssKind = 0;
    int *ssType = 0;
    if (numTypes > 0) {
      ssKind = (SsKind *) mem_alloc(sizeof(SsKind)*numSs);
      ssType = (int *) mem_alloc(sizeof(int)*numSs);
    }
    GuideFuncList::iterator g;
    for (g = guide_declaration->funcList->begin(), i = 0;
	 g != guide_declaration->funcList->end(); g++, i++) {
      muLeft[i] = symbolTable.lookupNumber((*g)->name2);
      muRight[i] = symbolTable.lookupNumber((*g)->name3);
      ssName[i] = (*g)->name1->str;
      if (numTypes > 0) {
	switch((*g)->kind) {
	case SS_UNIVHAT:
	  ssKind[i] = gtaSSUNIVHAT;
	  break;
	case SS_ORHAT: 
	  ssKind[i] = gtaSSORHAT;
	  break;
	case SS_ORLEAF: 
	  ssKind[i] = gtaSSORLEAF;
	  break;
	case SS_AND: 
	  ssKind[i] = gtaSSAND;
	  break;
	case SS_DUMMY:
	  ssKind[i] = gtaSSDUMMY;
	  break;
	default: ;
	}
	Ident sstype = 
	  symbolTable.lookupSSType(symbolTable.lookupIdent((*g)->name1));
	if (sstype != -1) {
	  char *sstypename = symbolTable.lookupSymbol(sstype);
	  int j;
	  for (j = 0; j < numTypes; j++)
	    if (treetypes[j].name == sstypename) {
	      ssType[i] = j;
	      break;
	    }
	  invariant(j < numTypes);
	}
	else
	  ssType[i] = -1;
      }
    }
    
    makeGuide(numSs, muLeft, muRight, ssName, numUnivs, 
	      univPos, univName, ssType, ssKind);
    
    if (!checkDisjoint()) {
      cout << "Illegal guide and universe declarations:\n"
	   << "Universes must have disjoint state spaces\n"
	   << "Execution aborted\n";
      exit(-1);
    }
    if (!checkAllCovered()) {
      cout << "Illegal guide and universe declarations:\n"
	   << "Every infinite path in the guide must belong to a universe\n"
	   << "Execution aborted\n";
      exit(-1);
    }
    if (!checkAllUsed()) {
      cout << "Illegal guide and universe declarations:\n"
	   << "All state spaces must be reachable\n"
	   << "Execution aborted\n";
      exit(-1);
    }
  }

  else { // no guide declared, make default
    if (!univs || univs->empty()) {
      // make one universe
      Name name = Name("<univ>", dummyPos);
      symbolTable.insertUniv(&name, (char *) NULL); 
      delete univs;
      univs = symbolTable.allUnivs();
    }
      
    if (univs->size() == 1) {
      // boolean and universe state space must be different
      Name name = Name("<dummy>", dummyPos);
      symbolTable.insertUniv(&name, NULL, true); 
      delete univs;
      univs = symbolTable.allUnivs();
    }
      
    // fill name table
    unsigned numUnivs = univs->size();
    char **univName = (char **) mem_alloc(sizeof(char *)*numUnivs);
    Ident *id;
    int u;
    for (id = univs->begin(), u = 0; id != univs->end(); id++, u++)
      univName[u] = symbolTable.lookupSymbol(*id);
      
    makeDefaultGuide(numUnivs, univName);
  }
  delete univs;
}

//  RECURSIVE TYPES -> GUIDE

char*
MonaUntypedAST::typedUnivs2guide(unsigned num, IdentList *univs, unsigned idx, 
				 char *pos)
{
  invariant(num>0);
  char *ssname;
  if (num > 1) { // branch
    ssname = new char[strlen(pos)+4];
    sprintf(ssname, "ss-%s", pos); // ss-01101
    ssname = symbolTable.insertString(ssname);

    char *leftpos, *rightpos;
    GuideFunc *g = makeStateSpace(ssname, pos, &leftpos, &rightpos, 
				  SS_UNIVHAT);
    g->name2 = new Name(typedUnivs2guide((num+1)/2, univs, idx, 
					 leftpos), 
			dummyPos);
    g->name3 = new Name(typedUnivs2guide(num/2, univs, idx+(num+1)/2, 
					 rightpos), 
			dummyPos);
  }
  else { // make type
    Ident univId = univs->get(idx);
    char *univ = symbolTable.lookupSymbol(univId);
    symbolTable.updateUnivPos(univId, pos);
    if (strcmp(univ, "<dummy>") == 0)
      ssname = makeDummySS(univ);
    else {
      Ident typeId = symbolTable.lookupUnivType(univId);
      ASTVariantList *variants = symbolTable.lookupTypeVariants(typeId);
      IdentList typeSet(typeId);
      ssname = variants2guide(variants->size(), variants, 0, &typeSet, typeId,
			      univ, symbolTable.lookupSymbol(typeId), "");
      symbolTable.addTypeStatespace(typeId, ssname);
      Name n(ssname, dummyPos);
      symbolTable.setSSType(symbolTable.lookupIdent(&n), typeId);
    }
  }
  return ssname;
}

char*
MonaUntypedAST::variants2guide(unsigned num, ASTVariantList *variants, 
			       unsigned idx, IdentList *typeSet,
			       Ident typeId,
			       char *univ, char *type, char *pos)
{
  char *ssname;
  invariant(num>0);
  if (num > 1) { // branch
    ssname = new char[strlen(univ)+strlen(type)+strlen(pos)+6];
    sprintf(ssname, "ss-%s-%s-%s", univ, type, pos); // ss-U-A-01101
    ssname = symbolTable.insertString(ssname);

    char *leftpos, *rightpos;
    GuideFunc *g = makeStateSpace(ssname, pos, &leftpos, &rightpos, SS_ORHAT);
    g->name2 = new Name(variants2guide((num+1)/2, variants, idx, typeSet, 
				       typeId, univ, type, leftpos), 
			dummyPos);
    g->name3 = new Name(variants2guide(num/2, variants, idx+(num+1)/2, typeSet,
				       typeId, univ, type, rightpos), 
			dummyPos);
  }
  else { // make variant leaf and start component tree
    ASTVariant *v = variants->get(idx);
    if (!v->path) {
      v->path = new BitList(pos);
      setVariantPos(symbolTable.lookupTypeNumber(typeId), idx, pos);
      symbolTable.setTypeReachable(typeId);
    }

    ssname = new char[strlen(univ)+strlen(type)+strlen(v->name)+7];
    sprintf(ssname, "ss-%s-%s-%s-", univ, type, v->name);// ss-U-A-a1-
    ssname = symbolTable.insertString(ssname);

    char *leftpos, *rightpos;
    GuideFunc *g = makeStateSpace(ssname, "", &leftpos, &rightpos, SS_ORLEAF);
    unsigned size = v->components ? v->components->size() : 0;
    g->name2 = new Name(components2guide((size+1)/2, v->components, 0, 
					 typeSet, typeId, idx,
					 univ, type, v->name, leftpos), 
			dummyPos);
    g->name3 = new Name(components2guide(size/2, v->components, (size+1)/2, 
					 typeSet, typeId, idx,
					 univ, type, v->name, rightpos), 
			dummyPos);
  }
  return ssname;
}

char*
MonaUntypedAST::components2guide(unsigned num, ASTComponentList *components,
				 unsigned idx, IdentList *typeSet, 
				 Ident typeId, unsigned variantidx,
				 char *univ, char *type, 
				 char *variant, char *pos)
{
  char *ssname;
  if (num > 1) { // branch
    ssname = new char[strlen(univ)+strlen(type)+strlen(variant)+strlen(pos)+7];
    sprintf(ssname, "ss-%s-%s-%s-%s", univ, type, variant, pos);// ss-U-A-a1-01
    ssname = symbolTable.insertString(ssname);

    char *leftpos, *rightpos;
    GuideFunc *g = makeStateSpace(ssname, pos, &leftpos, &rightpos, SS_AND);
    g->name2 = new Name(components2guide((num+1)/2, components, idx, 
					 typeSet, typeId, variantidx, 
					 univ, type, variant, leftpos), 
			dummyPos);
    g->name3 = new Name(components2guide(num/2, components, idx+(num+1)/2, 
					 typeSet, typeId, variantidx, 
					 univ, type, variant, rightpos), 
			dummyPos);
  }
  else if (num == 1) { // make new or reuse type
    ASTComponent *c = components->get(idx);
    if (!c->path) {
      c->path = new BitList(pos);
      setComponentPos(symbolTable.lookupTypeNumber(typeId), variantidx, idx, 
		      pos);
    }
    Name t = Name(c->type, dummyPos);
    Ident typeId = symbolTable.lookupIdent(&t);
    if (symbolTable.lookupType(typeId) != Typename)
      TypeError("Type name expected", c->pos);

    ASTVariantList *variants = symbolTable.lookupTypeVariants(typeId);
    if (!typeSet->exists(typeId)) { // make new
      typeSet->insert(typeId);
      ssname = variants2guide(variants->size(), variants, 0, typeSet, typeId,
			      univ, c->type, "");
      symbolTable.addTypeStatespace(typeId, ssname);
      Name n(ssname, dummyPos);
      symbolTable.setSSType(symbolTable.lookupIdent(&n), typeId);
    }
    else { // type already in current universe
      if (variants->size() > 1) {
	ssname = new char[strlen(univ)+strlen(c->type)+6];
	sprintf(ssname, "ss-%s-%s-", univ, c->type); // ss-U-A-
      }
      else {
	ASTVariant *v = variants->get(0);
	ssname = new char[strlen(univ)+strlen(c->type)+strlen(v->name)+7];
	sprintf(ssname, "ss-%s-%s-%s-", univ, c->type, v->name);// ss-U-A-a1-
      }
      ssname = symbolTable.insertString(ssname);
    }
  } 
  else  // need dummy
    ssname = makeDummySS(univ);
  return ssname;
}

GuideFunc*
MonaUntypedAST::makeStateSpace(char *ssname, char *pos,
			       char **leftpos, char **rightpos,
			       SSKind kind)
{
  Name n = Name(ssname, dummyPos);
  symbolTable.insertStatespace(&n);
  
  *leftpos = new char[strlen(pos)+2];
  sprintf(*leftpos, "%s0", pos);
  *leftpos = symbolTable.insertString(*leftpos);
  *rightpos = new char[strlen(pos)+2];
  sprintf(*rightpos, "%s1", pos);
  *rightpos = symbolTable.insertString(*rightpos);
  
  GuideFunc *g = new GuideFunc(new Name(ssname, dummyPos), NULL, NULL, kind);
  guide_declaration->funcList->push_back(g);
  return g;
}

char*
MonaUntypedAST::makeDummySS(char *univ)
{
  char *ssname = new char[strlen(univ)+7];
  sprintf(ssname, "dummy-%s", univ);
  ssname = symbolTable.insertString(ssname);

  if (!symbolTable.exists(ssname)) {
    Name n = Name(ssname, dummyPos);
    symbolTable.insertStatespace(&n);
    guide_declaration->funcList->push_back
      (new GuideFunc(new Name(ssname, dummyPos),
		     new Name(ssname, dummyPos),
		     new Name(ssname, dummyPos),
		     SS_DUMMY));
  }

  return ssname;
}

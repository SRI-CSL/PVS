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

#define _LANGUAGE_C_PLUS_PLUS

#include <iostream.h>
#include <new.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>

#include "env.h"
#include "untyped.h"
#include "predlib.h"
#include "ast.h"
#include "offsets.h"
#include "code.h"
#include "st_dfa.h"
#include "st_gta.h"
#include "timer.h"
#include "lib.h"
#include "printline.h"

extern "C" {
#include "../Mem/mem.h"
}

Options options;
MonaUntypedAST *untypedAST;
SymbolTable symbolTable(1019);
PredicateLib predicateLib;
Offsets offsets;
CodeTable *codeTable;
Guide guide;
AutLib lib;
int numTypes = 0;
bool regenerate = false;

extern int yyparse(void);
extern void loadFile(char *filename);
extern Deque<FileSource *> source; 

char *inputFileName = NULL;

extern Ident lastPosVar, allPosVar;

bool 
ParseArguments(int argc, char *argv[])
{
  options.printProgress = true;
  options.analysis = true;
  options.optimize = 1;
  options.reorder = true;

  switch (argc) {
  case 1:
      return false;

  case 2:
    if (argv[1][0] == '-')
      return false;

  default:
    for (int i = 1; i < argc - 1; i++) {
      
      if (argv[i][0] != '-')
	return false;

      if (strcmp(argv[i], "-demo") == 0)
	options.demo = true;
      else
	switch (argv[i][1]) {
	case 'o':
	  if (sscanf(argv[i]+2, "%u", &options.optimize) != 1)
	    return false;
	  break;
	case 'x':
	  if (argv[i][2] == 'w') {
	    options.printProgress = false;
	    options.externalWhole = true;
	    options.whole = true;
	    options.analysis = false;
	  }
	  else
	    return false;
	  break;
	case 'g':
	  options.printProgress = false;
	  switch (argv[i][2]) {
	  case 'w':
	    options.graphvizDFA = true;
	    options.whole = true;
	    options.analysis = false;
	    break;
	  case 'd':
	    options.graphvizDAG = true;
	    options.analysis = false;
	    break;
	  case 's':
	    options.graphvizSatisfyingEx = true;
	    options.analysis = true;
	    break;
	  case 'c':
	    options.graphvizCounterEx = true;
	    options.analysis = true;
	    break;
	  default:
	    return false;
	  }
	  break;
	default:
	  switch (argv[i][1]) {
	  case 'w':
	    options.whole = true;
	    break;
	  case 'n':
	    options.analysis = false;
	    break;
	  case 'd':
	    options.dump = true;
	    break;
	  case 't':
	    options.time = true;
	    break;
	  case 's':
	    options.statistics = true;
	    break;
	  case 'q':
	    options.printProgress = false;
	    break;
	  case 'c':
	    options.analysis = true;
	    break;
	  case 'e':
	    options.separateCompilation = true;
	    break;
	  case 'i':
	    options.intermediate = true;
	    options.statistics = true;
	    break;
	  case 'f':
	    options.treemodeOutput = true;
	    options.analysis = true;
	    break;
	  case 'h':
	    options.inheritedAcceptance = true;
	    break;
	  case 'u':
	    options.unrestrict = true;
	    break;
	  case 'm':
	    options.alternativeM2LStr = true;
	    break;
	  case 'r':
	    options.reorder = false;
	    break;
	  case 'p':
	    break; // ignore for compatibility
	  default:
	    return false;
	  }
	  if (argv[i][2] != 0)
	    return false;
	  break;
	}
    }
  } 
  
  inputFileName = argv[argc-1];
  return true;
}

void 
Usage()
{
  cout << "MONA v" << VERSION << " for WS1S/WS2S\n"
    << "Copyright (C) 1997-2000 BRICS\n\n"
    << "Usage: mona [options] <filename>\n\n"
    << "Options:\n"
    << " -w   Output whole automaton\n"
    << " -n   Don't analyze automaton\n\n"
    << " -t   Print elapsed time\n"
    << " -s   Print statistics\n"
    << " -i   Print intermediate automata (implies -s)\n"
    << " -d   Dump AST, symboltable, and code DAG\n"
    << " -q   Quiet, don't print progress\n\n"
    << " -e   Enable separate compilation\n"
    << " -oN  Code optimization level N (default 1)\n"
    << " -f   Force normal tree-mode output style\n"
    << " -m   Alternative M2L-Str emulation (v1.3 style)\n"
    << " -h   Inherited acceptance analysis\n"
    << " -u   Unrestrict output automata (create conventional automata)\n\n"
    << " -gw  Output whole automaton in Graphviz format (implies -n -q)\n"
    << " -gs  Output satisfying example tree in Graphviz format (implies -q)\n"
    << " -gc  Output counter-example tree in Graphviz format (implies -q)\n"
    << " -gd  Dump code DAG in Graphviz format (implies -n -q)\n\n"
    << " -xw  Output whole automaton in external format (implies -n -q)\n\n"
    << "Example: mona -w -t -e foo.mona\n\n"
    << "The environment variable MONALIB defines the\n"
    << "directory used for separate-compilation automata.\n\n"
    << "Full documentation is available at http://www.brics.dk/mona\n";
}

void
cpuLimit(int)
{
  cout << "\n\n-----\n"
       << "Interactive Demo time exceeded, execution stopped.\n";
  exit(-1);
}

int 
main(int argc, char *argv[])
{
  set_new_handler(&mem_error);

  if (!ParseArguments(argc, argv)) {
    Usage();
    exit(-1);
  }

  // Disable core dump
  struct rlimit r_core;
  r_core.rlim_cur = 0;
  r_core.rlim_max = 0;
  setrlimit(RLIMIT_CORE, &r_core);

  // Set demo limits 
  if (options.demo) {
    struct rlimit r_cpu, r_as;
    memlimit = true;

    r_cpu.rlim_cur = 30; // max 30 secs.
    r_cpu.rlim_max = 30;
    setrlimit(RLIMIT_CPU, &r_cpu);

    r_as.rlim_cur = 20971520; // max 20MB
    r_as.rlim_max = 20971520;
    setrlimit(RLIMIT_DATA, &r_as);

    signal(SIGXCPU, &cpuLimit);
  }

  initTimer();
  Timer timer_total;
  timer_total.start();
  
  ///////// PARSING ////////////////////////////////////////////////////////

  if (options.printProgress)
    cout << "MONA v" << VERSION " for WS1S/WS2S\n"
      "Copyright (C) 1997-2000 BRICS\n\n"
      "PARSING\n";

  Timer timer_parsing;
  timer_parsing.start();

  loadFile(inputFileName);
  yyparse();
  MonaAST *ast = untypedAST->typeCheck();
  lastPosVar = ast->lastPosVar;
  allPosVar = ast->allPosVar;

  timer_parsing.stop();

  if (options.printProgress) {
    cout << "Time: ";
    timer_parsing.print();
  }

  delete untypedAST;

  if (options.dump) {
    // Dump AST for main formula, verify formulas, and assertion
    cout << "Main formula:\n";
    (ast->formula)->dump();
    Deque<ASTForm *>::iterator vf;
    Deque<char *>::iterator vt;
    for (vf = ast->verifyformlist.begin(), vt = ast->verifytitlelist.begin();
	 vf != ast->verifyformlist.end(); vf++, vt++) {
      cout << "\n\nVerify " << *vt << ":\n";
      (*vf)->dump();
    }
    cout << "\n\nAssertions:\n";
    (ast->assertion)->dump();
    cout << "\n";

    if (lastPosVar != -1)
      cout << "\nLastPos variable: " 
	   << symbolTable.lookupSymbol(lastPosVar) << "\n";
    if (allPosVar != -1)
      cout << "\nAllPos variable: " 
	   << symbolTable.lookupSymbol(allPosVar) << "\n";
    
    // Dump ASTs for predicates and macros
    PredLibEntry *pred = predicateLib.first();
    while (pred != NULL) {
      if (pred->isMacro)
	cout << "\nMacro '";
      else
	cout << "\nPredicate '";
      cout << symbolTable.lookupSymbol(pred->name) 
	   << "':\n";
      (pred->ast)->dump();
      cout << "\n";
      pred = predicateLib.next();
    }

    // Dump restrictions
    if (symbolTable.defaultRestriction1) {
      cout << "\nDefault first-order restriction (" 
	   << symbolTable.lookupSymbol(symbolTable.defaultIdent1) << "):\n";
      symbolTable.defaultRestriction1->dump();
      cout << "\n";
    }
    if (symbolTable.defaultRestriction2) {
      cout << "\nDefault second-order restriction (" 
	   << symbolTable.lookupSymbol(symbolTable.defaultIdent2) << "):\n";
      symbolTable.defaultRestriction2->dump();
      cout << "\n";
    }

    Ident id;
    for (id = 0; id < (Ident) symbolTable.noIdents; id++) {
      Ident t;
      ASTForm *f = symbolTable.getRestriction(id, &t);
      if (f) {
	cout << "\nRestriction for #" << id << " (" 
	     << symbolTable.lookupSymbol(id) << "):";
	if (t != -1)
	  cout << " default\n";
	else {
	  cout << "\n";
	  f->dump();
	  cout << "\n";
	}
      }
    }
  }

  if (options.mode != TREE && 
      (options.graphvizSatisfyingEx || options.graphvizCounterEx ||
       options.inheritedAcceptance)) 
    cout << "Warning: options -gc, -gs, and -h are only used in tree mode\n";
  if (options.mode == TREE && options.graphvizDFA)
    cout << "Warning: option -gw is only used in linear mode\n";
  
  if (options.mode == TREE && (options.dump || options.whole) && 
      !options.externalWhole)
    printGuide();


  ///////// CODE GENERATION ////////////////////////////////////////////////
  
  if (options.printProgress)
    cout << "\nCODE GENERATION\n";
  Timer timer_gencode;
  timer_gencode.start();
  
  // Generate code
  codeTable = new CodeTable;
  VarCode formulaCode = ast->formula->makeCode();
  VarCode assertionCode = ast->assertion->makeCode();
  Deque<VarCode> verifyCode;
  for (Deque<ASTForm *>::iterator i = ast->verifyformlist.begin(); 
       i != ast->verifyformlist.end(); i++)
    verifyCode.push_back((*i)->makeCode());

  // Implicitly assert restrictions for all global variables
  for (IdentList::iterator i = ast->globals.begin(); 
       i != ast->globals.end(); i++)
    assertionCode = andList(assertionCode, getRestriction(*i, NULL));

  // Restrict assertion if not trivial
  if (assertionCode.code->kind != cTrue)
    assertionCode = codeTable->insert
      (new Code_Restrict(assertionCode, assertionCode.code->pos));

  // Add assertion to main formula and to all verify formulas
  for (Deque<VarCode>::iterator i = verifyCode.begin(); 
       i != verifyCode.end(); i++) {
    assertionCode.code->refs++;
    *i = andList(*i, VarCode(copy(assertionCode.vars), assertionCode.code));
    (*i).code->refs++;
  }
  formulaCode = andList(formulaCode, assertionCode);

  timer_gencode.stop();
  if (options.printProgress) {
    codeTable->print_statistics();
    /* if (options.dump && options.statistics)
      codeTable->print_sizes(); */
    cout << "Time: ";
    timer_gencode.print();
  }
  
  ///////// REORDER BDD OFFSETS ////////////////////////////////////////////

  if (options.reorder >= 1) {
    Timer timer_reorder;
    timer_reorder.start();
    if (options.printProgress)
      cout << "\nREORDERING\n";

    // reorder using heuristics
    offsets.reorder();
    
    // regenerate DAG in new codetable
    CodeTable *oldCodeTable = codeTable, *newCodeTable = new CodeTable;
    IdentList emptylist;
    codeTable = newCodeTable;
    regenerate = true; // force making new nodes
    VarCode newcode = formulaCode.substCopy(&emptylist, &emptylist);
    Deque<VarCode> newverifycode;
    for (Deque<VarCode>::iterator i = verifyCode.begin(); 
	 i != verifyCode.end(); i++)
      newverifycode.push_back((*i).substCopy(&emptylist, &emptylist));
    regenerate = false;
    codeTable = oldCodeTable;
    formulaCode.remove();
    for (Deque<VarCode>::iterator i = verifyCode.begin(); 
	 i != verifyCode.end(); i++)
      (*i).remove();
    formulaCode = newcode;
    verifyCode.reset();
    for (Deque<VarCode>::iterator i = newverifycode.begin(); 
	 i != newverifycode.end(); i++)
      verifyCode.push_back(*i);
    delete oldCodeTable;
    codeTable = newCodeTable;

    if (options.printProgress) {
      codeTable->print_statistics2();
      cout << "Time: ";
      timer_reorder.print();
    }
  }

  ///////// REDUCTION AND CODE DUMPING /////////////////////////////////////

  if (options.optimize >= 1) {
    if (options.printProgress)
      cout << "\nREDUCTION\n";
    Timer timer_reduction;
    timer_reduction.start();
    
    // Reduce
    formulaCode.reduceAll(&verifyCode);

    timer_reduction.stop();
    if (options.printProgress) {
      codeTable->print_reduction_statistics();
      /* if (options.dump && options.statistics)
	 codeTable->print_sizes(); */
      cout << "Time: ";
      timer_reduction.print();
    }
  }
  
  if (options.dump) {
    // Dump symboltable
    symbolTable.dump();
    
    // Dump code
    cout << "\nFormula code:\n";
    formulaCode.dump();
    cout << "\n\n";
  }
  
  if (options.graphvizDAG) {
    printf("digraph MONA_CODE_DAG {\n"
	   " size = \"7.5,10.5\";\n"
	   " main [shape = plaintext];\n"
	   " main -> L%x;\n", 
	   (unsigned) formulaCode.code);
    formulaCode.code->viz();
    formulaCode.unmark();
    cout << "}\n";
  }

  ///////// AUTOMATON CONSTRUCTION /////////////////////////////////////////

  // Make variable lists
  Deque<char *> *verifytitlelist = ast->verifytitlelist.copy();
  if (lastPosVar != -1)
    ast->globals.remove(lastPosVar); 
  if (allPosVar != -1)
    ast->globals.remove(allPosVar); 
  ast->globals.sort(); // sort by id (= index)
  int numVars = ast->globals.size();
  int ix = 0;
  char **vnames = new char*[numVars];
  unsigned *offs = new unsigned[numVars];
  char *types = new char[numVars];
  int **univs = new int*[numVars];
  int *trees = new int[numVars];
  SSSet *statespaces = new SSSet[numVars];
  IdentList sign, freeVars;
  IdentList::iterator id;
  for (id = ast->globals.begin(); id != ast->globals.end(); id++, ix++) {
    statespaces[ix] = stateSpaces(*id);
    vnames[ix] = symbolTable.lookupSymbol(*id);
    offs[ix] = offsets.off(*id);
    sign.push_back(ix);
    freeVars.push_back(*id);
    switch (symbolTable.lookupType(*id)) {
    case VarnameTree:
      trees[ix] = 1;
      break;
    default:
      trees[ix] = 0;
    }
    IdentList *uu = symbolTable.lookupUnivs(*id);
    if (uu) {
      unsigned j;
      univs[ix] = new int[uu->size()+1];
      for (j = 0; j < uu->size(); j++)
	univs[ix][j] = symbolTable.lookupUnivNumber(uu->get(j));
      univs[ix][j] = -1;
    }
    else
      univs[ix] = 0;
    switch (symbolTable.lookupType(*id)) 
      {
      case Varname0: 
	types[ix] = 0;
	break;
      case Varname1: 
	types[ix] = 1;
	break;
      default:
	types[ix] = 2;
	break;
      }
  }
  
  if (options.printProgress)
    cout << "\nAUTOMATON CONSTRUCTION\n";

  Timer timer_automaton;
  timer_automaton.start();
  
  DFA *dfa = 0;
  Deque<DFA *> dfalist;
  GTA *gta = 0;
  Deque<GTA *> gtalist;
  
  // Initialize
  bdd_init();
  codeTable->init_print_progress();

  if (options.mode != TREE) { 
    // Generate DFAs
    dfa = formulaCode.DFATranslate();
    if (lastPosVar != -1)
      dfa = st_dfa_lastpos(dfa, lastPosVar);
    if (allPosVar != -1)
      dfa = st_dfa_allpos(dfa, allPosVar);
    for (Deque<VarCode>::iterator i = verifyCode.begin(); 
	 i != verifyCode.end(); i++) {
      DFA *d = (*i).DFATranslate();
      if (lastPosVar != -1)
	d = st_dfa_lastpos(d, lastPosVar);
      if (allPosVar != -1)
	d = st_dfa_allpos(d, allPosVar);
      dfalist.push_back(d);
    }
  }
  else { 
    // Generate GTAs
    gta = formulaCode.GTATranslate();
    if (allPosVar != -1)
      gta = st_gta_allpos(gta, allPosVar);
    for (Deque<VarCode>::iterator i = verifyCode.begin(); 
	 i != verifyCode.end(); i++) {
      GTA *g = (*i).GTATranslate();
      if (allPosVar != -1)
	g = st_gta_allpos(g, allPosVar);
      gtalist.push_back(g);
    }
  }
  formulaCode.remove();
  for (Deque<VarCode>::iterator i = verifyCode.begin(); 
       i != verifyCode.end(); i++)
    (*i).remove();
  
  timer_automaton.stop();
  if (options.printProgress) {
    if (options.statistics)
      cout << "Total automaton construction time: ";
    else
      cout << "Time: ";
    timer_automaton.print();
  }

  delete ast;
  delete codeTable;

  ///////// PRINT AUTOMATON ////////////////////////////////////////////////

  DFA *dfa2 = dfa;
  GTA *gta2 = gta;
  Deque<DFA *> *dfalist2 = &dfalist;
  Deque<GTA *> *gtalist2 = &gtalist;

  if (options.whole &&
      !options.externalWhole)
    cout << "\n";
  if (options.unrestrict) {
    // Unrestrict automata
    if (options.mode != TREE) {
      DFA *t = dfaCopy(dfa2);
      dfaUnrestrict(t);
      dfa2 = dfaMinimize(t);
      dfaFree(t);
      dfalist2 = new Deque<DFA *>;
      for (Deque<DFA *>::iterator i = dfalist.begin(); i != dfalist.end(); i++) {
	t = dfaCopy(*i);
	dfaUnrestrict(t);
	dfalist2->push_back(dfaMinimize(t));
	dfaFree(t);
      }
    }
    else {
      GTA *t = gtaCopy(gta2);
      gtaUnrestrict(t);
      gta2 = gtaMinimize(t);
      gtaFree(t);
      gtalist2 = new Deque<GTA *>;
      for (Deque<GTA *>::iterator i = gtalist.begin(); i != gtalist.end(); i++) {
	t = gtaCopy(*i);
	gtaUnrestrict(t);
	gtalist2->push_back(gtaMinimize(t));
	gtaFree(t);	
      }
    }
  }

  if (options.whole)
    // Print whole automaton
    if (options.mode != TREE) {
      if (options.externalWhole) {
	if (!dfalist.empty())
	  cout << "Main formula:\n";
	DFA *t = dfaCopy(dfa2);
	st_dfa_replace_indices(t, &sign, &freeVars, false, true);
	dfaExport(t, 0, numVars, vnames, types);
	dfaFree(t);
	Deque<DFA *>::iterator i;
	Deque<char *>::iterator j;
	for (i = dfalist2->begin(), j = verifytitlelist->begin();
	     i != dfalist2->end(); i++, j++) {
	  cout << "\nFormula " << *j << ":\n";
	  t = dfaCopy(*i);
	  st_dfa_replace_indices(t, &sign, &freeVars, false, true);
	  dfaExport(t, 0, numVars, vnames, types);
	  dfaFree(t);
	}
      }
      else if (options.graphvizDFA) {
	dfaPrintGraphviz(dfa2, numVars, offs);
	for (Deque<DFA *>::iterator i = dfalist2->begin(); i != dfalist2->end(); i++)
	  dfaPrintGraphviz(*i, numVars, offs);
      }
      else {
	if (!dfalist.empty())
	  cout << "Main formula:\n";
	dfaPrint(dfa2, numVars, vnames, offs);
	Deque<DFA *>::iterator i;
	Deque<char *>::iterator j;
	for (i = dfalist2->begin(), j = verifytitlelist->begin(); 
	     i != dfalist2->end(); i++, j++) {
	  cout << "\nFormula " << *j << ":\n";
	  dfaPrint(*i, numVars, vnames, offs);
	}
      }
    }
    else {
      if (options.externalWhole) {
	if (!gtalist.empty())
	  cout << "Main formula:\n";
	GTA *t = gtaCopy(gta2);
	st_gta_replace_indices(t, &sign, &freeVars, false, true);
	gtaExport(t, 0, numVars, vnames, types, statespaces, 
		  options.inheritedAcceptance);
	gtaFree(t);
	Deque<GTA *>::iterator i;
	Deque<char *>::iterator j;
	for (i = gtalist2->begin(), j = verifytitlelist->begin();
	     i != gtalist2->end(); i++, j++) {
	  cout << "\nFormula " << *j << ":\n";
	  t = gtaCopy(*i);
	  st_gta_replace_indices(t, &sign, &freeVars, false, true);
	  gtaExport(t, 0, numVars, vnames, types, statespaces, 
		    options.inheritedAcceptance);
	  gtaFree(t);
	}
      }
      else {
	if (!gtalist.empty())
	  cout << "Main formula:\n";
	gtaPrint(gta2, offs, numVars, vnames, 
		 options.inheritedAcceptance);
	Deque<GTA *>::iterator i;
	Deque<char *>::iterator j;
	for (i = gtalist2->begin(), j = verifytitlelist->begin();
	     i != gtalist2->end(); i++, j++) {
	  cout << "\nFormula " << *j << ":\n";
	  gtaPrint(*i, offs, numVars, vnames, 
		   options.inheritedAcceptance);
	}
      }
    }
  else if (options.analysis &&
	   !options.graphvizSatisfyingEx &&
	   !options.graphvizCounterEx &&
	   options.printProgress) {
    // Print summary only
    if (options.mode != TREE) {
      if (!dfalist.empty())
	cout << "Main formula:";
      dfaPrintVitals(dfa2);
      Deque<DFA *>::iterator i;
      Deque<char *>::iterator j;
      for (i = dfalist2->begin(), j = verifytitlelist->begin(); 
	   i != dfalist2->end(); i++, j++) {
	cout << "\nFormula " << *j << ":";
	dfaPrintVitals(*i);
      }
    }
    else {
      if (!gtalist.empty())
	cout << "Main formula:";
      gtaPrintTotalSize(gta2);
      Deque<GTA *>::iterator i;
      Deque<char *>::iterator j;
      for (i = gtalist2->begin(), j = verifytitlelist->begin(); 
	   i != gtalist2->end(); i++, j++) {
	cout << "\nFormula " << *j << ":";
	gtaPrintTotalSize(*i);
      }
    }
  }
  if (dfa2 != dfa) {
    dfaFree(dfa2);
    for (Deque<DFA *>::iterator i = dfalist2->begin(); i != dfalist2->end(); i++) 
      dfaFree(*i);
    delete dfalist2;
  }
  if (gta2 != gta) {
    gtaFree(gta2);
    for (Deque<GTA *>::iterator i = gtalist2->begin(); i != gtalist2->end(); i++) 
      gtaFree(*i);
    delete gtalist2;
  }

  ///////// AUTOMATON ANALYSIS /////////////////////////////////////////////

  if (options.analysis) {
    if (options.printProgress)
      cout << "\nANALYSIS\n";
    
    if (options.mode != TREE) {
      if (!dfalist.empty())
	cout << "Main formula:\n";
      dfaAnalyze(dfa, numVars, vnames, offs, types, 
		 options.treemodeOutput);
      Deque<DFA *>::iterator i;
      Deque<char *>::iterator j;
      for (i = dfalist.begin(), j = verifytitlelist->begin(); 
	   i != dfalist.end(); i++, j++) {
	cout << "\nFormula " << *j << ":\n";
	dfaAnalyze(*i, numVars, vnames, offs, types, 
		   options.treemodeOutput);
      }
    }
    else {
      if (numTypes == 0 || options.treemodeOutput) {
	if (!gtalist.empty())
	  cout << "Main formula:\n";
	gtaAnalyze(gta, numVars, vnames, offs,
		   options.graphvizSatisfyingEx,
		   options.graphvizCounterEx);
	Deque<GTA *>::iterator i;
	Deque<char *>::iterator j;
	for (i = gtalist.begin(), j = verifytitlelist->begin(); 
	     i != gtalist.end(); i++, j++) {
	  cout << "\nFormula " << *j << ":\n";
	  gtaAnalyze(*i, numVars, vnames, offs,
		     options.graphvizSatisfyingEx,
		     options.graphvizCounterEx);
	}
      }
      else {
	if (options.graphvizSatisfyingEx ||
	    options.graphvizCounterEx)
	  cout << "Graphviz output of typed trees not implemented.\n";
	if (!gtalist.empty())
	  cout << "Main formula:\n";
	gtaTypeAnalyze(gta, numVars, vnames, types, offs, univs, trees);
	Deque<GTA *>::iterator i;
	Deque<char *>::iterator j;
	for (i = gtalist.begin(), j = verifytitlelist->begin(); 
	     i != gtalist.end(); i++, j++) {
	  cout << "\nFormula " << *j << ":\n";
	  gtaTypeAnalyze(*i, numVars, vnames, types, offs, univs, trees);
	}
      }
    }
  }

  ///////// CLEAN UP ///////////////////////////////////////////////////////

  if (options.mode != TREE) {
    dfaFree(dfa);
    for (Deque<DFA *>::iterator i = dfalist.begin(); i != dfalist.end(); i++)
      dfaFree(*i);
  }
  else {
    gtaFree(gta);
    for (Deque<GTA *>::iterator i = gtalist.begin(); i != gtalist.end(); i++)
      gtaFree(*i);
    freeGuide();
  }
  delete verifytitlelist;

  Deque<FileSource *>::iterator i;
  for (i = source.begin(); i != source.end(); i++)
    delete *i;
  
  for (ix = 0; ix < numVars; ix++) {
    delete[] univs[ix];
    mem_free(statespaces[ix]);
  }
  delete[] statespaces;
  delete[] vnames;
  delete[] offs;
  delete[] types;
  delete[] univs;
  delete[] trees;
  freeTreetypes();
    
  if (options.statistics)
    print_statistics();

  if (options.time) {
    timer_total.stop();
    cout << "\nTotal time:     ";
    timer_total.print();
    print_timing();
  }
  else if (options.printProgress) { 
    timer_total.stop();
    cout << "\nTotal time: ";
    timer_total.print();
  }
}

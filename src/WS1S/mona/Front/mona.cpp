//
// mona.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#define _LANGUAGE_C_PLUS_PLUS

#include <iostream.h>
#include <new.h>
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

Environment environment;
MonaUntypedAST *untypedAST;
SymbolTable symbolTable(1019);
PredicateLib predicateLib;
Offsets offsets;
CodeTable codeTable;
Guide guide;
AutLib lib;

extern int yyparse(void);
extern void loadFile(char *filename);
extern Deque<FileSource *> source; 

char *inputFileName = NULL;

extern Ident lastPosVar;

extern int memlimit;

bool 
ParseArguments(int argc, char *argv[])
{
  environment.reuseDegree = 100;
  environment.printProgress = true;
  environment.analysis = true;

  switch (argc) {
  case 1 :
    return false;

  case 2 :
    if (argv[1][0] == '-')
      return false;

  default :
    for (int i = 1; i < argc - 1; i++) {
      
      if (argv[i][0] != '-')
	return false;

      if (strcmp(argv[i], "-demo") == 0)
	environment.demo = true;
      else
	switch (argv[i][1]) {
	case 'o':
	  if (sscanf(argv[i]+2, "%u", &environment.optimize) != 1)
	    return false;
	  break;
	case 'r':
	  if (sscanf(argv[i]+2,"%u", &environment.reuseDegree) != 1)
	    return false;
	  break;
	case 'g':
	  environment.printProgress = false;
	  switch (argv[i][2]) {
	  case 'w':
	    environment.graphvizDFA = true;
	    environment.whole = true;
	    environment.analysis = false;
	    break;
	  case 'd':
	    environment.graphvizDAG = true;
	    environment.analysis = false;
	    break;
	  case 's':
	    environment.graphvizSatisfyingEx = true;
	    environment.analysis = true;
	    break;
	  case 'c':
	    environment.graphvizCounterEx = true;
	    environment.analysis = true;
	    break;
	  default:
	    return false;
	  }
	  break;
	default:
	  switch (argv[i][1]) {
	  case 'w':
	    environment.whole = true;
	    break;
	  case 'n':
	    environment.analysis = false;
	    break;
	  case 'd':
	    environment.dump = true;
	    break;
	  case 't':
	    environment.time = true;
	    break;
	  case 's':
	    environment.statistics = true;
	    break;
	  case 'q':
	    environment.printProgress = false;
	    break;
	  case 'c':
	    environment.analysis = true;
	    break;
	  case 'e':
	    environment.separateCompilation = true;
	    break;
	  case 'i':
	    environment.intermediate = true;
	    environment.statistics = true;
	    break;
	  case 'f':
	    environment.treemodeOutput = true;
	    environment.analysis = true;
	    break;
	  case 'h':
	    environment.inheritedAcceptance = true;
	    environment.whole = true;
	    break;
	  case 'u':
	    environment.unrestrict = true;
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
    "Copyright (C) 1997-1998 BRICS\n\n"
    "Usage: mona [options] <filename>\n\n"
    "Options:\n"
    " -w   Output whole automaton\n"
    " -n   Don't analyze automaton\n\n"
    " -t   Print elapsed time\n"
    " -s   Print statistics\n"
    " -i   Print intermediate automata (implies -s)\n"
    " -d   Dump AST, symboltable and code DAG\n"
    " -q   Quiet, don't print progress\n\n"
    " -e   Enable separate compilation\n"
    " -oN  Code optimization level N (not implemented yet)\n"
    " -rN  Automaton reuse degree N (default 100)\n"
    " -f   Force tree-mode output style\n"
    " -h   Print inherited acceptance status (implies -w)\n"
    " -u   Unrestrict output automata\n\n"
    " -gw  Output whole automaton in Graphviz format (implies -n -q)\n"
    " -gs  Output satisfying example tree in Graphviz format (implies -q)\n"
    " -gc  Output counter-example tree in Graphviz format (implies -q)\n"
    " -gd  Dump code DAG in Graphviz format (implies -n -q)\n\n"
    "Example: mona -w -t -e foo.mona\n\n"
    "The environment variable MONALIB defines the\n"
    "directory used for separate-compilation automata.\n\n"
    "Full documentation is available at http://www.brics.dk/mona\n";
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
  if (environment.demo) {
    struct rlimit r_cpu, r_as;
    memlimit = true;

    r_cpu.rlim_cur = 30; // max 30 secs.
    r_cpu.rlim_max = 30;
    setrlimit(RLIMIT_CPU, &r_cpu);

    r_as.rlim_cur = 16777216; // max 16MB
    r_as.rlim_max = 16777216;
    setrlimit(RLIMIT_AS, &r_as);

    signal(SIGXCPU, &cpuLimit);
  }

  initTimer();
  Timer timer_total;
  timer_total.start();
  
  ///////// PARSING ////////////////////////////////////////////////////////

  if (environment.printProgress)
    cout << "MONA v" << VERSION " for WS1S/WS2S\n"
      "Copyright (C) 1997-1998 BRICS\n\n"
      "PARSING\n";

  Timer timer_parsing;
  timer_parsing.start();

  loadFile(inputFileName);
  yyparse();
  MonaAST *ast = untypedAST->typeCheck();
  lastPosVar = ast->lastPosVar;

  timer_parsing.stop();

  if (environment.printProgress) {
    cout << "Time: ";
    timer_parsing.print();
  }

  delete untypedAST;

  if (environment.dump) {
    // Dump AST for main formula
    cout << "Main formula:\n";
    (ast->formula)->dump();
    cout << "\n\nAssertions:\n";
    (ast->assertion)->dump();
    cout << "\n";

    if (lastPosVar != -1)
      cout << "\nLastPos variable: " 
	   << symbolTable.lookupSymbol(lastPosVar) << "\n";
	
    
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
      cout << "\nDefault restriction 1 (" 
	   << symbolTable.lookupSymbol(symbolTable.defaultIdent1) << "):\n";
      symbolTable.defaultRestriction1->dump();
      cout << "\n";
    }
    if (symbolTable.defaultRestriction2) {
      cout << "\nDefault restriction 2 (" 
	   << symbolTable.lookupSymbol(symbolTable.defaultIdent2) << "):\n";
      symbolTable.defaultRestriction2->dump();
      cout << "\n";
    }

    Ident id;
    for (id = 0; id < (Ident) symbolTable.noIdents; id++) {
      Ident t;
      ASTForm *f = symbolTable.getRestriction(id, &t);
      if (f) {
	cout << "\nRestriction for id " << id << " (" 
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

  if (environment.mode != TREE && 
      (environment.graphvizSatisfyingEx || environment.graphvizCounterEx ||
       environment.inheritedAcceptance)) 
    cout << "Warning: options -gc, -gs and -h are only used in tree mode\n";
  if (environment.mode == TREE && environment.graphvizDFA)
    cout << "Warning: option -gw is only used in linear mode\n";
  
  if (environment.mode == TREE && (environment.dump || environment.whole))
    printGuide();


  ///////// OFFSETS ////////////////////////////////////////////////////////

  offsets.set();

  ///////// CODE GENERATION ////////////////////////////////////////////////
  
  if (environment.printProgress)
    cout << "\nCODE GENERATION\n";

  Timer timer_gencode;
  timer_gencode.start();
  
  // Generate code
  VarCode formulaCode = ast->formula->makeCode();
  VarCode assertionCode = ast->assertion->makeCode();

  // Reduce
  // ....
  
  // Make variable lists
  if (lastPosVar != -1)
    ast->globals.remove(lastPosVar); 
  ast->globals.sort();
  int numVars = ast->globals.size();
  int ix = 0;
  char **vnames = new char*[numVars];
  unsigned *offs = new unsigned[numVars];
  char *types = new char[numVars];
  IdentList::iterator id;
  for (id = ast->globals.begin(); id != ast->globals.end(); id++, ix++) {
    vnames[ix] = symbolTable.lookupSymbol(*id);
    offs[ix] = offsets.off(*id);
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
  
  if (environment.dump) {
    // Dump symboltable
    symbolTable.dump();

    // Dump code
    cout << "\nMain-formula code:\n";
    formulaCode.code->dump(formulaCode.vars, NULL);
    cout << "\n\nAssertion code:\n";
    assertionCode.code->dump(assertionCode.vars, NULL);
    cout << "\n\n";
  }
  
  timer_gencode.stop();

  if (environment.graphvizDAG) {
    printf("digraph MONA_CODE_DAG {\n"
	   " size = \"7.5,10.5\";\n"
	   " main [shape = plaintext];\n"
	   " main -> L%x;\n"
	   " assertion [shape = plaintext];\n"
	   " assertion -> L%x;\n", 
	   (unsigned) formulaCode.code, 
	   (unsigned) assertionCode.code);
    formulaCode.code->viz();
    assertionCode.code->viz();
    cout << "}\n";
  }

  if (environment.printProgress) {
    codeTable.print_statistics();
    /* if (environment.dump && environment.statistics)
      codeTable.print_sizes(); */
    cout << "Time: ";
    timer_gencode.print();
  }
  
  ///////// AUTOMATON CONSTRUCTION /////////////////////////////////////////

  if (environment.printProgress)
    cout << "\nAUTOMATON CONSTRUCTION\n";

  Timer timer_automaton;
  timer_automaton.start();
  
  DFA *dfaImpl = 0, *dfaConj = 0;
  GTA *gtaImpl = 0, *gtaConj = 0;
  
  // Initialize BDD
  bdd_init();

  if (environment.mode != TREE) { 
    // Generate DFAs
    DFA *assertionDfa = assertionCode.code->DFATranslate(assertionCode.vars);
    assertionCode.detach();
    DFA *formulaDfa = formulaCode.code->DFATranslate(formulaCode.vars);
    formulaCode.detach();

    dfaImpl = st_dfa_minimization(st_dfa_product(st_dfa_copy(assertionDfa),
                                                 st_dfa_copy(formulaDfa),
                                                 dfaIMPL, 
                                                 dummyPos));
    dfaConj = st_dfa_minimization(st_dfa_product(assertionDfa,
                                                 formulaDfa,
                                                 dfaAND,
                                                 dummyPos));
    if (lastPosVar != -1) {
      dfaImpl = st_dfa_lastpos(dfaImpl, offsets.off(lastPosVar));
      dfaConj = st_dfa_lastpos(dfaConj, offsets.off(lastPosVar));
    }
  }
  else { 
    // Generate GTAs
    GTA *assertionGta = assertionCode.code->GTATranslate(assertionCode.vars);
    assertionCode.detach();
    GTA *formulaGta = formulaCode.code->GTATranslate(formulaCode.vars);
    formulaCode.detach();

    gtaImpl = st_gta_minimization(st_gta_product(st_gta_copy(assertionGta),
                                                 st_gta_copy(formulaGta),
                                                 gtaIMPL,
                                                 dummyPos));
    gtaConj = st_gta_minimization(st_gta_product(assertionGta,
                                                 formulaGta,
                                                 gtaAND,
                                                 dummyPos));
    if (lastPosVar != -1) {
      gtaImpl = st_gta_lastpos(gtaImpl, offsets.off(lastPosVar));
      gtaConj = st_gta_lastpos(gtaConj, offsets.off(lastPosVar));
    }
  }
  formulaCode.detach();
  assertionCode.detach();
  
  timer_automaton.stop();
  if (environment.printProgress) {
    if (environment.statistics)
      cout << "Total automaton construction time: ";
    else
      cout << "Time: ";
    timer_automaton.print();
  }

  delete ast;

  ///////// PRINT AUTOMATON ////////////////////////////////////////////////

  if (environment.whole)
    cout << "\n";
  if (environment.unrestrict) {
    // Unrestrict automaton (has no effect on analysis)
    if (environment.whole &&
	!environment.graphvizSatisfyingEx &&
	!environment.graphvizCounterEx &&
	!environment.graphvizDFA &&
	!environment.graphvizDAG)
      cout << "Unrestricting automaton\n\n";
    if (environment.mode != TREE) {
      DFA *t = dfaConj;
      dfaUnrestrict(t);
      dfaConj = dfaMinimize(t);
      dfaFree(t);
    }
    else {
      GTA *t = gtaConj;
      gtaUnrestrict(t);
      gtaConj = gtaMinimize(t);
      gtaFree(t);
    }
  }

  if (environment.whole)
    // Print whole automaton
    if (environment.mode != TREE) {
      if (environment.graphvizDFA)
	dfaPrintGraphviz(dfaConj, numVars, offs);
      else
	dfaPrint(dfaConj, numVars, vnames, offs);
    }
    else {
      gtaPrint(gtaConj, offs, numVars, vnames, 
	       environment.inheritedAcceptance);
    }
  else if (environment.analysis &&
	   !environment.graphvizSatisfyingEx &&
	   !environment.graphvizCounterEx) {
    // Print summary only
    if (environment.mode != TREE)
      dfaPrintVitals(dfaConj);
    else 
      gtaPrintVitals(gtaConj);
  }

  ///////// AUTOMATON ANALYSIS /////////////////////////////////////////////

  if (environment.analysis) {
    if (environment.printProgress)
      cout << "\nANALYSIS\n";
    
    if (environment.mode != TREE)
      dfaAnalyze(dfaImpl, dfaConj, numVars, vnames, offs, 
		 types, environment.treemodeOutput);
    else 
      gtaAnalyze(gtaImpl, gtaConj, numVars, vnames, offs,
		 environment.graphvizSatisfyingEx,
		 environment.graphvizCounterEx);
  }

  ///////// CLEAN UP ///////////////////////////////////////////////////////

  if (environment.mode != TREE) {
    dfaFree(dfaConj);
    dfaFree(dfaImpl);
  }
  else {
    gtaFree(gtaConj);
    gtaFree(gtaImpl);
    freeGuide();
  }

  Deque<FileSource *>::iterator i;
  for (i = source.begin(); i != source.end(); i++)
    delete *i;
  
  delete[] vnames;
  delete[] offs;
  delete[] types;
    
  if (environment.statistics)
    print_statistics();

  if (environment.time) {
    timer_total.stop();
    cout << "\nTotal time:     ";
    timer_total.print();
    print_timing();
  }
  else if (environment.printProgress) { 
    timer_total.stop();
    cout << "\nTotal time: ";
    timer_total.print();
  }
}

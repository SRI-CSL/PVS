//
// env.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __ENV_H
#define __ENV_H

enum Mode {DEFAULT, LINEAR, TREE};

class Environment {
public:
  Environment() :
    time(false), whole(false), mode(DEFAULT),
    statistics(false), printProgress(false),
    analysis(false), separateCompilation(false),
    dump(false), intermediate(false),
    treemodeOutput(false), m2l(false), 
    graphvizDFA(false), graphvizDAG(false),
    graphvizSatisfyingEx(false), graphvizCounterEx(false), 
    demo(false), inheritedAcceptance(false), unrestrict(false),
    optimize(0), reuseDegree(0) {}

  bool time;
  bool whole;
  Mode mode;
  bool statistics;
  bool printProgress;
  bool analysis;
  bool separateCompilation;
  bool dump;
  bool intermediate;
  bool treemodeOutput;
  bool m2l;
  bool graphvizDFA;
  bool graphvizDAG;
  bool graphvizSatisfyingEx;
  bool graphvizCounterEx;
  bool demo;
  bool inheritedAcceptance;
  bool unrestrict;
  unsigned optimize;
  unsigned reuseDegree;
};

#endif

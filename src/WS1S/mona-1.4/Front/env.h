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

#ifndef __ENV_H
#define __ENV_H

enum Mode {LINEAR, TREE};

class Options {
public:
  Options() :
    time(false), whole(false), mode(LINEAR),
    statistics(false), printProgress(false),
    analysis(false), separateCompilation(false),
    dump(false), intermediate(false),
    treemodeOutput(false), m2l(false), 
    graphvizDFA(false), graphvizDAG(false),
    graphvizSatisfyingEx(false), graphvizCounterEx(false), 
    externalWhole(false), demo(false), 
    inheritedAcceptance(false), unrestrict(false), 
    alternativeM2LStr(false), reorder(false), optimize(0) {}

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
  bool externalWhole;
  bool demo;
  bool inheritedAcceptance;
  bool unrestrict;
  bool alternativeM2LStr;
  bool reorder;
  unsigned optimize;
};

#endif

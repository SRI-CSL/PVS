//
// timer.h
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __TIMER_H
#define __TIMER_H

class Timer {
public:
  Timer() : res(0) {}

  void start();
  void stop();

  void print();

private:
  unsigned long tclocks, tsteps, res;
};

void initTimer();

#endif

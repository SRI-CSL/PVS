//
// timer.cpp
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#define _LANGUAGE_C_PLUS_PLUS

#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <signal.h>
#include "timer.h"

const unsigned long hour = 360000;
const unsigned long min  = 6000;
const unsigned long sec  = 100;

const unsigned long step = 2000; // sec. pr. step

static unsigned long prev, clocks, steps; // CPU time (user+system)

void
refresh(int)
{
  unsigned long t = (unsigned long) clock();
  clocks += t - prev;
  prev = t;
  while (clocks >= step*CLOCKS_PER_SEC) {
    steps++;
    clocks -= step*CLOCKS_PER_SEC;
  }
  signal(SIGALRM, &refresh);
  alarm(step);
}

void
initTimer()
{
  clocks = steps = 0;
  refresh(0);
}

void
Timer::start()
{
  alarm(0);
  refresh(0);
  tsteps = steps;
  tclocks = clocks;
}

void
Timer::stop()
{
  alarm(0);
  refresh(0);
  unsigned long ttsteps = steps;
  unsigned long ttclocks = clocks;

  if (ttclocks < tclocks) {
    ttclocks += step*CLOCKS_PER_SEC;
    ttsteps--;
  }
  res += (ttsteps - tsteps)*step*100 + (ttclocks - tclocks)/(CLOCKS_PER_SEC/100);
}

void
Timer::print()
{
  unsigned long t = res, hours, mins, secs;

  hours = t / hour;
  t -= hours * hour;

  mins = t / min;
  t -= mins * min;

  secs = t / sec;
  t -= secs * sec;

  printf("%02lu:%02lu:%02lu.%02lu\n", hours, mins, secs, t);
}

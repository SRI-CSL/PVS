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

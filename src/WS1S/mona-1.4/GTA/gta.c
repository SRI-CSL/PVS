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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../Mem/mem.h"
#include "gta.h"

int gta_in_mem; /* number of automata currently in memory */

static void setHits()
{
  SsId s;

  guide.hitsLeft = (SsId **) mem_alloc(sizeof(SsId *)*guide.numSs);
  guide.hitsRight = (SsId **) mem_alloc(sizeof(SsId *)*guide.numSs);
  guide.numHitsLeft = (unsigned *) mem_alloc(sizeof(unsigned)*guide.numSs);
  guide.numHitsRight = (unsigned *) mem_alloc(sizeof(unsigned)*guide.numSs);

  for (s = 0; s < guide.numSs; s++) 
    guide.numHitsLeft[s] = guide.numHitsRight[s] = 0;
  for (s = 0; s < guide.numSs; s++) {
    guide.numHitsLeft[guide.muLeft[s]]++;
    guide.numHitsRight[guide.muRight[s]]++;
  }
  for (s = 0; s < guide.numSs; s++) {
    guide.hitsLeft[s] = 
      (SsId *) mem_alloc(sizeof(SsId)*guide.numHitsLeft[s]);
    guide.hitsRight[s] = 
      (SsId *) mem_alloc(sizeof(SsId)*guide.numHitsRight[s]);
    guide.numHitsLeft[s] = guide.numHitsRight[s] = 0;
  }
  for (s = 0; s < guide.numSs; s++) {
    guide.hitsLeft[guide.muLeft[s]]
      [(guide.numHitsLeft[guide.muLeft[s]])++] = s;
    guide.hitsRight[guide.muRight[s]]
      [(guide.numHitsRight[guide.muRight[s]])++] = s;
  }  
}

static void setUnivSS()
{
  int u, i;
  guide.numUnivSS = (unsigned *) mem_alloc(sizeof(unsigned)*guide.numUnivs);
  guide.univSS = (SsId **) mem_alloc(sizeof(SsId *)*guide.numUnivs);
  guide.ssUniv = (int *) mem_alloc(sizeof(int)*guide.numSs);
  guide.ssUnivRoot = (char *) mem_alloc(guide.numSs);

  for (i = 0; i < guide.numSs; i++) {
    guide.ssUniv[i] = -1; /* unmark */
    guide.ssUnivRoot[i] = 0;
  }

  for (u = 0; u < guide.numUnivs; u++) {
    SsId s = 0;
    unsigned head = 0, tail = 0;

    /* find state space of universe root */
    for (i = 0; guide.univPos[u][i]; i++) {
      guide.ssUniv[s] = -2; /* mark as 'hat' */
      if (guide.univPos[u][i] == '0')
	s = guide.muLeft[s];
      else
	s = guide.muRight[s];
    }
    guide.univSS[u] = (SsId *) mem_alloc(sizeof(SsId)*guide.numSs);
    guide.univSS[u][head++] = s;
    guide.ssUniv[s] = u;
    guide.ssUnivRoot[s] = 1;

    /* find reachable state spaces */
    while (head > tail) {
      SsId left, right;
      s = guide.univSS[u][tail++];
      left = guide.muLeft[s];
      right = guide.muRight[s];
      if (guide.ssUniv[left] != u) {
	guide.univSS[u][head++] = left;
	guide.ssUniv[left] = u;
      } 
      if (left != right && guide.ssUniv[right] != u) {
	guide.univSS[u][head++] = right;
	guide.ssUniv[right] = u;
      } 
    }
    guide.numUnivSS[u] = head;
  }
}

void makeGuide(unsigned numSs, SsId *muLeft, SsId *muRight, char **ssName,
	       unsigned numUnivs, char **univPos, char **univName,
	       int ssType[], SsKind *ssKind)
{
  guide.numSs = numSs;
  guide.muLeft = muLeft;
  guide.muRight = muRight;
  guide.ssName = ssName;
  guide.numUnivs = numUnivs;
  guide.univPos = univPos;
  guide.univName = univName;
  guide.ssKind = ssKind;
  guide.ssType = ssType;

  setHits();
  setUnivSS();
}

void makeDefaultGuide(unsigned numUnivs, char **univName)
{
  int u;
  SsId s;
  char **ssPos;

  invariant(numUnivs > 0);

  guide.numUnivs = numUnivs;
  guide.univName = univName;
  guide.ssKind = 0;

  guide.numSs = numUnivs*2-1;
  guide.univPos = (char **) mem_alloc(sizeof(char *)*numUnivs);
  guide.muLeft = (SsId *) mem_alloc(sizeof(SsId)*guide.numSs);
  guide.muRight = (SsId *) mem_alloc(sizeof(SsId)*guide.numSs);
  guide.ssName = (char **) mem_alloc(sizeof(char *)*guide.numSs);

  ssPos = (char **) mem_alloc(sizeof(char *)*guide.numSs);

  ssPos[0] = (char *) mem_alloc(1);
  ssPos[0][0] = 0;
  for (s = 0; s < numUnivs - 1; s++) {
    guide.muLeft[s] = 2*s+1;
    guide.muRight[s] = 2*s+2;
    guide.ssName[s] = (char *) mem_alloc(6);
    strcpy(guide.ssName[s], "<hat>");
    ssPos[2*s+1] = (char *) mem_alloc(strlen(ssPos[s])+2);
    strcpy(ssPos[2*s+1], ssPos[s]);
    strcat(ssPos[2*s+1], "0");
    ssPos[2*s+2] = (char *) mem_alloc(strlen(ssPos[s])+2);
    strcpy(ssPos[2*s+2], ssPos[s]);
    strcat(ssPos[2*s+2], "1");
  }
  for (u = 0; u < numUnivs; u++, s++) {
    guide.muLeft[s] = s;
    guide.muRight[s] = s;
    guide.ssName[s] = (char *) mem_alloc(strlen(univName[u])+1);
    guide.univPos[u] = (char *) mem_alloc(strlen(ssPos[s])+1);
    strcpy(guide.univPos[u], ssPos[s]);
    strcpy(guide.ssName[s], univName[u]);
  }
  for (s = 0; s < guide.numSs; s++)
    mem_free(ssPos[s]);
  mem_free(ssPos);

  setHits();
  setUnivSS();
}

void freeGuide()
{
  SsId s;

  for (s = 0; s < guide.numSs; s++) {
    mem_free(guide.hitsLeft[s]);
    mem_free(guide.hitsRight[s]);
  }
  mem_free(guide.hitsLeft);
  mem_free(guide.hitsRight);
  mem_free(guide.numHitsLeft);
  mem_free(guide.numHitsRight);
  mem_free(guide.muLeft);
  mem_free(guide.muRight);
  mem_free(guide.ssKind);
}

void printGuide()
{
  SsId s;
  printf("Guide:\n");
  for (s = 0; s < guide.numSs; s++) {
    printf(" %s: %d -> (%d,%d)", 
	   guide.ssName[s], s, guide.muLeft[s], guide.muRight[s]);
    if (guide.ssKind)
      switch (guide.ssKind[s]) {
      case gtaSSUNIVHAT:
	printf(" [universe branch]");
	break;
      case gtaSSORHAT: 
	printf(" [variant-tree branch]");
	break;
      case gtaSSORLEAF: 
	printf(" [variant-tree leaf]");
	break;
      case gtaSSAND: 
	printf(" [component-tree branch]");
	break;
      case gtaSSDUMMY: 
	printf(" [dummy]");
	break;
      }
    printf("\n");
  }
  printf("\n");
}

int hasMember(SSSet ss, SsId s)
{
  return ss[s];
}

static int univs_required;

static int checkPosCovered(char *pos)
{
  int u, res;
  char *pos0, *pos1;

  for (u = 0; u < guide.numUnivs; u++)
    if (strcmp(guide.univPos[u], pos) == 0)
      return 1;

  univs_required++;
  if (univs_required > guide.numUnivs)
    return 0;

  pos0 = (char *) mem_alloc(strlen(pos)+2);
  strcpy(pos0, pos);
  strcat(pos0, "0");
  pos1 = (char *) mem_alloc(strlen(pos)+2);
  strcpy(pos1, pos);
  strcat(pos1, "1");
  res = checkPosCovered(pos0) && checkPosCovered(pos1);
  mem_free(pos0);
  mem_free(pos1);
  return res;
}

int checkAllCovered()
{
  univs_required = 1;
  return checkPosCovered("");
}

int checkDisjoint()
{
  int u, i;

  for (u = 0; u < guide.numUnivs; u++)
    for (i = 0; i < guide.numUnivSS[u]; i++)
      if (guide.ssUniv[guide.univSS[u][i]] != u)
	return 0;
  return 1;
}

int checkAllUsed()
{
  int s;

  for (s = 0; s < guide.numSs; s++)
    if (guide.ssUniv[s] == -1) /* -1: not met, -2: hat */
      return 0;
  return 1;
}

GTA *gtaMake()
{
  SsId s;

  GTA *res = (GTA *) mem_alloc(sizeof(GTA));
  res->final = 0; /* nothing allocated */
  res->ss = (StateSpace *) mem_alloc(sizeof(StateSpace)*guide.numSs);

  for (s = 0; s < guide.numSs; s++) {
    StateSpace *ss = &res->ss[s];

    ss->size = 0;
    ss->ls = ss->rs = 0;
    ss->behaviour = 0; /* nothing allocated */
    ss->bddm = 0; /* nothing allocated */
  }
  gta_in_mem++;
  return res;
}

void gtaFree(GTA* P)
{
  SsId i;

  mem_free(P->final);
  for (i = 0; i < guide.numSs; i++) {
    mem_free(P->ss[i].behaviour);
    bdd_kill_manager(P->ss[i].bddm);
  }
  mem_free(P->ss);
  mem_free(P);
  gta_in_mem--;
}

/* gta.c */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "gta.h"

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

  for (i = 0; i < guide.numSs; i++)
    guide.ssUniv[i] = -1; /* unmark */

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
	       unsigned numUnivs, char **univPos, char **univName)
{
  guide.numSs = numSs;
  guide.muLeft = muLeft;
  guide.muRight = muRight;
  guide.ssName = ssName;
  guide.numUnivs = numUnivs;
  guide.univPos = univPos;
  guide.univName = univName;

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
    free(ssPos[s]);
  free(ssPos);

  setHits();
  setUnivSS();
}

void freeGuide()
{
  SsId s;

  for (s = 0; s < guide.numSs; s++) {
    free(guide.hitsLeft[s]);
    free(guide.hitsRight[s]);
  }
  free(guide.hitsLeft);
  free(guide.hitsRight);
  free(guide.numHitsLeft);
  free(guide.numHitsRight);
  free(guide.muLeft);
  free(guide.muRight);
}

void printGuide()
{
  SsId s;
  printf("Guide:\n");
  for (s = 0; s < guide.numSs; s++)
    printf(" %s: %d -> (%d,%d)\n", 
	   guide.ssName[s], s, guide.muLeft[s], guide.muRight[s]);
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
  free(pos0);
  free(pos1);
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
  return res;
}

void gtaFree(GTA* P)
{
  SsId i;

  free(P->final);
  for (i = 0; i < guide.numSs; i++) {
    free(P->ss[i].behaviour);
    bdd_kill_manager(P->ss[i].bddm);
  }
  free(P->ss);
  free(P);
}

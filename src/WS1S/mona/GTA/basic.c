/* basic.c */

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
#include "gta.h"

GTA *gtaIn(int P, int Q, SSSet uP, SSSet uQ)
{ 
  if (P == Q) {
    /* this should never happen */
    invariant(0);
    return 0;
  }
  else{
    int var[2], d, Ok = 0, Bad = 1, Done = 2;
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(3); /* 3 states in root */
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 3, 3, var, 2); /* 3 states in all state spaces */
      if ((!hasMember(uP, d)) && (!hasMember(uQ, d))) {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok); /* (Ok, Ok, anything) -> OK */
      }
      else if (hasMember(uP, d) && !hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 1);
	gtaStoreException(Ok, "0X"); /* (Ok, Ok, 0X) -> Ok */
	gtaStoreDefault(Bad); /* (Ok, Ok, x) -> Bad, if x<>0X */
      }
      else if (!hasMember(uP, d) && hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok); 
      }
      else { /* d \in uP \cap uQ */
	gtaAllocExceptions(Ok, Ok, 3);
	gtaStoreException(Ok, "01");
	gtaStoreException(Done, "11");
	gtaStoreException(Ok, "00");
	gtaStoreDefault(Bad); 
      }
      
      gtaAllocExceptions(Done, Bad, 0);
      gtaStoreDefault(Bad);

      gtaAllocExceptions(Bad, Done, 0);
      gtaStoreDefault(Bad);

      gtaAllocExceptions(Done, Ok, 0);
      gtaStoreDefault(Done);
  
      gtaAllocExceptions(Ok, Done, 0);
      gtaStoreDefault(Done);
      
      gtaAllocExceptions(Done, Done, 0);
      gtaStoreDefault(Bad);

      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaBuildDelta(Ok); 
    }
  
    free(uP);
    free(uQ);
    return gtaBuild("001");
  }
}   



/* gtaFirstOrder checks that there is at least one position with a 1
   in track P and that there are not two such positions that are
   incomparable */
GTA *gtaFirstOrder(int P, SSSet uP) { 
int var[1], d, Bad = 0, Notmet = 1, Met=2; 
/* Met in hat transition is accepting */

  var[0] = P;
  
  gtaSetup(3); /* 3 states in root */
  
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 3, 3, var, 1); /* 3 states in all state spaces */

    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Notmet, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Met, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Notmet, Bad, 0);
    gtaStoreDefault(Bad);
    
    if (hasMember(uP, d)) {
      gtaAllocExceptions(Notmet,Notmet,1);
      gtaStoreException(Met, "1"); /* (Notmet,Notmet,H) -> Met */
      gtaStoreDefault(Notmet); 
    
      gtaAllocExceptions(Notmet,Met,1);
      gtaStoreException(Bad, "1"); /* (Notmet,Met,H) -> Bad (unique pos.) */
      gtaStoreDefault(Met);

      gtaAllocExceptions(Met,Notmet,1);
      gtaStoreException(Bad, "1"); /* (Notmet,Met,H) -> Bad (unique pos.) */
      gtaStoreDefault(Met); 
    }
    else {
      gtaAllocExceptions(Notmet, Notmet, 0);
      gtaStoreDefault(Notmet); 

      gtaAllocExceptions(Notmet, Met, 0);
      gtaStoreDefault(Met); 

      gtaAllocExceptions(Met, Notmet, 0);
      gtaStoreDefault(Met); 
    }      
    
    gtaAllocExceptions(Met, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Met, Met, 0);
    gtaStoreDefault(Bad);
    
    gtaBuildDelta(Notmet); 
  }
  free(uP);
  return gtaBuild("001"); /* Met is the only accepting state */
}

/* gtaLast accepts if and only if there is an occurrence of
   variable P in every leaf in every non-empty universe in uP;
   the condition that the universe be non-empty is derived
   from our use below of the state Initial;it is not possible
   currently to use initial states that depend on the universe,
   thus we are forced to accept an empty tree in a universe in uP */
GTA *gtaLast(int P, SSSet uP) { 
  int var[1], d, Initial = 0, Bad = 1, Met=2; 
/* Met in hat transition is accepting */

  var[0] = P;
  
  gtaSetup(3); /* 3 states in root */
  
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 3, 3, var, 1); /* 3 states in all state spaces */

    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Initial, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Initial, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Met, Initial, 0);
    gtaStoreDefault(Met);
    
    gtaAllocExceptions(Initial, Met, 0);
    gtaStoreDefault(Met);
    
    gtaAllocExceptions(Met, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Met, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Met, Met, 0);
    gtaStoreDefault(Met);

    if (hasMember(uP, d)) {
      /*check that we encounter an occurrence of an element in P */
      gtaAllocExceptions(Initial,Initial,1);
      gtaStoreException(Met, "1");
      gtaStoreDefault(Bad); 
    }
    else { /* outside universes in uP, there is nothing to check,
	      so we use the state Initial to denote that every
	      thing is ok; even an empty tree in such a universe
	      may be accepted, so Initial is also an accepting state */
      gtaAllocExceptions(Initial,Initial, 0);
      gtaStoreDefault(Initial); 
    }    
    gtaBuildDelta(Initial); 
  }
  free(uP);
  return gtaBuild("101"); /* Met is the only accepting state */
}


GTA *gtaEq1(int P, int Q, SSSet uP, SSSet uQ)
{ 
  if (P == Q)
    return gtaTrue();
  else {
    int var[2], d, Ok = 0, Bad = 1, Done = 2;
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(3); /* 3 states in root */
			 
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 3, 3, var, 2); /* 3 states in all state spaces */
      if ((!hasMember(uP, d)) && (!hasMember(uQ, d))) {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok); /* (Ok, Ok, anything) -> OK */
      }
      else if (hasMember(uP, d) && !hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 1);
	gtaStoreException(Ok, "0X"); /* (Ok, Ok, 0X) -> Ok */
	gtaStoreDefault(Bad); /* (Ok, Ok, x) -> Bad, if x<>0X */
      }
      else if (!hasMember(uP, d) && hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 1);
	gtaStoreException(Ok, "X0");
	gtaStoreDefault(Bad); 
      }
      else { /* d \in uP \cap uQ */
	gtaAllocExceptions(Ok, Ok, 2);
	gtaStoreException(Ok, "00");
	gtaStoreException(Done, "11");
	gtaStoreDefault(Bad); 
      }
      
      gtaAllocExceptions(Done, Bad, 0);
      gtaStoreDefault(Bad);

      gtaAllocExceptions(Bad, Done, 0);
      gtaStoreDefault(Bad);

      gtaAllocExceptions(Done, Ok, 0);
      gtaStoreDefault(Done);
  
      gtaAllocExceptions(Ok, Done, 0);
      gtaStoreDefault(Done);
      
      gtaAllocExceptions(Done, Done, 0);
      gtaStoreDefault(Bad);

      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaBuildDelta(Ok); 
    }
    free(uP);
    free(uQ);
    return gtaBuild("001");
  }
}


GTA *gtaEq2(int P, int Q, SSSet uP, SSSet uQ)
{
  if (P == Q)
    return gtaTrue();
  else {
    int var[2], d, Ok = 0, Bad = 1;
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(2); /* 2 states in root */
			 
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 2, 2, var, 2); /* 2 states in all state spaces */
      if ((!hasMember(uP, d)) && (!hasMember(uQ, d))) {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok); /* (Ok, Ok, anything) -> OK */
      }
      else if (hasMember(uP, d) && !hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 1);
	gtaStoreException(Ok, "0X"); /* (Ok, Ok, 0X) -> Ok */
	gtaStoreDefault(Bad); /* (Ok, Ok, x) -> Bad, if x<>0X */
      }
      else if (!hasMember(uP, d) && hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 1);
	gtaStoreException(Ok, "X0");
	gtaStoreDefault(Bad); 
      }
      else { /* d \in uP \cap uQ */
	gtaAllocExceptions(Ok, Ok, 2);
	gtaStoreException(Ok, "00");
	gtaStoreException(Ok, "11");
	gtaStoreDefault(Bad); 
      }
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaBuildDelta(Ok); 
    }
    free(uP);
    free(uQ);
    return gtaBuild("10");
  }
}
  
GTA *gtaTrue()
{
  int d, Ok = 0;
  int var[1];

  gtaSetup(1);
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 1, 1, var, 0); /* 1 state in all state spaces */

    gtaAllocExceptions(Ok, Ok, 0);
    gtaStoreDefault(Ok); /* (Ok, Ok) -> OK */

    gtaBuildDelta(Ok); 
  }
  
  return gtaBuild("1");
}

GTA *gtaFalse()
{
  int d, Ok = 0;
  int var[1];

  gtaSetup(1);
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 1, 1, var, 0); /* 1 state in all state spaces */

    gtaAllocExceptions(Ok, Ok, 0);
    gtaStoreDefault(Ok); /* (Ok, Ok) -> OK */

    gtaBuildDelta(Ok); 
  }
  
  return gtaBuild("0");
}
 
GTA *gtaSingleton(int P, SSSet uP)
{
  int var[1], d, Bad = 0, Notmet = 1, Met=2;
  /* Met in hat transition is accepting */

  var[0] = P;
  
  gtaSetup(3); /* 3 states in root */
  
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 3, 3, var, 1); /* 3 states in all state spaces */

    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Notmet, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Met, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Notmet, Bad, 0);
    gtaStoreDefault(Bad);
    
    if (hasMember(uP, d)) {
      gtaAllocExceptions(Notmet, Notmet, 1);
      gtaStoreException(Met, "1"); /* (Notmet, Notmet, H) -> Met */
      gtaStoreDefault(Notmet); 
    
      gtaAllocExceptions(Notmet, Met, 1);
      gtaStoreException(Bad, "1"); /* (Notmet, Met, H) -> Bad (unique pos.) */
      gtaStoreDefault(Met);

      gtaAllocExceptions(Met, Notmet, 1);
      gtaStoreException(Bad, "1"); /* (Notmet, Met, H) -> Bad (unique pos.) */
      gtaStoreDefault(Met); 
    }
    else {
      gtaAllocExceptions(Notmet, Notmet, 0);
      gtaStoreDefault(Notmet); 

      gtaAllocExceptions(Notmet, Met, 0);
      gtaStoreDefault(Met); 

      gtaAllocExceptions(Met, Notmet, 0);
      gtaStoreDefault(Met); 
    }      
    
    gtaAllocExceptions(Met, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Met, Met, 0);
    gtaStoreDefault(Bad);
    
    gtaBuildDelta(Notmet); 
  }
  free(uP);
  return gtaBuild("001"); /* Met is the only accepting state */
}

/* We assume that P and Q are defined for the same universes */
GTA *gtaDot1(int P, int Q, SSSet uPQ)
{
  if (P == Q)
    return gtaFalse();
  else {
    int var[2], d, NoP = 0, Qtobemet = 1, Bad=2;
    /* Met in hat transition is accepting */
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(3); /* 4 states in root */
    
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 3, 3, var, 2); /* 3 states in all state spaces */
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Qtobemet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, NoP, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qtobemet, Bad, 0);
      gtaStoreDefault(Bad);
      
      
      gtaAllocExceptions(NoP, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qtobemet, NoP, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qtobemet, Qtobemet, 0);
      gtaStoreDefault(Bad);

      
      if (hasMember(uPQ, d)) {
	gtaAllocExceptions(NoP, NoP, 2);
	gtaStoreException(Qtobemet, "10"); /* (NoP, NoP, 10) -> Qtobemet */
	gtaStoreException(NoP, "00"); /* (NoP, NoP, 00) -> NoP */
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(NoP, Qtobemet, 2);
	gtaStoreException(NoP, "01");
	gtaStoreException(Qtobemet, "11");
	gtaStoreDefault(Bad);
 
      }
      else { /* d not member of uPQ */
	gtaAllocExceptions(NoP, NoP, 0);
	gtaStoreDefault(NoP); 
	
	gtaAllocExceptions(NoP, Qtobemet, 0);
	gtaStoreDefault(Bad);
      }
      
      gtaBuildDelta(NoP); 
    }
    free(uPQ);
    return gtaBuild("100"); /* NoP is the only accepting state */
  }
}

/* We assume that P and Q are defined for the same universes */
GTA *gtaDot0(int P, int Q, SSSet uPQ)
{
  if (P == Q)
    return gtaFalse();
  else {
    int var[2], d, NoP = 0, Qtobemet = 1, Bad=2;
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(3); /* Three states in root */
    
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 3, 3, var, 2); /* 3 states in all state spaces */
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(NoP, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qtobemet, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Qtobemet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, NoP, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(NoP, Qtobemet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qtobemet, Qtobemet, 0);
      gtaStoreDefault(Bad);
      
      if (hasMember(uPQ, d)) {
	gtaAllocExceptions(NoP, NoP, 2);
	gtaStoreException(Qtobemet, "10"); /* (NoP, NoP, 10) -> Qtobemet */
	gtaStoreException(NoP, "00"); /* (NoP, NoP, 00) -> NoP */
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(Qtobemet, NoP, 2);
	gtaStoreException(NoP, "01");
	gtaStoreException(Qtobemet, "11");
	gtaStoreDefault(Bad);
      }
      else { /* d not member of uPQ */
	gtaAllocExceptions(NoP, NoP, 0);
	gtaStoreDefault(NoP); 
	
	gtaAllocExceptions(Qtobemet, NoP, 0);
	gtaStoreDefault(Bad);
      }
      
      gtaBuildDelta(NoP); 
    }
    free(uPQ);
    return gtaBuild("100"); /* NoP is the only accepting state */
  }
}

/* We assume P and Q are defined for the same universes */
GTA *gtaUp(int P, int Q, SSSet uPQ)
{
  /* Calculate the automaton that checks that the positions in P are 
     those of Q moved one step upwards (towards the root).  Only 
     positions is universes uPQ are considered; also recall that a 
     root (of one of the universes in uPQ) is a position p in the 
     complete, binary tree such that if d is the state ID of p and d' 
     is the state ID of the parent of p then uPQ.has_member(p) and 
     !uPQ.has_member (p'). In case, there is only one universe, 
     the root is the root of the infinite, binary tree. 
     
     We use state Ok to denote that so far (below), everything is all 
     right and (if we are in a universe, then we didn't just (right 
     below) see an element in Q).  State Qmet is similar, but it 
     denotes that we just met an element in Q.  State PQmet denotes 
     that we met an element in Q below, but also one in P that is not 
     a position in Q moved upwards; in this situation, we expect that 
     the position below is a root. */

  if (P == Q)
    return gtaRoot(P, uPQ, uPQ);
  else {
    int var[2], d, Ok = 0, Bad = 1, Qmet=2, PQmet=3;
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(4); 
    
    for (d = 0; d < guide.numSs; d++) {

      gtaSetupDelta(d, 4, 4, var, 2); 
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qmet, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(PQmet, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Qmet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, PQmet, 0);
      gtaStoreDefault(Bad); 
      
      gtaAllocExceptions(PQmet, Qmet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Qmet, PQmet, 0);
      gtaStoreDefault(Bad);
    
      /* 
	 at this point, the following is defined (as bad): 
	 ----- OK  Bad Qmet PQmet      
	 OK         *   
	 Bad   *    *   *     *
	 Qmet       *         *
	 PQmet      *   *
	 
	 so, we need to fill out the remaining seven entries
	 
	 */
      
      if (hasMember(uPQ, d)) {
	
	gtaAllocExceptions(Ok, Ok, 3);
	gtaStoreException(PQmet, "11");
	gtaStoreException(Qmet, "01");
	gtaStoreException(Ok, "00");
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(Ok, Qmet, 2);
	gtaStoreException(Qmet, "11");
	gtaStoreException(Ok, "10");
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(Qmet, Ok, 2);
	gtaStoreException(Qmet, "11");
	gtaStoreException(Ok, "10");
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(Qmet, Qmet, 2);
	gtaStoreException(Qmet, "11");
	gtaStoreException(Ok, "10");
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(PQmet, Ok, 0);
	gtaStoreDefault(Bad); /* left child wasn't root after all */ 
	
	gtaAllocExceptions(Ok, PQmet, 0);
	gtaStoreDefault(Bad);  /* right child wasn't root after all */ 
	
	gtaAllocExceptions(PQmet, PQmet, 0);
	gtaStoreDefault(Bad); /* children are not roots */
	
      }
      else { /* d not member of uPQ */
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok);
	
	gtaAllocExceptions(Ok, Qmet, 0);
	gtaStoreDefault(Bad);   /*right child is a root, but we didn't see that position
			    in P */
	
	gtaAllocExceptions(Qmet, Ok, 0);
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(Qmet, Qmet, 0);
	gtaStoreDefault(Bad);
     
	gtaAllocExceptions(PQmet, Ok, 0);
	gtaStoreDefault(Ok);  /* left child is a root (and so is possibly the right child) */
	
	gtaAllocExceptions(Ok, PQmet, 0);
	gtaStoreDefault(Ok);  
	
	gtaAllocExceptions(PQmet, PQmet, 0);
	gtaStoreDefault(Ok); 
	
      }
      gtaBuildDelta(Ok); 
    }
    free(uPQ);
    return gtaBuild("1001"); /* Ok and PQmet are accepting states; the latter
			       to account for situation where there is only
			       one universe */
  }
}

GTA *gtaEmpty(int P, SSSet uP) {
  int var[1], d, Ok = 0, Bad = 1;

  var[0] = P;
  
  gtaSetup(2); /* Two states in root */
  
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 2, 2, var, 1); /* 2 states in all state spaces */

    gtaAllocExceptions(Ok, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Bad, Ok, 0);
    gtaStoreDefault(Bad);

    if (hasMember(uP, d)) {
      
      gtaAllocExceptions(Ok, Ok, 1);
      gtaStoreException(Bad, "1");
      gtaStoreDefault(Ok); 
    }
    else { /* d not member of uP */
      gtaAllocExceptions(Ok, Ok, 0);
      gtaStoreDefault(Ok);
    }
    
    gtaBuildDelta(Ok); 
  }
  free(uP);
  return gtaBuild("10"); 
}

GTA *gtaSub(int P, int Q, SSSet uP, SSSet uQ)
{
  if (P == Q)
    return gtaTrue();
  else{
    int var[2], d, Ok = 0, Bad = 1;
    
    var[0] = P;
    var[1] = Q;
    
    gtaSetup(2); /* Two states in root */
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 2, 2, var, 2); /* 2 states in all state spaces */
      if ((!hasMember(uP, d)) && (!hasMember(uQ, d))) {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok); /* (Ok, Ok, anything) -> OK */
      }
      else if (hasMember(uP, d) && !hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 1);
	gtaStoreException(Ok, "0X"); /* (Ok, Ok, 0X) -> Ok */
	gtaStoreDefault(Bad); /* (Ok, Ok, x) -> Bad, if x<>0X */
      }
      else if (!hasMember(uP, d) && hasMember(uQ, d)) {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok); 
      }
      else { /* d \in uP \cap uQ */
	gtaAllocExceptions(Ok, Ok, 2);
	gtaStoreException(Ok, "X1");
	gtaStoreException(Ok, "00");
	gtaStoreDefault(Bad); 
      }
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaBuildDelta(Ok); 
    }
    free(uP);
    free(uQ);
    return gtaBuild("10");
  }
}

/** all p in P: p is a root of one of the universes U */
GTA *gtaRoot(int P, SSSet uP, SSSet U) {

  int var[1], d, Ok = 0, Bad = 1, Pmet=2, notPmet=3;

  var[0] = P;
  
  gtaSetup(4); 
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 4, 4, var, 1); /* 4 states in all state spaces */

    gtaAllocExceptions(Ok, Bad, 0);
    gtaStoreDefault(Bad);
	
    gtaAllocExceptions(Bad, Ok, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(notPmet, Bad, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Bad, notPmet, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Pmet, Bad, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Bad, Pmet, 0);
    gtaStoreDefault(Bad);

    if (hasMember(uP, d)) {
      if (hasMember(U, d)) {
	gtaAllocExceptions(notPmet, notPmet, 1);
	gtaStoreException(Pmet, "1"); /* we are, presumable, at the root
				      of a universe in U here */
	gtaStoreDefault(notPmet);
      
	gtaAllocExceptions(Pmet, Ok, 0); /* Ok doesn't make sense here */
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(Pmet, notPmet, 0); /* we met something in P below a root */ 	
	gtaStoreDefault(Bad);
      
	gtaAllocExceptions(Ok, Pmet, 0);
	gtaStoreDefault(Bad);
     
        gtaAllocExceptions(notPmet, Pmet, 0); /* we met something in P below a root */
	gtaStoreDefault(Bad);
      
	gtaAllocExceptions(Pmet, Pmet, 0); /* we met something in P below a root */
	gtaStoreDefault(Bad);
    
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Bad);
    
	gtaAllocExceptions(Ok, notPmet, 0);
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(notPmet, Ok, 0);
	gtaStoreDefault(Bad); 
      } 
      else {
	gtaAllocExceptions(Ok, Ok, 0); 
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(Pmet, Ok, 0);
	gtaStoreDefault(Bad);
      
	gtaAllocExceptions(Ok, Pmet, 0);
	gtaStoreDefault(Bad);
      
	gtaAllocExceptions(Pmet, Pmet, 0);
	gtaStoreDefault(Bad);

	gtaAllocExceptions(Ok, notPmet, 0);
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(notPmet, Ok, 0);
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(Pmet, notPmet, 0);
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(notPmet, Pmet, 0);
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(notPmet, notPmet, 1);
	gtaStoreException(Bad, "1"); /* we should't meet anything in P outside U */ 
	gtaStoreDefault(notPmet);
     
      }
    }
    else { 
      /* d is not member of uP; in particular this is the transition
	 function used in the hat */

      gtaAllocExceptions(Ok, Ok, 0);
      gtaStoreDefault(Ok);
      
      gtaAllocExceptions(Pmet, Ok, 0);
      gtaStoreDefault(Ok); /* we are in the hat, and the left child is in
			P and is the root of a universe in uP
			mentioned in U */
      
      gtaAllocExceptions(Ok, Pmet, 0); 
      gtaStoreDefault(Ok);
      
      gtaAllocExceptions(Pmet, Pmet, 0); /* both children are roots mentioned in U */
      gtaStoreDefault(Ok);

      gtaAllocExceptions(Ok, notPmet, 0);
      gtaStoreDefault(Ok);
	
      gtaAllocExceptions(notPmet, Ok, 0);
      gtaStoreDefault(Ok);
	
      gtaAllocExceptions(Pmet, notPmet, 0); /* both children are roots mentioned in U */
      gtaStoreDefault(Ok);
	
      gtaAllocExceptions(notPmet, Pmet, 0); /* both children are roots mentioned in U */
      gtaStoreDefault(Ok);
	
      gtaAllocExceptions(notPmet, notPmet, 0); /* both children are roots mentioned in U */
      gtaStoreDefault(Ok);    
    }
        
    gtaBuildDelta(notPmet); 
  }
  free(uP);
  free(U);
  return gtaBuild("1011"); /* Pmet, notPmet, and Ok are accepting */
}

/* Use Single(p) and Single(q) to ensure correct first-order p and q */
GTA *gtaLess(int p, int q, SSSet up, SSSet uq)
{
  if (p == q)
    return gtaFalse();
  else {
    int var[2], d, Ok = 0, qmet = 1, Bad=2;
    
    var[0] = p;
    var[1] = q;
    
    gtaSetup(3); /* Two states in root */
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 3, 3, var, 2); /* 3 states in all state spaces */
      
      gtaAllocExceptions(Bad, qmet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(qmet, qmet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(qmet, Bad, 0);
      gtaStoreDefault(Bad);
      
      if (hasMember(up, d) && hasMember(uq, d)) {
	gtaAllocExceptions(Ok, Ok, 2);
	gtaStoreException(qmet, "01");
	gtaStoreException(Bad, "1X");
	gtaStoreDefault(Ok);
	
	gtaAllocExceptions(Ok, qmet, 2);
	gtaStoreException(Ok, "10");
	gtaStoreException(qmet, "00");
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(qmet, Ok, 2);
	gtaStoreException(Ok, "10");
	gtaStoreException(qmet, "00");
	gtaStoreDefault(Bad);
      }
      else {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok);
	
	gtaAllocExceptions(qmet, Ok, 0);
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(Ok, qmet, 0);
	gtaStoreDefault(Bad); 
      }
      
      gtaBuildDelta(Ok); 
    }
    free(up);
    free(uq);
    return gtaBuild("100");
  }
}


/* Use Single(p) and Single(q) to ensure correct first-order p and q */
GTA *gtaLesseq(int p, int q, SSSet up, SSSet uq)
{
  if (p == q)
    return gtaTrue();
  else {
    int var[2], d, Ok = 0, qmet = 1, Bad=2;
    
    var[0] = p;
    var[1] = q;
    
    gtaSetup(3); /* Two states in root */
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 3, 3, var, 2); /* 3 states in all state spaces */
      
      gtaAllocExceptions(Bad, qmet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(qmet, qmet, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(qmet, Bad, 0);
      gtaStoreDefault(Bad);
      
      if (hasMember(up, d) && hasMember(uq, d)) {
	gtaAllocExceptions(Ok, Ok, 2);/************** changed from 3 to 2 ********/
	gtaStoreException(qmet, "01");
	gtaStoreException(Bad, "10");
	gtaStoreDefault(Ok); /* Goes to OK for "11", the only diff. betw. less & less_eq */
	
	gtaAllocExceptions(Ok, qmet, 2);
	gtaStoreException(Ok, "10");
	gtaStoreException(qmet, "00");
	gtaStoreDefault(Bad);
	
	gtaAllocExceptions(qmet, Ok, 2);
	gtaStoreException(Ok, "10");
	gtaStoreException(qmet, "00");
	gtaStoreDefault(Bad);
      }
      else {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok);
	
	gtaAllocExceptions(qmet, Ok, 0);
	gtaStoreDefault(Bad); 
	
	gtaAllocExceptions(Ok, qmet, 0);
	gtaStoreDefault(Bad); 
      }

      gtaBuildDelta(Ok); 
    }
    free(up);
    free(uq);
    return gtaBuild("100");
  }
}

GTA *gtaBoolvar(int alpha)
{
  int d, Ok = 0, Bad = 1;
  int var[1];
  var[0] = alpha;

  gtaSetup(2); /* Two states in root */
  
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 2, 2, var, 1); /* 2 states in all state spaces */

    if (d == 0) {
      gtaAllocExceptions(Ok, Ok, 1);
      gtaStoreException(Bad, "0");
      gtaStoreDefault(Ok); 
    } 
    else {
      gtaAllocExceptions(Ok, Ok, 0);
      gtaStoreDefault(Ok); 
    }

    gtaAllocExceptions(Ok, Bad, 0);
    gtaStoreDefault(Bad);
	
    gtaAllocExceptions(Bad, Ok, 0);
    gtaStoreDefault(Bad);
    
    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);
    
    gtaBuildDelta(Ok); 
  }
  
  return gtaBuild("10");
}

/* We assume common universes again */
GTA *gtaUnion(int P, int Q, int R, SSSet uPQR)
{
  if (P == Q) 
    return gtaSub(R, P, uPQR, uPQR);
  else if (P == R) 
    return gtaSub(Q, R, uPQR, uPQR);
  else if (Q == R)
    return gtaEq2(P, Q, uPQR, uPQR);
  else {
    int var[3], d, Ok = 0, Bad = 1;
    
    var[0] = P;
    var[1] = Q;
    var[2] = R;
    
    gtaSetup(2); /* Two states in root */
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 2, 2, var, 3); /* 2 states in all state spaces */
      if (hasMember(uPQR, d)) {
	gtaAllocExceptions(Ok, Ok, 3);
	gtaStoreException(Ok, "1X1");
	gtaStoreException(Ok, "110");
	gtaStoreException(Ok, "000");
	gtaStoreDefault(Bad);
      }
      else {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok);
      }
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaBuildDelta(Ok); 
    }
    free(uPQR);
    return gtaBuild("10");
  }
}

/* We assume common universes again */
GTA *gtaInter(int P, int Q, int R, SSSet uPQR)
{
  if (P == Q) 
    return gtaSub(P, R, uPQR, uPQR);
  else if (P == R)
    return gtaSub(P, Q, uPQR, uPQR);
  else if (Q == R)
    return gtaEq2(P, Q, uPQR, uPQR);
  else {
    int var[3], d, Ok = 0, Bad = 1;
    
    var[0] = P;
    var[1] = Q;
    var[2] = R;
  
    gtaSetup(2); /* Two states in root */
    for (d = 0; d < guide.numSs; d++) {
      gtaSetupDelta(d, 2, 2, var, 3); /* 2 states in all state spaces */
      if (hasMember(uPQR, d)) {
	gtaAllocExceptions(Ok, Ok, 3);
	gtaStoreException(Ok, "111");
	gtaStoreException(Ok, "00X");
	gtaStoreException(Ok, "010");
	gtaStoreDefault(Bad);
      }
      else {
	gtaAllocExceptions(Ok, Ok, 0);
	gtaStoreDefault(Ok);
      }
      
      gtaAllocExceptions(Ok, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Ok, 0);
      gtaStoreDefault(Bad);
      
      gtaAllocExceptions(Bad, Bad, 0);
      gtaStoreDefault(Bad);
      
      gtaBuildDelta(Ok); 
    }
    free(uPQR);
    return gtaBuild("10");
  }  
}

/* We assume common universes again */
GTA *gtaSetminus(int P, int Q, int R, SSSet uPQR)
{
  if (P == R) {
    if (P == Q)
      return gtaEmpty(P, uPQR);
    else {
      /* P == R and P != Q, so P = Q \ R means that P and Q are empty */
      int var[2], d, Ok = 0, Bad = 1;
      var[0] = P;
      var[1] = R;
      
      gtaSetup(2); 
      for (d = 0; d < guide.numSs; d++) {
	gtaSetupDelta(d, 2, 2, var, 3); 
	if (hasMember(uPQR, d)) {
 	  gtaAllocExceptions(Ok, Ok, 2);
 	  gtaStoreException(Ok, "00");
 	  gtaStoreException(Ok, "00");
 	  gtaStoreDefault(Bad);
	}
	else {
	  gtaAllocExceptions(Ok, Ok, 0);
	  gtaStoreDefault(Ok);
	}
	
	gtaAllocExceptions(Ok, Bad, 0);
   	gtaStoreDefault(Bad);
	
   	gtaAllocExceptions(Bad, Ok, 0);
   	gtaStoreDefault(Bad);
	
   	gtaAllocExceptions(Bad, Bad, 0);
   	gtaStoreDefault(Bad);
	
   	gtaBuildDelta(Ok); 
      }
      free(uPQR);
      return gtaBuild("10");
    }
  }
  else
    
    /* P != R */

    if (Q == R)
      return gtaEmpty(P, uPQR);
    else {
      if (P == Q) { /* P inter R = empty */
	int var[2], d, Ok = 0, Bad = 1;
	
	var[0] = P;
	var[2] = R;
      
	gtaSetup(2); 
	for (d = 0; d < guide.numSs; d++) {
	  gtaSetupDelta(d, 2, 2, var, 2); /* 2 states in all state spaces */
	  if (hasMember(uPQR, d)) {
	    gtaAllocExceptions(Ok, Ok, 1);
	    gtaStoreException(Bad, "11");
	    gtaStoreDefault(Ok);
	  }
	  else {
	    gtaAllocExceptions(Ok, Ok, 0);
	    gtaStoreDefault(Ok);
	  }
	
	  gtaAllocExceptions(Ok, Bad, 0);
	  gtaStoreDefault(Bad);
	
	  gtaAllocExceptions(Bad, Ok, 0);
	  gtaStoreDefault(Bad);
	
	  gtaAllocExceptions(Bad, Bad, 0);
	  gtaStoreDefault(Bad);
	
	  gtaBuildDelta(Ok); 
	}
	free(uPQR);
	return gtaBuild("10");
      } else { /* P, Q, R: they are all different */
	  int var[3], d, Ok = 0, Bad = 1;
      
	  var[0] = P;
	  var[1] = Q;
	  var[2] = R;
      
	  gtaSetup(2); 
	  for (d = 0; d < guide.numSs; d++) {
	    gtaSetupDelta(d, 2, 2, var, 3); /* 2 states in all state spaces */
	    if (hasMember(uPQR, d)) {
	      gtaAllocExceptions(Ok, Ok, 3);
	      gtaStoreException(Ok, "110");
	      gtaStoreException(Ok, "00X");
	      gtaStoreException(Ok, "011");
	      gtaStoreDefault(Bad);
	    }
	    else {
	      gtaAllocExceptions(Ok, Ok, 0);
	      gtaStoreDefault(Ok);
	    }
	
	    gtaAllocExceptions(Ok, Bad, 0);
	    gtaStoreDefault(Bad);
	
	    gtaAllocExceptions(Bad, Ok, 0);
	    gtaStoreDefault(Bad);
	
	    gtaAllocExceptions(Bad, Bad, 0);
	    gtaStoreDefault(Bad);
	
	    gtaBuildDelta(Ok); 
	  }
	  free(uPQR);
	  return gtaBuild("10");
	}
    }
}

GTA *gtaInStateSpace(int P, SSSet ss, SSSet uP)
{ 
  int var[1], d, Ok = 0, Bad = 1, Done = 2;
    
  var[0] = P;
    
  gtaSetup(3); /* 3 states in root */
  for (d = 0; d < guide.numSs; d++) {
    gtaSetupDelta(d, 3, 3, var, 1); /* 3 states in all state spaces */
    if (!hasMember(uP, d)) {
      gtaAllocExceptions(Ok, Ok, 0);
      gtaStoreDefault(Ok); /* (Ok, Ok, anything) -> OK */
    }
    else if (hasMember(uP, d) && !hasMember(ss, d)) {
      gtaAllocExceptions(Ok, Ok, 1);
      gtaStoreException(Ok, "0"); /* (Ok, Ok, L) -> Ok */
      gtaStoreDefault(Bad); /* (Ok, Ok, x) -> Bad, if x<>L */
    }
    else { /* d \in uP and d \in ss */
      gtaAllocExceptions(Ok, Ok, 2);
      gtaStoreException(Ok, "0");
      gtaStoreException(Done, "1");
      gtaStoreDefault(Bad); 
    }
      
    gtaAllocExceptions(Done, Bad, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Bad, Done, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Done, Ok, 0);
    gtaStoreDefault(Done);
  
    gtaAllocExceptions(Ok, Done, 0);
    gtaStoreDefault(Done);
      
    gtaAllocExceptions(Done, Done, 0);
    gtaStoreDefault(Bad);

    gtaAllocExceptions(Ok, Bad, 0);
    gtaStoreDefault(Bad);
      
    gtaAllocExceptions(Bad, Ok, 0);
    gtaStoreDefault(Bad);
      
    gtaAllocExceptions(Bad, Bad, 0);
    gtaStoreDefault(Bad);
      
    gtaBuildDelta(Ok); 
  }
  free(uP);
  free(ss);
  return gtaBuild("001");
}



/* basic.c */

/*
 * MONA is Copyright (C) 1997 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#include <stdlib.h>
#include "dfa.h"

DFA *dfaTrue()
{
  dfaSetup(2, 0, NULL);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);

  /* state 1 */
  dfaAllocExceptions(0);
  dfaStoreState(1);

  return dfaBuild("0+");
}

DFA *dfaFalse()
{
  dfaSetup(2, 0, NULL);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);

  /* state 1 */
  dfaAllocExceptions(0);
  dfaStoreState(1);

  return dfaBuild("0-");
}

DFA *dfaSingleton(int i)
{
  int var_index[1];
  var_index[0] = i;
  
  dfaSetup(4, 1, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);

  /* state 1 */
  dfaAllocExceptions(1);
  dfaStoreException(2, "1");
  dfaStoreState(1);

  /* state 2 */
  dfaAllocExceptions(1);
  dfaStoreException(3, "1");
  dfaStoreState(2);

  /* state 3 */
  dfaAllocExceptions(0);
  dfaStoreState(3);

  return dfaBuild("0-+-");
}

DFA *dfaEq2(int i, int j)
{
  if (i == j) 
    return dfaTrue();
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(3, 2, var_index);
    
    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);

    /* state 1 */
    dfaAllocExceptions(2);
    dfaStoreException(1, "00");
    dfaStoreException(1, "11");
    dfaStoreState(2);

    /* state 2 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    return dfaBuild("0+-");
  }
}

DFA *dfaSubset(int i, int j)
{
  if (i == j) 
    return dfaTrue();
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(3, 2, var_index);
    
    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);

    /* state 1 */
    dfaAllocExceptions(1);
    dfaStoreException(2, "10");
    dfaStoreState(1);

    /* state 2 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    return dfaBuild("0+-");
  }
}

DFA *dfaEmpty(int i)
{
  int var_index[1];
  var_index[0] = i;

  dfaSetup(3, 1, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  
  /* state 1 */
  dfaAllocExceptions(1);
  dfaStoreException(1, "0");
  dfaStoreState(2);
    
  /* state 2 */
  dfaAllocExceptions(0);
  dfaStoreState(2);

  return dfaBuild("0+-");
}

DFA *dfaPlus2(int i, int j)
{
  if (i == j)
    return dfaEmpty(i); /* Pi = Pi+1 iff Pi = empty */
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(4, 2, var_index);
    
    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(2);
    dfaStoreException(1, "00");
    dfaStoreException(2, "01");
    dfaStoreState(3);
    
    /* state 2 */
    dfaAllocExceptions(2);
    dfaStoreException(3, "0X");
    dfaStoreException(1, "10");
    dfaStoreState(2);
    
    /* state 3 */
    dfaAllocExceptions(0);
    dfaStoreState(3);
    
    return dfaBuild("0+--");
  }    
}

DFA *dfaMinus2(int i, int j)
{
  if (i == j) {
    int var_index[1];
    var_index[0] = i;

    dfaSetup(4, 1, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    /* state 2 */
    dfaAllocExceptions(1);
    dfaStoreException(2, "0");
    dfaStoreState(3);
    
    /* state 3 */
    dfaAllocExceptions(0);
    dfaStoreState(3);
    
    return dfaBuild("0++-");
  }
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;    

    dfaSetup(6, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(3, "00");
    dfaStoreException(4, "10");
    dfaStoreException(2, "11");
    dfaStoreState(5);
        
    /* state 2 */
    dfaAllocExceptions(1);
    dfaStoreException(4, "1X");
    dfaStoreState(3);
        
    /* state 3 */
    dfaAllocExceptions(2);
    dfaStoreException(3, "00");
    dfaStoreException(4, "10");
    dfaStoreState(5);
    
    /* state 4 */
    dfaAllocExceptions(2);
    dfaStoreException(4, "11");
    dfaStoreException(3, "01");
    dfaStoreState(5);
            
    /* state 5 */
    dfaAllocExceptions(0);
    dfaStoreState(5);
    
    return dfaBuild("0+++--");
  }
}

DFA *dfaUnion(int i, int j, int k)
{
  if (i == j)
    return dfaSubset(k, i);
  else if (i == k)
    return dfaSubset(j, i);
  else if (j == k)
    return dfaEq2(i, j);
  else {
    int var_index[3];
    var_index[0] = i;
    var_index[1] = j;
    var_index[2] = k;

    dfaSetup(3, 3, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(1, "000");
    dfaStoreException(1, "1X1");
    dfaStoreException(1, "110");
    dfaStoreState(2);
    
    /* state 2 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    return dfaBuild("0+-");
  }
}

DFA *dfaInter(int i, int j, int k)
{
  if (i == j)
    return dfaSubset(i, k);
  else if (i == k)
    return dfaSubset(i, j);
  else if (j == k)
    return dfaEq2(i, j);
  else {
    int var_index[3];
    var_index[0] = i;
    var_index[1] = j;
    var_index[2] = k;

    dfaSetup(3, 3, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(1, "111");
    dfaStoreException(1, "00X");
    dfaStoreException(1, "010");
    dfaStoreState(2);
    
    /* state 2 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    return dfaBuild("0+-");
  }
}

DFA *dfaSetminus(int i, int j, int k)
{
  if (j == k || i == k)
    return dfaEmpty(i);
  else if (i == j) {    /* make: k inter i = empty */
    int var_index[2];
    var_index[0] = i;
    var_index[1] = k;

    dfaSetup(3, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(2);
    dfaStoreException(1,"0X");
    dfaStoreException(1,"10");
    dfaStoreState(2);

    /* state 2 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    return dfaBuild("0+-");
  }
  else {
    int var_index[3];
    var_index[0] = i;
    var_index[1] = j;
    var_index[2] = k;

    dfaSetup(3, 3, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(1, "00X");
    dfaStoreException(1, "110");
    dfaStoreException(1, "011");
    dfaStoreState(2);
    
    /* state 2 */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    return dfaBuild("0+-");
  }
}

DFA *dfaBoolvar(int b)
{
  int var_index[1];
  var_index[0] = b;

  dfaSetup(3, 1, var_index);

  /* boolvar */
  dfaAllocExceptions(1);
  dfaStoreException(2, "0");
  dfaStoreState(1);      

  /* state 1 */
  dfaAllocExceptions(0);
  dfaStoreState(1);      

  /* state 2 */
  dfaAllocExceptions(0);
  dfaStoreState(2);      

  return dfaBuild("0+-");
}

DFA *dfaFirstOrder(int i)
{
  int var_index[1];
  var_index[0] = i;

  dfaSetup(3, 1, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  
  /* state 1 */
  dfaAllocExceptions(1);
  dfaStoreException(1, "0");
  dfaStoreState(2);
  
  /* state 2 */
  dfaAllocExceptions(0);
  dfaStoreState(2);
  
  return dfaBuild("0-+");
}

/* an automaton that expresses a non-WS1S property: it accepts iff it
   reads an empty string or if first-order variable i assumes the
   value n-1, where n is the length of the string */
DFA *dfaLast(int i)
{
  int var_index[1];
  var_index[0] = i;

  dfaSetup(5, 1, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(4);

  /* state 1, rejecting */
  dfaAllocExceptions(1);
  dfaStoreException(1, "0");
  dfaStoreState(2); /* on "1" go to transitory accepting state */
  
  /* state 2, accepting */
  dfaAllocExceptions(0);
  dfaStoreState(3);
    
  /* state 3, rejecting */
  dfaAllocExceptions(0);
  dfaStoreState(3);

  /* state 4, accepting so as to accept word that describes only
     Booleans (the empty word when Boolean part is removed) */
  dfaAllocExceptions(1);
  dfaStoreException(1, "0");
  dfaStoreState(2); /* on "1" go to transitory accepting state */
    
  return dfaBuild("0-+-+");
}

/* assume i != j */
DFA *dfaIn(int i, int j)
{
  int var_index[2];
  var_index[0] = i;
  var_index[1] = j;

  dfaSetup(4, 2, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  
  /* state 1 */
  dfaAllocExceptions(2);
  dfaStoreException(3, "10");
  dfaStoreException(2, "11");
  dfaStoreState(1);
  
  /* state 2 accept */
  dfaAllocExceptions(0);
  dfaStoreState(2);
  
  /* state 3 reject */
  dfaAllocExceptions(0);
  dfaStoreState(3);
    
  return dfaBuild("0-+-");
}

DFA *dfaEq1(int i, int j)
{
  if (i == j) 
    return dfaTrue();
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(4, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
  
    /* state 1 */
    dfaAllocExceptions(2);
    dfaStoreException(1, "00");
    dfaStoreException(2, "11");
    dfaStoreState(3);
  
    /* state 2 accept */
    dfaAllocExceptions(0);
    dfaStoreState(2);
  
    /* state 3 reject */
    dfaAllocExceptions(0);
    dfaStoreState(3);
    
    return dfaBuild("0-+-");
  }
}

DFA *dfaLess(int i, int j)
{
  if (i == j) 
    return dfaFalse();
  else {
    int var_index[2];
    var_index[0] = i,
    var_index[1] = j;

    dfaSetup(5, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(2);
    dfaStoreException(1, "00");
    dfaStoreException(2, "10");
    dfaStoreState(3);
    
    /* state 2 */
    dfaAllocExceptions(1);
    dfaStoreException(2, "X0");
    dfaStoreState(4);

    /* state 3 */
    dfaAllocExceptions(0);
    dfaStoreState(3);

    /* state 4 */
    dfaAllocExceptions(0);
    dfaStoreState(4);

    return dfaBuild("0---+");
  }
}

DFA *dfaLesseq(int i, int j)
{
  if (i == j) 
    return dfaTrue();
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(5, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(1, "00");
    dfaStoreException(2, "10");
    dfaStoreException(4, "11");
    dfaStoreState(3);
    
    /* state 2 */
    dfaAllocExceptions(1);
    dfaStoreException(2, "X0");
    dfaStoreState(4);

    /* state 3 */
    dfaAllocExceptions(0);
    dfaStoreState(3);

    /* state 4 */
    dfaAllocExceptions(0);
    dfaStoreState(4);

    return dfaBuild("0---+");
  }
}

DFA *dfaConst(int n, int i)
{
  DFA *aut;
  int var_index[1];
  int state_no;
  char *finals;
  
  var_index[0] = i;
  
  dfaSetup(n+4, 1, var_index);
  
  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(3);
  
  /* state 1  - Accept */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  
  /* state 2 - Reject */
  dfaAllocExceptions(0);
  dfaStoreState(2);
  
  /* states 3 .. (n+2) */
  for (state_no = 3; state_no < n+3; state_no++) {
    dfaAllocExceptions(1);
    dfaStoreException(state_no+1, "0");
    dfaStoreState(2);
  }
  
  /* state n+3 */
  dfaAllocExceptions(1);
  dfaStoreException(1, "1");
  dfaStoreState(2);
  
  {
    int k;
    
    finals = (char *) malloc ((n+4) * (sizeof(char *)));
    for (k = 0; k < n+4; k++)
      finals[k] = '-';
    finals[0] = '0';
    finals[1] = '+';
  }
  
  aut = dfaBuild(finals);
  free(finals);  
  return aut;
}

DFA *dfaPlus1(int i, int j, int n)
{
  if (n == 0)
    return dfaEq1(i, j);
  else if (i == j)
    return dfaFalse();  
  else {
    DFA *aut;
    int var_index[2];
    int state_no;
    char *finals;

    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(n+4, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);

    /* state 1 */
    dfaAllocExceptions(2);
    dfaStoreException(1, "00");
    dfaStoreException(3, "01");
    dfaStoreState(2);

    /* state 2 - Reject */
    dfaAllocExceptions(0);
    dfaStoreState(2);

    /* state 3 .. (n+1) */
    for (state_no = 3; state_no <= n+1; state_no++) {
      dfaAllocExceptions(1);
      dfaStoreException(state_no+1,"0X");
      dfaStoreState(2);
    }
    
    /* state n+2 */
    dfaAllocExceptions(1);
    dfaStoreException(n+3, "1X");
    dfaStoreState(2);

    /* state n+3 - Accept */
    dfaAllocExceptions(0);
    dfaStoreState(n+3);

    {
      int k;

      finals = (char *) malloc((n+4) * (sizeof(char *)));
      for (k = 0; k < n+4; k++)
	finals[k] = '-';
      finals[0] = '0';
      finals[n+3] = '+';
    }

    aut = dfaBuild(finals);
    free(finals);
    return aut;
  }
}

DFA *dfaMinus1(int i, int j, int n)
{
  if (n == 0)
    return dfaEq1(i, j);
  else if (i == j) { /* <=> pi=0 */
    int var_index[1];
    var_index[0] = i;

    dfaSetup(4, 1, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(1);
    dfaStoreException(3,"1");
    dfaStoreState(2);

    /* state 2 - Reject */
    dfaAllocExceptions(0);
    dfaStoreState(2);
    
    /* state 3 - Accept */
    dfaAllocExceptions(0);
    dfaStoreState(3);
    
    return dfaBuild("0--+");
  }
  else {
    DFA *aut;
    int var_index[2];
    int state_no;
    char *finals;

    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(n+5, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);

    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(2, "00");
    dfaStoreException(n+4, "11");
    dfaStoreException(4, "10");
    dfaStoreState(3);

    /* state 2 */
    dfaAllocExceptions(2);
    dfaStoreException(2, "00");
    dfaStoreException(4, "10");
    dfaStoreState(3);

    /* state 3 - Reject */
    dfaAllocExceptions(0);
    dfaStoreState(3);

    /* state 4 .. (n+2) */
    for (state_no = 4; state_no <= n+2; state_no++) {
      dfaAllocExceptions(1);
      dfaStoreException(state_no+1,"X0");
      dfaStoreState(3);
    }
    
    /* state n+3 */
    dfaAllocExceptions(1);
    dfaStoreException(n+4, "X1");
    dfaStoreState(3);

    /* state n+4 - Accept */
    dfaAllocExceptions(0);
    dfaStoreState(n+4);

    {
      int k;

      finals = (char *) malloc((n+4) * (sizeof(char *)));
      for (k = 0; k < n+5; k++)
	finals[k] = '-';
      finals[0] = '0';
      finals[n+4] = '+';
    }

    aut = dfaBuild(finals);
    free(finals);
    return aut;
  }
}

DFA *dfaMax(int i, int j)
{
  if (i == j)
    return dfaTrue();
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(5, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(1);
    dfaStoreException(2, "0X");
    dfaStoreState(3);

    /* state 2 */
    dfaAllocExceptions(2);
    dfaStoreException(2, "0X");
    dfaStoreException(4, "10");
    dfaStoreState(3);

    /* state 3 - Accept */
    dfaAllocExceptions(1);
    dfaStoreException(3, "X0");
    dfaStoreState(4);
    
    /* state 4 - All reject */
    dfaAllocExceptions(0);
    dfaStoreState(4);

    return dfaBuild("0--+-");
  }
}

/* is i (first-order) lowest element in j?; i=0 if j is empty */
DFA *dfaMin(int i, int j)
{
  if (i == j)
    return dfaTrue();
  else {
    int var_index[2];
    var_index[0] = i;
    var_index[1] = j;

    dfaSetup(6, 2, var_index);

    /* boolvar */
    dfaAllocExceptions(0);
    dfaStoreState(1);
    
    /* state 1 */
    dfaAllocExceptions(3);
    dfaStoreException(2, "00");
    dfaStoreException(3, "01");
    dfaStoreException(4, "10");
    dfaStoreState(5);

    /* state 2 */
    dfaAllocExceptions(2);
    dfaStoreException(2, "00");
    dfaStoreException(5, "11");
    dfaStoreState(3);

    /* state 3 - Reject */
    dfaAllocExceptions(0);
    dfaStoreState(3);

    /* state 4 - Accept */
    dfaAllocExceptions(1);
    dfaStoreException(4, "X0");
    dfaStoreState(3);
    
    /* state 5 - Accept */
    dfaAllocExceptions(0);
    dfaStoreState(5);
    
    return dfaBuild("0---++");
  }
}

DFA *dfaPlusModulo1(int i, int j, int k)  /* see plusmodulo.mona */
{
  int var_index[3];
  var_index[0] = i;
  var_index[1] = j;
  var_index[2] = k;

  dfaSetup(13, 3, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  
  /* state 1 */ 
  dfaAllocExceptions(6);
  dfaStoreException(2, "000");
  dfaStoreException(3, "0X1");
  dfaStoreException(4, "010");
  dfaStoreException(5, "100");
  dfaStoreException(6, "101");
  dfaStoreException(7, "110");
  dfaStoreState(8);

  /* state 2 */ 
  dfaAllocExceptions(3);
  dfaStoreException(9, "000");
  dfaStoreException(4, "010");
  dfaStoreException(10, "100");
  dfaStoreState(3);

  /* state 3 */ 
  dfaAllocExceptions(0);
  dfaStoreState(3);

  /* state 4 */ 
  dfaAllocExceptions(1);
  dfaStoreException(11, "1X0");
  dfaStoreState(3);

  /* state 5 */ 
  dfaAllocExceptions(3);
  dfaStoreException(12, "X00");
  dfaStoreException(6, "X01");
  dfaStoreException(7, "X10");
  dfaStoreState(8);

  /* state 6 */ 
  dfaAllocExceptions(1);
  dfaStoreException(6, "X0X");
  dfaStoreState(8);

  /* state 7 */ 
  dfaAllocExceptions(1);
  dfaStoreException(8, "XX1");
  dfaStoreState(3);

  /* state 8 */ 
  dfaAllocExceptions(0);
  dfaStoreState(8);

  /* state 9 */ 
  dfaAllocExceptions(2);
  dfaStoreException(9, "000");
  dfaStoreException(4, "010");
  dfaStoreState(3);

  /* state 10 */ 
  dfaAllocExceptions(2);
  dfaStoreException(10, "X00");
  dfaStoreException(8, "X11");
  dfaStoreState(3);

  /* state 11 */ 
  dfaAllocExceptions(1);
  dfaStoreException(11, "XX0");
  dfaStoreState(8);

  /* state 12 */ 
  dfaAllocExceptions(3);
  dfaStoreException(12, "X00");
  dfaStoreException(6, "X01");
  dfaStoreException(7, "X10");
  dfaStoreState(3);

  return dfaBuild("0-------+----");
}

DFA *dfaMinusModulo1(int i, int j, int k)  /* see minusmodulo.mona */
{
  int var_index[3];
  var_index[0] = i;
  var_index[1] = j;
  var_index[2] = k;

  dfaSetup(12, 3, var_index);

  /* boolvar */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  
  /* state 1 */ 
  dfaAllocExceptions(6);
  dfaStoreException(2, "000");
  dfaStoreException(4, "010");
  dfaStoreException(5, "100");
  dfaStoreException(6, "101");
  dfaStoreException(7, "110");
  dfaStoreException(8, "111");
  dfaStoreState(3);

  /* state 2 */ 
  dfaAllocExceptions(2);
  dfaStoreException(2, "000");
  dfaStoreException(9, "100");
  dfaStoreState(3);

  /* state 3 */ 
  dfaAllocExceptions(0);
  dfaStoreState(3);

  /* state 4 */ 
  dfaAllocExceptions(2);
  dfaStoreException(4, "0X0");
  dfaStoreException(7, "1X0");
  dfaStoreState(3);

  /* state 5 */ 
  dfaAllocExceptions(3);
  dfaStoreException(10, "X00");
  dfaStoreException(6, "X01");
  dfaStoreException(11, "X10");
  dfaStoreState(8);

  /* state 6 */ 
  dfaAllocExceptions(1);
  dfaStoreException(6, "X0X");
  dfaStoreState(8);

  /* state 7 */ 
  dfaAllocExceptions(1);
  dfaStoreException(8, "XX1");
  dfaStoreState(3);

  /* state 8 */ 
  dfaAllocExceptions(0);
  dfaStoreState(8);

  /* state 9 */ 
  dfaAllocExceptions(2);
  dfaStoreException(11, "X10");
  dfaStoreException(8, "X11");
  dfaStoreState(3);

  /* state 10 */ 
  dfaAllocExceptions(2);
  dfaStoreException(10, "X00");
  dfaStoreException(6, "X01");
  dfaStoreState(3);

  /* state 11 */ 
  dfaAllocExceptions(1);
  dfaStoreException(11, "XX0");
  dfaStoreState(8);

  return dfaBuild("0-------+---");
}

DFA *dfaPresbConst(int i, int n)
{
  /* the following code constructs an automaton for the Presburger 
     relation 'p_i = n' where (non-negative) numbers are encoded 
     with least-significant bit first */
  
  int var_index[1];  /* array of indices of the free variables */
  int bits;  /* total number of bits required to represent 'n' */ 
  char *status;  /* array used for state kinds (-1/0/1) */
  DFA *res;
  int t, s;
  
  /* fill 'var_index', only one variable in this case */
  var_index[0] = i; 
  
  /* calculate 'bits' */
  for (t = n, bits = 0; t != 0; t >>= 1)
    bits++;

  /* prepare construction of automaton with
     'bits + 3' states and  1 variable */ 
  status = (char *) malloc(bits + 3);
  dfaSetup(bits + 3, 1, var_index);
  
  /* now create the states on at a time,
     always start with the initial state (state 0),
     state 0: */
  dfaAllocExceptions(0);
  dfaStoreState(2);
  status[0] = '0';  /* '0' denotes "don't care" */
  /* these two lines read: there are 0 exceptions for
     going to state 2, this is what we want since the first
     symbol being read encodes the values of the Boolean
     variables of which there are none in this case */
  
  /* we choose to use state 1 as the 'all reject' state,
     state 1: */
  dfaAllocExceptions(0);
  dfaStoreState(1);
  status[1] = '-';  /* '-' denotes "reject" */
  
  /* now generate one state for each bit in 'n' */
  for (t = n, s = 2; s <= bits+1; s++, t >>= 1) {
    /* state 's' goes to state 's+1' or all reject
       depending on the next bit */
    dfaAllocExceptions(1);
    dfaStoreException(1, (t&1) ? "0" : "1");
    dfaStoreState(s+1);
    status[s] = '-'; /* '-' denotes "reject" */
  }

  /* the last state accepts and loops on '0' */
  dfaAllocExceptions(1);
  dfaStoreException(1, "1");
  dfaStoreState(bits+2);
  status[bits+2] = '+'; /* '+' denotes "accept" */

  /* finalize the construction */
  res = dfaBuild(status);
  free(status);  /* deallocate 'status' */

  return res;
}

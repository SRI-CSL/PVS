/*
 * Extracted from Mona Code... (hr 11/98) and adjusted to Mona-14 (hr 7/01)
 */

#include <stdlib.h>
#include "dfa.h"

DFA* dfaConjunction(DFA *a1, DFA *a2)
{
  return (DFA*) dfaProduct(a1,a2,dfaAND);
}

DFA* dfaDisjunction(DFA *a1, DFA *a2)
{
  return (DFA*) dfaProduct(a1,a2,dfaOR);
}

DFA* dfaImplication(DFA *a1, DFA *a2)
{
  return (DFA*) dfaProduct(a1,a2,dfaIMPL);
}

DFA* dfaIff(DFA *a1, DFA *a2)
{
  return (DFA*) dfaProduct(a1,a2,dfaBIIMPL);
}


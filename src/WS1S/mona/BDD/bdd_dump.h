/* bdd_dump.h */

/*
 * MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
 *
 * Reproduction of all or part of this software is permitted for
 * educational or research use on condition that this copyright notice is
 * included in any copy. This software comes with no warranty of any
 * kind. In no event will BRICS be liable for any damages resulting from
 * use of this software.
 */

#ifndef __BDD_DUMP_H
#define __BDD_DUMP_H

#include "bdd.h"

void bddReverseMarks(bdd_manager *bddm, bdd_ptr p);
void bddDumpNode(bdd_manager *bddm, bdd_ptr p);
void bddDump(bdd_manager *bddm);

#endif

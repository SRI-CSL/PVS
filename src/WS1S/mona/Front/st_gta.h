//
// st_gta.h  
//
// MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
// 
// Reproduction of all or part of this software is permitted for
// educational or research use on condition that this copyright notice is
// included in any copy. This software comes with no warranty of any
// kind. In no event will BRICS be liable for any damages resulting from
// use of this software.

#ifndef __ST_GTA_H
#define __ST_GTA_H

extern "C" {
#include "../GTA/gta.h"
}

#include "ident.h"
#include "printline.h"

GTA *st_gta_restrict(GTA *g, Pos &p);
GTA *st_gta_negation(GTA *g, Pos &p);
GTA *st_gta_product(GTA *g1, GTA *g2, gtaProductType ff, Pos &p);
GTA *st_gta_project(GTA *a, unsigned i, Pos &p, bool quotient = true);
GTA *st_gta_minimization(GTA *g);
GTA *st_gta_copy(GTA *g);
void st_gta_replace_indices(GTA *a, IdentList *newvars, IdentList *oldvars,
			    bool offnew = true, bool offold = true);
GTA *st_gta_lastpos(GTA *gta, unsigned i);

#endif

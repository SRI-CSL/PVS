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
GTA *st_gta_project(GTA *a, Ident i, Pos &p, bool quotient = true);
GTA *st_gta_minimization(GTA *g);
GTA *st_gta_copy(GTA *g);
void st_gta_replace_indices(GTA *a, IdentList *newvars, IdentList *oldvars,
			    bool offnew = true, bool offold = true);
GTA *st_gta_allpos(GTA *gta, Ident i);

#endif

/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : bdd_quant.h
 unit-title: 
 ref.	   :
 author(s) : Copyright (c) 1990-1996 G.L.J.M. Janssen
 date	   : 23-APR-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* ************************************************************************ */
/* FILE DOCUMENTATION:                                                      */
/*                                                                          */
/* ************************************************************************ */

#ifndef BDD_QUANT_H
#define BDD_QUANT_H

/* ------------------------------------------------------------------------ */
/* DEFINES                                                                  */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* TYPE DEFINITIONS                                                         */
/* ------------------------------------------------------------------------ */

/* ------------------------------------------------------------------------ */
/* VARIABLES                                                                */
/* ------------------------------------------------------------------------ */
static char SccsId_BDD_QUANT_H[] = "%Z%%Y%/%M% %I% %G%";

/* ------------------------------------------------------------------------ */
/* FUNCTION PROTOTYPES                                                      */
/* ------------------------------------------------------------------------ */

extern BDDPTR bdd_quantify   (int existential, BDDPTR f, BDD_LIST vars);
extern BDDPTR bdd_quantify_c (int existential, BDDPTR f, BDDPTR vars_cube);
extern BDDPTR *bdd_quantify_vec
                     (int existential, BDDPTR *f_vec, int size, BDD_LIST vars);
extern BDDPTR *bdd_quantify_c_vec
	     (int existential, BDDPTR *f_vec, int size, BDDPTR vars_cube);
extern BDDPTR bdd_and_smooth (BDDPTR f, BDDPTR g, BDD_LIST vars);
extern BDDPTR bdd_and_smooth_c (BDDPTR f, BDDPTR g, BDDPTR vars_cube);
extern BDDPTR bdd_smooth                      (BDDPTR f, int x);
extern BDDPTR bdd_consensus                   (BDDPTR f, int x);

#endif /* BDD_QUANT_H */

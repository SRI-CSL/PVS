/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500, IBM RS/6000
 file	   : vfns.h
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1995 G.L.J.M. Janssen
 date	   : 15-JUL-1995
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#define BDDVEC_SIZE(F)			((F) ? (int) (F)[-1] : 0)

extern BDDPTR *MakeBDDVec		(int size);
extern   void  FreeBDDVec		(BDDPTR *F);
extern   void  FreeBDDVec2		(BDDPTR *F, int size);
extern BDDPTR *CopyBDDVec		(BDDPTR *F);
extern BDDPTR *CopyBDDVec2		(BDDPTR *F, int size, BDDPTR *toF);
extern   void  ComplBDDVec2             (BDDPTR *F, register int size);
extern   void  ComplBDDVec              (BDDPTR *F);

extern BDDPTR *bdd_constrain_vec	(BDDPTR *F, int i, int j, BDDPTR c);

/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : plot.h
 unit-title: 
 ref.	   : 
 author(s) : Copyright (C) 1991-1996 G.L.J.M. Janssen
 date	   : 19-NOV-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#ifdef COMMENT
typedef struct {
  int level;
  int position;
} XYPOS, *XYPOSPTR;

#define LEVEL_F(v)	((XYPOSPTR) BDD_USER_DATA_PTR (v))->level
#define POSITION_F(v)	((XYPOSPTR) BDD_USER_DATA_PTR (v))->position
#endif

#define LEVEL_F(v)	BDD_AUX1_L (v)
#define POSITION_F(v)	BDD_AUX2_L (v)

/* Places the vertices of the BDD's of vector f_vec in the carthesian plane,
   i.e. assigns X and Y position in the fields POSITION_F and LEVEL_F.
   Top variable node of f has lowest Y coordinate (1), THEN edges point to
   lower X positions (left), ELSE edges point to higher X coordinates (right).
   So the origin (x=1,y=1) is in upper left corner.
   Returns max X position assigned via max_xp; max level assigned is
   returned via max_yp. Constant nodes have no assigned X coordinate;
   their Y coordinate is equal to max level.
*/
extern void bdd_prepare_for_plot_vec
		(BDDPTR *f, int size, int *max_xp, int *max_yp, int compact,
		 int use_rank_leveling);

extern void bdd_plot_vec
	(FILE *fp, BDDPTR *f_vec, int size, int max_x, int max_y,
	 char *(*name_func) (BDDPTR));

extern void bdd_plot_vec2
	(FILE *fp, BDDPTR *f_vec, int size, int max_x, int max_y,
	 char *(*node_name_func) (BDDPTR),
	 char *(*func_name_func) (int));

/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : draw_ascii.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1994 G.L.J.M. Janssen
 date	   :  8-MAR-1994
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>
#include "alloc.h"
#include "bdd.h"
#include "plot.h"

static char **page;

#define MARGIN	2
#define X(x)	((x)<<1)
#define Y(y)	((y)<<1)

#define LOC(x, ox, y, oy)	page[Y(y)+(oy)]+X(x)+(ox)+MARGIN

static void copy (char *p, char *str)
{
  int c;

  while (c = *str++)
    if (c == '_') {
      if (*p == ' ')
	*p++ = c;
      else
	p++;
    }
    else
      *p++ = c;
}

static char * (*plot_func) (int) = NULL;

static char *default_plot_func (int index)
{
  static char buf[16];

  sprintf (buf, "f%d", index);
  return buf;
}

static void draw_func_name (FILE *fp, int x, int index)
{
  char *string = plot_func (index);
  int len = strlen (string);

  copy (LOC (x, 0, 0, 0), string);
}

static void draw_node (FILE *fp, int x, int y, char *id)
{
  static char buf[16];

  sprintf (buf, "(%s)", id);
  copy (LOC (x, -1, y, 0), buf);
}

static void draw_initial_edge (FILE *fp, int x, int tx, int ty,
			       int neg, int inv)
{
  copy (LOC (tx, 0, ty, -1), neg ? (inv ? "*" : "!") : (inv ? "+" : "|"));
}

static void draw_const (FILE *fp, int x, int y, int c)
{
  copy (LOC (x, 0, y, 0), c == 2 ? "X" : (c ? "1" : "0"));
}

static void draw_void (FILE *fp, int x, int y)
{
  copy (LOC (x, 0, y, 0), "void");
}

static void draw_then_const (FILE *fp, int x, int y, int c)
{
  copy (LOC (x, -1, y, 1), c == 2 ? "X" : (c ? "1" : "0"));
}

static void draw_else_const (FILE *fp, int x, int y, int c)
{
  copy (LOC (x, 1, y, 1), c == 2  ? "X" : (c ? "1" : "0"));
}

static void draw_then_edge (FILE *fp, int fx, int fy, int tx, int ty,
			    int neg, int inv)
{
  if (fx == tx)
    copy (LOC (fx, -1, fy, 1), neg ? (inv ? "*" : "!") : (inv ? "+" : "|"));
  else {
    copy (LOC (fx, -1, fy, 1), neg ? (inv ? "X." : "/.") : (inv ? "X" : "/"));
    if (ty == fy + 1 && tx < fx) {
      int x;

      for (x = tx; x < fx - 1; x++) {
	copy (LOC (x, 1, fy, 1), "_");
	copy (LOC (x, 2, fy, 1), "_");
      }
    }
  }
}

static void draw_else_edge (FILE *fp, int fx, int fy, int tx, int ty,
			    int neg, int inv)
{
  if (fx == tx)
    copy (LOC (fx, 1, fy, 1), neg ? (inv ? "*" : "!") : (inv ? "+" : "|"));
  else {
    copy (LOC (fx, 1, fy, 1), neg ? (inv ? "X." : "\\.") : (inv ? "X" : "\\"));
    if (ty == fy + 1 && fx < tx) {
      int x;

      for (x = fx + 1; x < tx; x++) {
	copy (LOC (x,  0, fy, 1), "_");
	copy (LOC (x,  1, fy, 1), "_");
      }
    }
  }
}

static void draw_start (FILE *fp, int max_x, int max_y)
{
  register int i;
  int maxx = max_x << 1;
  int maxy = max_y << 1;

  page = CALLOC_ARRAY (maxy + 2, char *);
  for (i = 0; i <= maxy; i++) {
    register int j;

    page[i] = MALLOC_ARRAY (maxx + 2 + MARGIN + 1, char);
    for (j = 0; j <= maxx + 2 + MARGIN; j++)
      page[i][j] = ' ';
    page[i][j] = '\0';
  }
}

static void draw_finish (FILE *fp, int max_x, int max_y)
{
  register int i;
  int maxy = max_y << 1;

  for (i = 0; i <= maxy; i++) {
    fprintf (fp, page[i]);
    fprintf (fp, "\n");
    free (page[i]);
  }
  free (page);
}

static FILE *plot_fp;
static char * (*plot_name) (BDDPTR);

static char *default_plot_name (BDDPTR v)
{
  static char buf[16];

  sprintf (buf, "%d", BDD_VARID (v));
  return buf;
}

#define Y_POS(v)	(LEVEL_F (v))

static void plot_node (BDDPTR v)
{
  /* Draw the node v: */
  if (!BDD_CONST_P (v)) {
    BDDPTR T = BDD_THEN (v);
    BDDPTR E = BDD_ELSE (v);

    /* Draw the non-terminal node: */
    draw_node (plot_fp, POSITION_F (v), Y_POS (v), (*plot_name) (v));

    /* Draw the outgoing edges: */
    if (BDD_0_P (T))
      draw_then_const (plot_fp, POSITION_F (v), Y_POS (v), 0);
    else
    if (BDD_1_P (T))
      draw_then_const (plot_fp, POSITION_F (v), Y_POS (v), 1);
    else
    if (BDD_X_P (T))
      draw_then_const (plot_fp, POSITION_F (v), Y_POS (v), 2);
    else
      draw_then_edge (plot_fp, POSITION_F (v), Y_POS (v),
		      POSITION_F (T), Y_POS (T),
		      BDD_NEG_P (T), BDD_I_INV_EDGE_P (T));

    if (BDD_0_P (E))
      draw_else_const (plot_fp, POSITION_F (v), Y_POS (v), 0);
    else
    if (BDD_1_P (E))
      draw_else_const (plot_fp, POSITION_F (v), Y_POS (v), 1);
    else
    if (BDD_X_P (E))
      draw_else_const (plot_fp, POSITION_F (v), Y_POS (v), 2);
    else
      draw_else_edge (plot_fp, POSITION_F (v), Y_POS (v),
		      POSITION_F (E), Y_POS (E),
		      BDD_NEG_P (E), BDD_I_INV_EDGE_P (E));
  }
}

void bdd_plot_vec (FILE *fp,
		   BDDPTR *f_vec,
		   int size,
		   int max_x, int max_y,
		   char *(*name_func) (BDDPTR))
{
  int i;
  float incr;
  int x;

  if (!f_vec)
    return;

  /* Watch out: constant don't have valid position. */
  if (size == 1 && !BDD_VOID_P (f_vec[0]) && !BDD_CONST_P (f_vec[0])) {
    x = POSITION_F (f_vec[0]);
    incr = 0.0;
  }
  else {
    x = 0;
    if (size > max_x) {
      max_x = size;
      incr = 1;
    }
    else
      incr = (max_x + 1) / size;
  }

  draw_start (fp, max_x, max_y);

  x = 0;

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    draw_func_name (fp, x, i);

    if (BDD_VOID_P (f))
      draw_void (fp, x, 0);
    else
    if (BDD_0_P (f))
      draw_const (fp, x, 0, 0);
    else
    if (BDD_1_P (f))
      draw_const (fp, x, 0, 1);
    else
    if (BDD_X_P (f))
      draw_const (fp, x, 0, 2);
    else {
      /* Draw initial edge to top variable node of f: */
      /* First check whether this is a negative edge: */
      /* BDD_NEG_P (f) */

      draw_initial_edge (fp, x, POSITION_F (f), LEVEL_F (f), BDD_NEG_P (f),
			 BDD_I_INV_EDGE_P (f));

      plot_fp = fp;
      plot_name = name_func ? name_func : default_plot_name;
      if (!BDD_MARK (f))
	bdd_traverse_pre (f, plot_node);
    }
    x += incr;
  } /*for*/

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      /* Let's be nice (and safe) and clear all aux fields.
	 Already encountered segmentation violation once...
      */
      bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);
  }

  draw_finish (fp, max_x, max_y);
}

void bdd_plot_vec2 (FILE *fp, BDDPTR *f_vec, int size,
		    int max_x, int max_y,
		    char *(*node_name_func) (BDDPTR),
		    char *(*func_name_func) (int))
{
  plot_func = func_name_func ? func_name_func : default_plot_func;

  bdd_plot_vec (fp, f_vec, size, max_x, max_y, node_name_func);
}

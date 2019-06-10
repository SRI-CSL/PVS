/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : draw_backdraw.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1996 G. Janssen/E. Huijbregts
 date	   : 11-NOV-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <setjmp.h>

#include "alloc.h"
#include "bdd.h"
#include "plot.h"
/*#include "run_child.h"*/

/* Vertical spacing between centers of 2 nodes: */
static int V_SPACE;
static int fh = 0;		/* font height */
static int fw = 2;		/* font width */
/*static int fch;*/
static int fcw = 0;

/* Radius for filled dot: */
#define r		4
/* Radius of circle for a node: */
#define R		4*r
/* Horizontal spacing between centers of 2 nodes: */
#define H_SPACE		(3*R)
#define X(x)		((x) * H_SPACE)
#define Y(y)		((y) * V_SPACE)

static void draw_node (FILE *fp, int x, int y, char *id, 
		       int T_neg, int T_inv, int E_neg, int E_inv)
{
  int fx, fy, tx, ty, len;
  int tmp = (int) (0.707 * (float) R);

  x = X(x);
  y = Y(y);
  fprintf (fp, "set-plane 0\n");
  fprintf (fp, "draw-circle %d %d %d\n", x, y, R);

  /* Draw ELSE edge stub: */
  if (T_neg && T_inv)
    fprintf (fp, "set-color blue\n");
  else
  if (T_neg)
    fprintf (fp, "set-color green\n");
  else
  if (T_inv)
    fprintf (fp, "set-color cyan\n");

  /* Draw THEN edge stub: */
  fx  = x-tmp;
  fy  = y+tmp;
  tx  = x-R;
  ty  = y+R;
  fprintf (fp, "draw-line %d %d %d %d\n", fx, fy, tx, ty);

  if (E_neg && E_inv)
    fprintf (fp, "set-color blue\n");
  else
  if (E_neg)
    fprintf (fp, "set-color green\n");
  else
  if (E_inv)
    fprintf (fp, "set-color cyan\n");
  else
    fprintf (fp, "set-color red\n");

  fx = x+tmp;
  tx = x+R;
  fprintf (fp, "draw-line %d %d %d %d\n", fx, fy, tx, ty);

  fprintf (fp, "set-color red\n");

  fprintf (fp, "set-plane 2\n");
  len = strlen (id);
  fx  = x - ((len-1)*fw + fcw)/2;
  fy  = y + fh/2;

  fprintf (fp, "draw-string %d %d \"%s\"\n", fx, fy, id);
}

static void draw_initial_edge (FILE *fp,
			       int fx, int tx, int ty, int neg, int inv)
{
  int fy;

  /* Will look like: 

  --> x
  |
  V        (x,0)
   y         |
             | (x, 0.5)
              \
               \
              (tx, ty)
  */


  fprintf (fp, "set-plane 0\n");
  if (neg && inv)
    fprintf (fp, "set-color blue\n");
  else
  if (neg)
    fprintf (fp, "set-color green\n");
  else
  if (inv)
    fprintf (fp, "set-color cyan\n");

  /* Draw first straight down part of edge: */
  fx = X(fx);
  tx = X(tx);
  fy = Y(0.5);
  ty = Y(ty) - R;
  fprintf (fp, "draw-line %d %d %d %d\n", fx, Y(0), fx, fy);

  /* Draw second part of edge: */
  fprintf (fp, "draw-line %d %d %d %d\n", fx, fy, tx, ty);

  /* Draw negation dot: */
  if (neg) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    fprintf (fp, "fill-circle %d %d %d\n", fx, fy, r);
  }

  /* Draw inversion dash: */
  if (inv) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    fprintf (fp, "draw-line %d %d %d %d\n", fx-r, fy, fx+r, fy);
  }
  fprintf (fp, "set-color red\n");
}

static void draw_const (FILE *fp, int x, int y, int c)
{
  char *string;

  fprintf (fp, "set-plane 2\n");

  x = X(x);
  y = Y(y+0.25);

  string = c == 2 ? "X" : c ? "1" : "0";

  x -= fcw/2;
  y += fh/2;

  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

/* Not finished! */
static void draw_terminal (FILE *fp, int x, int y)
{
  char *string;

  fprintf (fp, "set-plane 2\n");

  x = X(x);
  y = Y(y);

  string = "S";

  x -= fcw/2;
  y += fh/2;

  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

static char * (*plot_func) (int) = NULL;

static void draw_func_name (FILE *fp, int x, int index)
{
  char *string = plot_func (index);
  int y, len = strlen (string);

  fprintf (fp, "set-plane 2\n");

  x = X(x);
  y = Y(0);

  x -= ((len-1)*fw + fcw)/2;
  y -= fh/2;

  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

static void draw_void (FILE *fp, int x, int y)
{
  fprintf (fp, "set-plane 2\n");

  x = X(x);
  y = Y(y+0.25);

  x -= (3*fw + fcw)/2;
  y += fh/2;

  fprintf (fp, "draw-string %d %d \"void\"\n", x, y);
}

static void draw_then_const (FILE *fp, int x, int y, int c)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = c == 2 ? "X" : c ? "1" : "0";

  x -= R + fcw/2;
  y += R + 0.1 * V_SPACE + fh;

  fprintf (fp, "set-plane 2\n");
  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

static void draw_then_term (FILE *fp, int x, int y)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = "S";

  x -= R + fcw/2;
  y += R + 0.1 * V_SPACE + fh;

  fprintf (fp, "set-plane 2\n");
  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

static void draw_else_const (FILE *fp, int x, int y, int c)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = c == 2 ? "X" : c ? "1" : "0";

  x += R - fcw/2;
  y += R + 0.1 * V_SPACE + fh;

  fprintf (fp, "set-plane 2\n");
  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

static void draw_else_term (FILE *fp, int x, int y)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = "S";

  x += R - fcw/2;
  y += R + 0.1 * V_SPACE + fh;

  fprintf (fp, "set-plane 2\n");
  fprintf (fp, "draw-string %d %d \"%s\"\n", x, y, string);
}

static void draw_then_edge (FILE *fp,
			    int fx, int fy, int tx, int ty, int neg, int inv)
{
  fx = X(fx);
  tx = X(tx);
  fy = Y(fy);
  ty = Y(ty);

  fx -= R;
  fy += R;
  ty -= R;

  fprintf (fp, "set-plane 0\n");
  if (neg && inv)
    fprintf (fp, "set-color blue\n");
  else
  if (neg)
    fprintf (fp, "set-color green\n");
  else
  if (inv)
    fprintf (fp, "set-color cyan\n");

  fprintf (fp, "draw-line %d %d %d %d\n", fx, fy, tx, ty);

  /* Draw negation dot: */
  if (neg) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    fprintf (fp, "fill-circle %d %d %d\n", fx, fy, r);
  }

  /* Draw inversion dash: */
  if (inv) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    fprintf (fp, "draw-line %d %d %d %d\n", fx-r, fy, fx+r, fy);
  }
  fprintf (fp, "set-color red\n");
}

static void draw_else_edge (FILE *fp,
			    int fx, int fy, int tx, int ty, int neg, int inv)
{
  fx = X(fx);
  tx = X(tx);
  fy = Y(fy);
  ty = Y(ty);

  fx += R;
  fy += R;
  ty -= R;

  fprintf (fp, "set-plane 0\n");
  if (neg && inv)
    fprintf (fp, "set-color blue\n");
  else
  if (neg)
    fprintf (fp, "set-color green\n");
  else
  if (inv)
    fprintf (fp, "set-color cyan\n");

  fprintf (fp, "draw-line %d %d %d %d\n", fx, fy, tx, ty);

  /* Draw negation dot: */
  if (neg) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    fprintf (fp, "fill-circle %d %d %d\n", fx, fy, r);
  }

  /* Draw inversion dash: */
  if (inv) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    fprintf (fp, "draw-line %d %d %d %d\n", fx-r, fy, fx+r, fy);
  }
  fprintf (fp, "set-color red\n");
}

static void draw_start (FILE *fp, int max_x, int max_y)
{
  int tmp;

  tmp = ((float) (H_SPACE * max_x)) / ((float) max_y) + 0.5;
  V_SPACE = tmp < H_SPACE ? H_SPACE : tmp;

  fprintf (fp, "set-view-port %d %d %d %d\n",
	   -3*R, -2*R, X(max_x)+3*R, Y(max_y)+6*R);
  fprintf (fp, "set-world  %d %d %d %d\n",
	   -3*R, -2*R, X(max_x)+3*R, Y(max_y)+6*R);
  fprintf (fp, "clear-planes\n");
  fprintf (fp, "set-plane 0\n");
  fprintf (fp, "set-color red\n"); /* nodes and regular edges */
  fprintf (fp, "set-plane 1\n");
  fprintf (fp, "set-color blue\n");
  fprintf (fp, "set-plane 2\n"); /* text */
  fprintf (fp, "set-color black\n");
  fprintf (fp, "set-font 9x15\n");
}

static void draw_finish (FILE *fp)
{
  fprintf (fp, "/* end of BDD drawing */\n");
  fflush (fp);
}

static FILE *plot_fp;
static char * (*plot_name) ();

static char *default_plot_name (BDDPTR v)
{
  static char buf[16];

  sprintf (buf, "%d", BDD_VARID (v));
  return buf;
}

static char *default_plot_func (int index)
{
  static char buf[16];

  sprintf (buf, "f%d", index);
  return buf;
}

#define Y_POS(v)	(LEVEL_F (v))

static void plot_node (BDDPTR v)
{
  /* Draw the node v: */
  if (!BDD_TERM_P (v)) {
    BDDPTR T = BDD_THEN (v);
    BDDPTR E = BDD_ELSE (v);

    /* Draw the non-terminal node: */
    draw_node (plot_fp, POSITION_F (v), Y_POS (v), (*plot_name) (v),
	       BDD_O_INV_EDGE_P (T),
	       BDD_I_INV_EDGE_P (T),
	       !BDD_0_P (E) && BDD_O_INV_EDGE_P (E),
	       BDD_I_INV_EDGE_P (E));

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
    if (BDD_TERM_P (T)) {
      /* Special user defined terminal node. */
      draw_then_term (plot_fp, POSITION_F (v), Y_POS (v));
    }
    else
      draw_then_edge (plot_fp, POSITION_F (v), Y_POS (v),
		      POSITION_F (T), Y_POS (T),
		      BDD_O_INV_EDGE_P (T),
		      BDD_I_INV_EDGE_P (T));

    if (BDD_0_P (E))
      draw_else_const (plot_fp, POSITION_F (v), Y_POS (v), 0);
    else
    if (BDD_1_P (E))
      draw_else_const (plot_fp, POSITION_F (v), Y_POS (v), 1);
    else
    if (BDD_X_P (E))
      draw_else_const (plot_fp, POSITION_F (v), Y_POS (v), 2);
    else
    if (BDD_TERM_P (E)) {
      /* Special user defined terminal node. */
      draw_else_term (plot_fp, POSITION_F (v), Y_POS (v));
    }
    else
      draw_else_edge (plot_fp, POSITION_F (v), Y_POS (v),
		      POSITION_F (E), Y_POS (E),
		      BDD_O_INV_EDGE_P (E),
		      BDD_I_INV_EDGE_P (E));
  }
}

/* Let's be nice (and safe) and clear all aux fields.
   Already encountered segmentation violation once...
   Assumes that BDD_MARK is on and will thus reset those.
*/
static void bdd_leave_in_safe_state (BDDPTR *f_vec, int size, int extra_safe)
{
  int i;

  if (extra_safe) {
    /* First clear all marks: */
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && BDD_MARK (f))
	bdd_reset_marks (f);
    }

    /* Now mark all nodes again: */
    for (i = 0; i < size; i++) {
      BDDPTR f = f_vec[i];

      if (!BDD_VOID_P (f) && !BDD_MARK (f))
	bdd_reset_marks (f);
    }
  }

  /* Assume all nodes are marked. */
  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);
  }
}

/*
   (0,0)     x
    +------->
    |
    |
  y V

*/
void bdd_plot_vec (FILE *fp,
		   BDDPTR *f_vec,
		   int size,
		   int max_x, int max_y,
		   char *(*name_func) (BDDPTR))
{
  int i;
  float xf, incr;
  int x;

  if (!f_vec)
    return;

  /* Watch out: terminals don't have valid POSITION_F field. */
  if (size == 1 && !BDD_VOID_P (f_vec[0]) && !BDD_TERM_P (f_vec[0])) {
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

  xf = 0.0;

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    draw_func_name (fp, x, i);

    if (BDD_VOID_P (f))
      draw_void (fp, x, 0);
    else
    if (BDD_0_P (f)) {
/*      draw_initial_edge (fp, x, x, 1, 0, 0);*/
      draw_const (fp, x, 0, 0);
    }
    else
    if (BDD_1_P (f)) {
/*      draw_initial_edge (fp, x, x, 1, 0, 0);*/
      draw_const (fp, x, 0, 1);
    }
    else
    if (BDD_X_P (f)) {
/*      draw_initial_edge (fp, x, x, 1, 0, 0);*/
      draw_const (fp, x, 0, 2);
    }
    else
    if (BDD_TERM_P (f)) {
      /* Special user defined terminal node. */
      draw_terminal (fp, x, 0);
    }
    else {
      /* Draw initial edge to top variable node of f: */
      /* First check whether this is a negative edge: */
      /* BDD_NEG_P (f) */

      draw_initial_edge (fp, x, POSITION_F (f), LEVEL_F (f),
			 BDD_O_INV_EDGE_P (f),
			 BDD_I_INV_EDGE_P (f));
      plot_fp = fp;
      plot_name = name_func ? name_func : default_plot_name;
      if (!BDD_MARK (f))
	/* Can't use the regular "bdd_traverse_pre" because we here anticipate
	   interruption and we don't want to leave the f_vec BDDs in a corrupt
	   state!
	*/
	bdd_traverse_pre_rec (f, plot_node);
    }
    if (i < size-1) {
      xf += incr;
      x = xf;
    }
  } /*for*/

  bdd_leave_in_safe_state (f_vec, size, 0);

  draw_finish (fp);
}

#ifdef COMMENT
/* Old version: simply plot to file pointer fp. */
void bdd_plot_vec2 (FILE *fp, BDDPTR *f_vec, int size,
		    int max_x, int max_y,
		    char *(*node_name_func) (BDDPTR),
		    char *(*func_name_func) (int))
{
  plot_func = func_name_func ? func_name_func : default_plot_func;

  bdd_plot_vec (fp, f_vec, size, max_x, max_y, node_name_func);
}

/* Another old version: uses run_child to set up pipes to backdraw. */
/* This routine pipes the BDD drawing directly to the "backdraw" program,
   and waits till it's finished.
*/
void bdd_plot_vec2 (FILE *dummy,
		    BDDPTR *f_vec, int size,
		    int max_x, int max_y,
		    char *(*node_name_func) (BDDPTR),
		    char *(*func_name_func) (int))
{
  FILE *file_in, *file_out;
  const char *child = "backdraw";

  if (run_child (child, NULL, 0/*monitor_child*/, NULL, &file_in, &file_out)) {

    /* Write child input: */
    plot_func = func_name_func ? func_name_func : default_plot_func;
    bdd_plot_vec (file_in, f_vec, size, max_x, max_y, node_name_func);

    if (fclose (file_in))
      print_message ("EPLT001", "errno %d: %s", errno, strerror (errno));

    /* Wait for child to finish: */
    print_message ("IPLT002", "Waiting for %s to finish.", child);
    wait (0);

    if (fclose (file_out))
      print_message ("EPLT003", "errno %d: %s", errno, strerror (errno));
  }
  else
    print_message ("EPLT004", "Cannot run %s.", child);
}
#endif

static jmp_buf Context;

static void (*SIGPIPE_OAction)();
static void (*SIGINT_OAction)();

static void SIGPIPE_handler (int sig)
{
  print_message ("IPLT001", "Received signal %d.", sig);
  longjmp (Context, 1);
}

/* This routine pipes the BDD drawing directly to the "backdraw" program,
   and waits till it's finished or interrupted.
*/
void bdd_plot_vec2 (FILE *dummy,
		    BDDPTR *f_vec, int size,
		    int max_x, int max_y,
		    char *(*node_name_func) (BDDPTR),
		    char *(*func_name_func) (int))
{
  const char *child = "exec backdraw";
  FILE *file_in = (FILE *) popen (child, "w");

  if (!file_in) {
    print_message ("EPLT002", "Cannot open pipe to `%s' program.\n"
	     "\tEither not in PATH or this program is not installed.",
	     child);
    return;
  }

  /* Catch any broken pipes conditions: */
  SIGPIPE_OAction = signal (SIGPIPE, SIGPIPE_handler);
  SIGINT_OAction  = signal (SIGINT,  SIGPIPE_handler);

  if (!setjmp (Context)) {
    /* Write child input: */
    plot_func = func_name_func ? func_name_func : default_plot_func;

    bdd_plot_vec (file_in, f_vec, size, max_x, max_y, node_name_func);

    /* Wait for child to finish: */
    print_message ("IPLT003", "Waiting for %s to finish.", child);
  }
  else {
    /* Here because of SIGPIPE signal handler. */
    print_message ("IPLT005", "Program `%s' prematurely exited.", child);
    bdd_leave_in_safe_state (f_vec, size, 1);
  }

  if (pclose (file_in))
    print_message ("EPLT004", "errno %d: %s", errno, strerror (errno));

  /* Reset signal handlers: */
  signal(SIGPIPE, SIGPIPE_OAction);
  signal(SIGINT,  SIGINT_OAction);
}

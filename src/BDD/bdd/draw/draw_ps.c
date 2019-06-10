/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : draw_ps.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1994 G. Janssen/E. Huijbregts
 date	   : 25-FEB-1994
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>
#include "bdd.h"
#include "plot.h"

static void draw_node (FILE *fp, int x, int y, char *id)
{
  fprintf (fp, "(%s) %d %d node\n", id, x, y);
}

static void draw_initial_edge (FILE *fp,
			       int x, int tx, int ty, int neg, int inv)
{
  fprintf (fp, "(%s) %d %d %d edge\n", neg ? "n" : "p", x, tx, ty);
}

static void draw_const (FILE *fp, int x, int y, int c)
{
  fprintf (fp, "(%s) %d %d const\n", c == 2 ? "X" : c ? "1" : "0", x, y);
}


static char * (*plot_func) (int) = NULL;

static void draw_func_name (FILE *fp, int x, int index)
{
  char *string = plot_func (index);

  fprintf (fp, "(%s) %d %d name\n", string, x, 0);
}

static void draw_void (FILE *fp, int x, int y)
{
  fprintf (fp, "(void) %d %d void\n", x, y);
}

static void draw_then_const (FILE *fp, int x, int y, int c)
{
  fprintf (fp, "(%s) %d %d then_const\n", c == 2 ? "X" : c ? "1" : "0", x, y);
}

static void draw_else_const (FILE *fp, int x, int y, int c)
{
  fprintf (fp, "(%s) %d %d else_const\n", c == 2 ? "X" : c ? "1" : "0", x, y);
}

static void draw_then_edge (FILE *fp,
			    int fx, int fy, int tx, int ty, int neg, int inv)
{
  fprintf (fp, "(%s) %d %d %d %d then\n", neg ? "n" : "p", fx, fy, tx, ty);
}

static void draw_else_edge (FILE *fp,
			    int fx, int fy, int tx, int ty, int neg, int inv)
{
  fprintf (fp, "(%s) %d %d %d %d else\n", neg ? "n" : "p", fx, fy, tx, ty);
}

static void draw_start (FILE *fp, int max_x, int max_y)
{
fprintf (fp, "%%!\n");
fprintf (fp, "/mm {72 mul 25.4 div} def\n");
fprintf( fp, "/PageWidth  210 mm def %% const\n");
fprintf( fp, "/PageHeight 297 mm def %% const\n");
fprintf( fp, "/BorderWidth  7 mm def %% const\n");
fprintf( fp, "/FontHeight     10 def %% const\n");
fprintf (fp, "/setR {/R exch def} def\n");
fprintf (fp, "/setr {/r exch def} def\n");
fprintf (fp, "/setscaleX {/scaleX exch def} def\n");
fprintf (fp, "/setscaleY {/scaleY exch def} def\n");
fprintf (fp, "/setx {/x exch def} def\n");
fprintf (fp, "/sety {/y exch def} def\n");
fprintf (fp, "/setfx {/fx exch def} def\n");
fprintf (fp, "/setfy {/fy exch def} def\n");
fprintf (fp, "/settx {/tx exch def} def\n");
fprintf (fp, "/setty {/ty exch def} def\n");
fprintf (fp, "/settmp {/tmp exch def} def\n");
fprintf (fp, "/Times-Roman findfont FontHeight scalefont setfont\n");
fprintf (fp, "\n");
fprintf (fp, "/rescale {                     %% stack: max_pos, max_level\n");
fprintf (fp, "  PageHeight 2 BorderWidth mul\n");
fprintf (fp, "  sub exch 1 add div setscaleY %% scaleY = (PH-2BW)/(max_level+1)\n" );
fprintf (fp, "  PageWidth 2 BorderWidth mul\n");
fprintf (fp, "  sub exch div setscaleX       %% scaleX = (PH-2BW)/(max_pos)\n" );
fprintf (fp, "  scaleX .33 mul dup setR\n");
fprintf( fp, "  0.25 scaleY mul gt {.2 scaleY mul setR} if\n");
fprintf (fp, "  .6 mm setr\n");
fprintf (fp, "  0 PageHeight translate       %% go to top of page\n");
fprintf (fp, "  BorderWidth scaleX 2 div add\n");
fprintf (fp, "  BorderWidth scaleY add\n");
fprintf (fp, "  neg translate                %% go to left top of drawing\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/node {                        %% stack: (id), x, y\n");
fprintf (fp, "  scaleY mul sety\n");
fprintf (fp, "  scaleX mul setx\n");
fprintf (fp, "  newpath\n");
fprintf (fp, "  %% draw node\n");
fprintf (fp, "  x y neg R 0 360 arc\n");
fprintf (fp, "  %% draw then edge\n");
fprintf (fp, "  .5 sqrt R mul settmp         %% tmp = sqrt(2) * R\n");
fprintf (fp, "  x tmp sub setfx              %% fx = X(x) - tmp\n");
fprintf (fp, "  y tmp add setfy              %% fy = Y(y) - sqrt 2 * R\n");
fprintf (fp, "  x R sub settx                %% tx = X(x) - R\n");
fprintf (fp, "  y R add setty                %% ty = Y(y) + R\n");
fprintf (fp, "  fx fy neg moveto\n");
fprintf (fp, "  tx ty neg lineto\n");
fprintf (fp, "  %% draw else edge\n");
fprintf (fp, "  x tmp add setfx              %% fx = X(x) + tmp\n");
fprintf (fp, "  x R add settx                %% tx = X(x) + R\n");
fprintf (fp, "  fx fy neg moveto\n");
fprintf (fp, "  tx ty neg lineto\n");
fprintf (fp, "  stroke\n");
fprintf (fp, "  %% draw string\n");
fprintf (fp, "  dup stringwidth pop\n");
fprintf (fp, "  2 div neg x add\n");
fprintf (fp, "  y FontHeight 3 div add\n");
fprintf (fp, "  neg moveto show\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/edge {                        %% stack: (polarity), fx, tx, ty\n");
fprintf (fp, "  %% draw initial edge\n");
/*fprintf (fp, "  dup scaleY mul R sub setty   %% ty = Y(y) - R\n");*/
fprintf (fp, "  scaleY mul R sub setty       %% ty = Y(y) - R\n");
/*fprintf (fp, "  1 sub scaleY mul setfy       %% fy = Y(y-1)\n");*/
fprintf (fp, "  0 setfy                      %% fy = Y(0)\n");
/*fprintf (fp, "  scaleX mul dup setfx settx   %% fx = tx = X(x)\n");*/
fprintf (fp, "  scaleX mul settx             %% tx = X(tx)\n");
fprintf (fp, "  scaleX mul setfx             %% fx = X(fx)\n");
fprintf (fp, "  newpath\n");
fprintf (fp, "  fx fy neg moveto\n");
fprintf (fp, "  0.5 scaleY mul setfy         %% fy = Y(0.5)\n");
fprintf (fp, "  fx fy neg lineto\n");
fprintf (fp, "  tx ty neg lineto\n");
fprintf (fp, "  stroke\n");
fprintf (fp, "  %% draw negation if necessary\n");
fprintf (fp, "  (n) eq {\n" );
fprintf (fp, "    fx tx add 2 div            %% fx = (fx+tx)/2\n");
fprintf (fp, "    fy ty add 2 div            %% fy = (fy+ty)/2\n");
fprintf (fp, "    neg r 0 360 arc\n");
fprintf (fp, "    0 setgray fill\n");
fprintf (fp, "  }if\n" );
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/then { %%stack: (polarity), fx, fy, tx, ty\n");
fprintf (fp, "  %% draw then edge\n");
fprintf (fp, "  scaleY mul R sub setty       %% ty = Y(ty) - R\n");
fprintf (fp, "  scaleX mul settx             %% tx = X(tx)\n");
fprintf (fp, "  scaleY mul R add setfy       %% fy = Y(fy) + R\n");
fprintf (fp, "  scaleX mul R sub setfx       %% fx = X(fx) - R\n");
fprintf (fp, "  newpath\n");
fprintf (fp, "  fx fy neg moveto\n");
fprintf (fp, "  tx ty neg lineto\n");
fprintf (fp, "  stroke\n");
fprintf (fp, "  %% draw negation if necessary\n");
fprintf (fp, "  (n) eq {\n" );
fprintf (fp, "    fx tx add 2 div            %% fx = (fx+tx)/2\n");
fprintf (fp, "    fy ty add 2 div            %% fy = (fy+ty)/2\n");
fprintf (fp, "    neg r 0 360 arc\n");
fprintf (fp, "    0 setgray fill\n");
fprintf (fp, "  }if\n" );
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/else { %%stack: (polarity), fx, fy, tx, ty\n");
fprintf (fp, "  %% draw else edge\n");
fprintf (fp, "  scaleY mul R sub setty       %% ty = Y(ty) - R\n");
fprintf (fp, "  scaleX mul settx             %% tx = X(tx)\n");
fprintf (fp, "  scaleY mul R add setfy       %% fy = Y(fy) + R\n");
fprintf (fp, "  scaleX mul R add setfx       %% fx = X(fx) + R\n");
fprintf (fp, "  newpath\n");
fprintf (fp, "  fx fy neg moveto\n");
fprintf (fp, "  tx ty neg lineto\n");
fprintf (fp, "  stroke\n");
fprintf (fp, "  %% draw negation if necessary\n");
fprintf (fp, "  (n) eq {\n" );
fprintf (fp, "    fx tx add 2 div            %% fx = (fx+tx)/2\n");
fprintf (fp, "    fy ty add 2 div            %% fy = (fy+ty)/2\n");
fprintf (fp, "    neg r 0 360 arc\n");
fprintf (fp, "    0 setgray fill\n");
fprintf (fp, "  }if\n" );
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/const {                       %% stack: (const), x, y\n");
fprintf (fp, "  scaleY mul FontHeight 2 div add\n");
fprintf (fp, "  exch scaleX mul exch\n");
fprintf (fp, "  newpath neg moveto dup stringwidth\n");
fprintf (fp, "  pop 2 div neg 0 rmoveto show\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/void {                       %% stack: (void), x, y\n");
fprintf (fp, "  scaleY mul FontHeight 2 div add\n");
fprintf (fp, "  exch scaleX mul exch\n");
fprintf (fp, "  newpath neg moveto dup stringwidth\n");
fprintf (fp, "  pop 2 div neg 0 rmoveto show\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/name {                       %% stack: (name), x, y\n");
fprintf (fp, "  scaleY mul FontHeight sub\n");
fprintf (fp, "  exch scaleX mul exch\n");
fprintf (fp, "  newpath neg moveto dup stringwidth\n");
fprintf (fp, "  pop 2 div neg 0 rmoveto show\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/then_const {                  %% stack: (const), x, y\n");
fprintf (fp, "  scaleY mul exch\n");
fprintf (fp, "  scaleX mul exch\n");
fprintf (fp, "  newpath neg moveto dup stringwidth pop\n");
fprintf (fp, "  2 div R add neg              %% x = X(x) - R - fw/2\n");
fprintf (fp, "  R 0.025 scaleY mul add\n");
fprintf (fp, "  FontHeight add               %% y = Y(y) + R + 0.025*scaleY + FH\n");
fprintf (fp, "  neg rmoveto show\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "/else_const {                  %% stack: (const), x, y\n");
fprintf (fp, "  scaleY mul exch\n");
fprintf (fp, "  scaleX mul exch\n");
fprintf (fp, "  newpath neg moveto dup stringwidth pop\n");
fprintf (fp, "  2 div neg R add              %% x = X(x) + R - fw/2\n");
fprintf (fp, "  R 0.025 scaleY mul add\n");
fprintf (fp, "  FontHeight add               %% y = Y(y) + R + 0.025*scaleY + FH\n");
fprintf (fp, "  neg rmoveto show\n");
fprintf (fp, "} def\n");
fprintf (fp, "\n");
fprintf (fp, "%d %d rescale\n", max_x, max_y);
}

static void draw_finish (FILE *fp)
{
  fprintf (fp, "showpage\n");
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
		      BDD_NEG_P (T),
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
      draw_else_edge (plot_fp, POSITION_F (v), Y_POS (v),
		      POSITION_F (E), Y_POS (E),
		      BDD_NEG_P (E),
		      BDD_I_INV_EDGE_P (E));
  }
}

void bdd_plot_vec (FILE *fp,
		   BDDPTR *f_vec,
		   int size,
		   int max_x, int max_y,
		   char *(*name_func) ())
{
  int i;
  float xf, incr;
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
    if (i < size-1) {
      xf += incr;
      x = xf;
    }
  } /*for*/

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      /* Let's be nice (and safe) and clear all aux fields.
	 Already encountered segmentation violation once...
      */
      bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);
  }

  draw_finish (fp);
}

void bdd_plot_vec2 (FILE *fp, BDDPTR *f_vec, int size,
		    int max_x, int max_y,
		    char *(*node_name_func) (BDDPTR),
		    char *(*func_name_func) (int))
{
  plot_func = func_name_func ? func_name_func : default_plot_func;

  bdd_plot_vec (fp, f_vec, size, max_x, max_y, node_name_func);
}

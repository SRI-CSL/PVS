/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : draw_X.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1996 G. Janssen/E. Huijbregts
 date	   : 25-NOV-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* LINTLIBRARY */
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>

#include "bdd.h"
#include "plot.h"
#include "bdd_icon.h"

int DEBUG = 0;

#define WINDOWNAME     "XRobdd"
#define ICONNAME       "XRobdd"
#define MAXDIM              5 /* max 5 characters in string */
#define WBW                10 /* white boundary width */

#define X(x)  (int) ((x) * scaleX + 0.5 * scaleX + WBW)
#define Y(y)  (int) ((y) * scaleY + 0.5 * scaleY + WBW)
#define r      2


static unsigned long  bw;
static int            depth;
static int            xdim, ydim;
static float          scaleX, scaleY;
static int            R;
static int            fh, fch, fw, fcw;
static Pixmap         pix;           
static Display        *dpy;
static int            scr_id;
static Screen         *screen;
static Window         win;
static Pixmap         iconpix;
static XSizeHints     xsh;
static XFontStruct    *font_info;
static void           (*RedrawRoutine)();
static GC             gcblack, gcwhite;

static int first_expose = 1;

/********** SCALE ***********/
static void scale (void)
{
  float tmp;

  scaleX = (float) (xsh.width  - 2*WBW) / (float) (xdim);
  scaleY = (float) (xsh.height - 2*WBW) / (float) (ydim + 1);

  tmp = (float) scaleX / 3.0;
  if (tmp  > 0.25 * scaleY)
     tmp = (float) scaleY / 5.0;

  R = (int) tmp;
/*
  fprintf( stdout, "xdim = %d, ydim = %d\n", xdim, ydim);
  fprintf( stdout, "scaleX = %f, scaleY = %f, R = %d (%f)\n", scaleX, scaleY, R, tmp);
*/
}

/********** EVENT_HANDLER **********/
static void event_handler (void)
{
  XEvent event;
  int reduced = 0;

  while (True) {
    XNextEvent (dpy, &event);
    switch (event.type) {
    case Expose:
      if (!event.xexpose.count) { /* merge multiple expose events */
	if (first_expose) {
	  first_expose = 0;
	  RedrawRoutine ();
	}
	else
	if (!DoesBackingStore(screen)) {
	  XCopyArea (dpy, pix, win, gcwhite, 0, 0, xsh.width, xsh.height,
		     0, 0);
	}
      }
      break;

    case ConfigureNotify:
      /* Skip move events here, they are handled as Expose events */
      if (!(   xsh.width  == event.xconfigure.width
	    && xsh.height == event.xconfigure.height)) {
        if (   xsh.width  > event.xconfigure.width
	    && xsh.height > event.xconfigure.height)
	  reduced = 1;

	first_expose = 1;	
	xsh.width  = event.xconfigure.width;
	xsh.height = event.xconfigure.height;
	if (!DoesBackingStore (screen)) {
	  XFreePixmap (dpy, pix);
	  pix = XCreatePixmap (dpy, win, xsh.width, xsh.height, depth);
	  XFillRectangle (dpy, pix, gcwhite, 0, 0, xsh.width, xsh.height);
	}
	scale ();
	XClearWindow (dpy, win);

	if (reduced) {
	  reduced = 0;
	  RedrawRoutine ();
        }
      }
      break;

    case KeyPress:
      {
	XComposeStatus compose;
	KeySym         keysym;
	char           buffer[10];
	int            bufsize = 10;

	XLookupString (&event.xkey, buffer, bufsize, &keysym, &compose);
	if (keysym == XK_Q || keysym == XK_q)
	  return;
	break;
      }

    default:
      break;
    } /* end switch */
  }
}

/********** DRAW_START **********/
static void draw_start (int max_pos, int max_level,
			void (*action)(void))
{
  static XEvent event;
  XGCValues            gcv;
  XSetWindowAttributes xswa;  /* Temporary Set Window Attribute struct */
  char *getenv ();

  /* Open display: */
  if (!(dpy = XOpenDisplay (getenv ("DISPLAY")))) {
    fprintf (stderr, "Cannot connect to Xserver %s\n", XDisplayName (NULL));
    exit(1);
  }

  /* Create window: */
  scr_id     = DefaultScreen (dpy);
  screen     = ScreenOfDisplay (dpy, scr_id);
  depth      = DefaultDepth (dpy, scr_id); 
  bw         = 3;
  xsh.x      = 0;
  xsh.y      = 0;
  xsh.width  = 500;
  xsh.height = 500;
  xsh.flags  = (USPosition | USSize); 

  win = XCreateSimpleWindow (dpy, RootWindow(dpy,scr_id), xsh.x, xsh.y,
			     xsh.width, xsh.height, bw,
			     BlackPixel (dpy,scr_id), WhitePixel (dpy,scr_id));

  if (!DoesBackingStore(screen)) {
    if (DEBUG)
      fprintf (stdout, "IT HAS NO BACKING STORE\n");
    pix = XCreatePixmap (dpy, win, xsh.width, xsh.height, depth);
  }

  /* Create icon: */
  iconpix = XCreateBitmapFromData (dpy, win, bdd_icon_bits, bdd_icon_width,
				   bdd_icon_height);

  /* Set window properties: */
  XSetStandardProperties (dpy, win, WINDOWNAME, ICONNAME, iconpix, NULL,
			  0, &xsh);
  xswa.save_under    = True;
  xswa.backing_store = Always;
  xswa.colormap      = DefaultColormap (dpy, scr_id);
  xswa.bit_gravity   = CenterGravity;
  XChangeWindowAttributes (dpy, win, (CWSaveUnder | CWBackingStore
				      | CWColormap | CWBitGravity), &xswa);

  /* Set events: */
  XSelectInput (dpy, win, ExposureMask | StructureNotifyMask | KeyPressMask);

  /* Scale: */
  xdim = max_pos;
  ydim = max_level;
  scale ();

  /* Make graphic contents: */
  if (!(font_info = XLoadQueryFont (dpy, "9x15"))) {
    fprintf (stderr, "Cannot open 9x15 font\n");
    exit (1);
  }

  /* Calculate font height in pixels: */
  fh  = font_info->ascent;
  fch = font_info->max_bounds.ascent;
  fw  = font_info->max_bounds.width;
  fcw = font_info->max_bounds.rbearing - font_info->max_bounds.lbearing;

  /* WHITE */
  gcv.foreground = WhitePixel (dpy,scr_id);
  gcv.background = BlackPixel (dpy,scr_id);
  gcv.font       = font_info->fid;
  gcwhite        = XCreateGC (dpy, win, GCForeground | GCBackground | GCFont,
			      &gcv);

  /* BLACK */
  gcv.foreground = BlackPixel(dpy,scr_id);
  gcv.background = WhitePixel(dpy,scr_id);
  gcblack        = XCreateGC (dpy, win, GCForeground | GCBackground, &gcv);

  /* Ensure white background in window: */
  if (!DoesBackingStore(screen))
    XFillRectangle (dpy, pix, gcwhite, 0, 0, xsh.width, xsh.height);
  XFillRectangle (dpy, win, gcwhite, 0, 0, xsh.width, xsh.height);
    
  XMapWindow (dpy, win);

  RedrawRoutine = action;                     /* set RedrawRoutine */

  first_expose = 1;

  /* Force an expose event in all cases to  */
  /* overcome problems with display manager */
  event.type = Expose;
  XSendEvent (dpy, win, True, ExposureMask, &event);

  XSync (dpy, False);
}

/********** DRAW_FINISH **********/
static void draw_finish (void)
{
  XUnloadFont (dpy, font_info->fid);
  XFreeGC (dpy,gcwhite);
  XFreeGC (dpy,gcblack);
  XCloseDisplay (dpy);
}

/********** LOW LEVEL DRAWING ROUTINES **********/
static void draw_node (int p, int l, char *string)
{
  int tmp = (int) (0.707* (float)R);
  int fx, fy, tx, ty, len;

  /* scale */
  p = X(p);
  l = Y(l);

  /* draw node */
  if (!DoesBackingStore (screen))
    XDrawArc (dpy, pix, gcblack, p-R, l-R, 2*R, 2*R, 0, 360*64);
  XDrawArc (dpy, win, gcblack, p-R, l-R, 2*R, 2*R, 0, 360*64);

  /* draw string */
  len = strlen (string);
  fx  = p - ((len-1)*fw + fcw)/2;
  fy  = l + fh/2;
  if (!DoesBackingStore (screen))
    XDrawString (dpy,pix,gcblack,fx,fy,string,len);
  XDrawString (dpy,win,gcblack,fx,fy,string,len);

  /* draw then edge */
  fx  = p-tmp;
  fy  = l+tmp;
  tx  = p-R;
  ty  = l+R;
  if (!DoesBackingStore (screen))
    XDrawLine (dpy, pix, gcblack, fx, fy, tx, ty);
  XDrawLine (dpy, win, gcblack, fx, fy, tx, ty);

  /* draw else edge */
  fx = p+tmp;
  tx = p+R;
  if (!DoesBackingStore (screen))
    XDrawLine (dpy, pix, gcblack, fx, fy, tx, ty);
  XDrawLine (dpy, win, gcblack, fx, fy, tx, ty);
}

static void draw_initial_edge (int x, int p, int l, int neg, int inv)
{
  int fx, fy, tx, ty;

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
  /* Draw first straight down part of edge: */
  fx  = X(x);
  fy  = Y(0);
  tx  = fx;
  ty  = Y(0.5);

  if (!DoesBackingStore (screen))
    XDrawLine (dpy, pix, gcblack, fx, fy, tx, ty);
  XDrawLine (dpy, win, gcblack, fx, fy, tx, ty);

  /* Draw second part of edge: */
  fy  = ty;
  tx  = X(p);
  ty  = Y(l)-R;

  if (!DoesBackingStore (screen))
    XDrawLine (dpy, pix, gcblack, fx, fy, tx, ty);
  XDrawLine (dpy, win, gcblack, fx, fy, tx, ty);

  /* draw negation node */
  if (neg) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    if (!DoesBackingStore (screen))
      XDrawArc (dpy, pix, gcblack, fx-r, fy-r, 2*r, 2*r, 0, 360*64);
    XDrawArc (dpy, win, gcblack, fx-r, fy-r, 2*r, 2*r, 0, 360*64);
  }

  /* draw inv node */
  if (inv) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    if (!DoesBackingStore (screen))
      XDrawLine (dpy, pix, gcblack, fx-2*r, fy, fx+2*r, fy);
    XDrawLine (dpy, win, gcblack, fx-2*r, fy, fx+2*r, fy);
  }
}

static void draw_const (int x, int y, int c)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = c == 2 ? "X" : c ? "1" : "0";

  x -= fcw/2;
  y += fh/2;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, string, 1);
  XDrawString (dpy, win, gcblack, x, y, string, 1);
}

/* Not finished! */
static void draw_terminal (int x, int y)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = "S";

  x -= fcw/2;
  y += fh/2;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, string, 1);
  XDrawString (dpy, win, gcblack, x, y, string, 1);
}

static char * (*plot_func) (int) = NULL;

static void draw_func_name (int x, int index)
{
  int y, len;
  char *string = plot_func (index);

  /* scale */
  x = X(x);
  y = Y(0);

  /* draw string */
  len = strlen (string);
  x -= ((len-1)*fw + fcw)/2;
  y -= fh/2;
  if (!DoesBackingStore (screen))
    XDrawString (dpy,pix,gcblack,x,y,string,len);
  XDrawString (dpy,win,gcblack,x,y,string,len);
}

static void draw_void (int x, int y)
{
  /* scale */
  x = X(x);
  y = Y(y);

  x -= ((3)*fw + fcw)/2;
  y += fh/2;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, "void", 4);
  XDrawString (dpy, win, gcblack, x, y, "void", 4);
}

static void draw_then_edge (int fp, int fl, int tp, int tl, int neg, int inv)
{
  int fx, fy, tx, ty;

  /* scale */
  fp = X(fp);
  tp = X(tp);
  fl = Y(fl);
  tl = Y(tl);

  /* draw then edge */
  fx  = fp-R;
  fy  = fl+R;
  tx  = tp;
  ty  = tl-R;
  if (!DoesBackingStore (screen))
    XDrawLine (dpy, pix, gcblack, fx, fy, tx, ty);
  XDrawLine (dpy, win, gcblack, fx, fy, tx, ty);

  /* draw negation node */
  if (neg) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    if (!DoesBackingStore (screen))
      XDrawArc (dpy, pix, gcblack, fx-r, fy-r, 2*r, 2*r, 0, 360*64);
    XDrawArc (dpy, win, gcblack, fx-r, fy-r, 2*r, 2*r, 0, 360*64);
  }

  /* draw inv node */
  if (inv) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    if (!DoesBackingStore (screen))
      XDrawLine (dpy, pix, gcblack, fx-2*r, fy, fx+2*r, fy);
    XDrawLine (dpy, win, gcblack, fx-2*r, fy, fx+2*r, fy);
  }
}

static void draw_else_edge (int fp, int fl, int tp, int tl, int neg, int inv)
{
  int fx, fy, tx, ty;

  /* scale */
  fp = X(fp);
  fl = Y(fl);
  tp = X(tp);
  tl = Y(tl);

  /* draw then edge */
  fx  = fp+R;
  fy  = fl+R;
  tx  = tp;
  ty  = tl-R;
  if (!DoesBackingStore (screen))
    XDrawLine (dpy, pix, gcblack, fx, fy, tx, ty);
  XDrawLine (dpy, win, gcblack, fx, fy, tx, ty);

  /* draw negation node */
  if (neg) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    if (!DoesBackingStore (screen))
      XDrawArc (dpy, pix, gcblack, fx-r, fy-r, 2*r, 2*r, 0, 360*64);
    XDrawArc (dpy, win, gcblack, fx-r, fy-r, 2*r, 2*r, 0, 360*64);
  }

  /* draw inv node */
  if (inv) {
    fx = (fx+tx)/2;
    fy = (fy+ty)/2;
    if (!DoesBackingStore (screen))
      XDrawLine (dpy, pix, gcblack, fx-2*r, fy, fx+2*r, fy);
    XDrawLine (dpy, win, gcblack, fx-2*r, fy, fx+2*r, fy);
  }
}

static void draw_then_const (int x, int y, int c)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = c == 2 ? "X" : c ? "1" : "0";

  x -= R + fcw/2;
  y += R + 0.1 * scaleY + fh;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, string, 1);
  XDrawString (dpy, win, gcblack, x, y, string, 1);
}

static void draw_then_term (int x, int y)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = "S";

  x -= R + fcw/2;
  y += R + 0.1 * scaleY + fh;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, string, 1);
  XDrawString (dpy, win, gcblack, x, y, string, 1);
}

static void draw_else_const (int x, int y, int c)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = c == 2 ? "X" : c ? "1" : "0";

  x += R - fcw/2;
  y += R + 0.1 * scaleY + fh;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, string, 1);
  XDrawString (dpy, win, gcblack, x, y, string, 1);
}

static void draw_else_term (int x, int y)
{
  char *string;

  /* scale */
  x = X(x);
  y = Y(y);

  /* draw string */
  string = "S";

  x += R - fcw/2;
  y += R + 0.1 * scaleY + fh;

  if (!DoesBackingStore (screen))
    XDrawString (dpy, pix, gcblack, x, y, string, 1);
  XDrawString (dpy, win, gcblack, x, y, string, 1);
}

static char * (*plot_name) (BDDPTR);

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
    draw_node (POSITION_F (v), Y_POS (v), (*plot_name) (v));

    /* Draw the outgoing edges: */
    if (BDD_0_P (T))
      draw_then_const (POSITION_F (v), Y_POS (v), 0);
    else
    if (BDD_1_P (T))
      draw_then_const (POSITION_F (v), Y_POS (v), 1);
    else
    if (BDD_X_P (T))
      draw_then_const (POSITION_F (v), Y_POS (v), 2);
    else
    if (BDD_TERM_P (T)) {
      /* Special user defined terminal node. */
      draw_then_term (POSITION_F (v), Y_POS (v));
    }
    else
      draw_then_edge (POSITION_F (v), Y_POS (v),
		      POSITION_F (T), Y_POS (T),
		      BDD_O_INV_EDGE_P (T),
		      BDD_I_INV_EDGE_P (T));

    if (BDD_0_P (E))
      draw_else_const (POSITION_F (v), Y_POS (v), 0);
    else
    if (BDD_1_P (E))
      draw_else_const (POSITION_F (v), Y_POS (v), 1);
    else
    if (BDD_X_P (E))
      draw_else_const (POSITION_F (v), Y_POS (v), 2);
    else
    if (BDD_TERM_P (E)) {
      /* Special user defined terminal node. */
      draw_else_term (POSITION_F (v), Y_POS (v));
    }
    else
      draw_else_edge (POSITION_F (v), Y_POS (v),
		      POSITION_F (E), Y_POS (E),
		      BDD_O_INV_EDGE_P (E),
		      BDD_I_INV_EDGE_P (E));
  }
}

static int save_x;
static double incr;
static BDDPTR *f_vec;
static int size;

static void draw_bdds (void)
{
  int i;
  double xf;
  int x = save_x;

  xf = 0.0;

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    draw_func_name (x, i);

    if (BDD_VOID_P (f))
      draw_void (x, 0);
    else
    if (BDD_0_P (f)) {
/*      draw_initial_edge (x, x, 1, 0, 0);*/
      draw_const (x, 0, 0);
    }
    else
    if (BDD_1_P (f)) {
/*      draw_initial_edge (x, x, 1, 0, 0);*/
      draw_const (x, 0, 1);
    }
    else
    if (BDD_X_P (f)) {
/*      draw_initial_edge (x, x, 1, 0, 0);*/
      draw_const (x, 0, 2);
    }
    else
    if (BDD_TERM_P (f)) {
      /* Special user defined terminal node. */
      draw_terminal (x, 0);
    }
    else {
      /* Draw initial edge to top variable node of f: */
      /* First check whether this is a negative edge: */
      /* BDD_O_INV_EDGE_P (f) */

      draw_initial_edge (x, POSITION_F (f), LEVEL_F (f), BDD_O_INV_EDGE_P (f),
			 BDD_I_INV_EDGE_P (f));
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
      bdd_reset_marks (f);
  }
}

void bdd_plot_vec (FILE *fp, BDDPTR *_f_vec, int _size, /* > 0 */
		   int max_x, int max_y,
		   char *(*name_func) (BDDPTR))
{
  int i;

  size = _size;
  f_vec = _f_vec;

  if (!f_vec)
    return;

  plot_name = name_func ? name_func : default_plot_name;

  /* Watch out: terminal don't have valid position. */
  if (size == 1 && !BDD_VOID_P (f_vec[0]) && !BDD_TERM_P (f_vec[0])) {
    save_x = POSITION_F (f_vec[0]);
    incr = 0.0;
  }
  else {
    save_x = 0;
    if (size > max_x) {
      max_x = size;
      incr = 1.0;
    }
    else
      incr = (max_x - 1.0) / (size - 1);
  }

  draw_start (max_x, max_y, draw_bdds);

  event_handler ();

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && !BDD_MARK (f))
      /* Let's be nice (and safe) and clear all aux fields.
	 Already encountered segmentation violation once...
      */
      bdd_traverse_pre (f, bdd_reinit_aux1_and_aux2_action);
  }

  for (i = 0; i < size; i++) {
    BDDPTR f = f_vec[i];

    if (!BDD_VOID_P (f) && BDD_MARK (f))
      /* Let's be nice (and safe) and clear all aux fields.
	 Already encountered segmentation violation once...
      */
      bdd_reset_marks (f);
  }

  draw_finish ();
}

void bdd_plot_vec2 (FILE *fp, BDDPTR *f_vec, int size,
		    int max_x, int max_y,
		    char *(*node_name_func) (BDDPTR),
		    char *(*func_name_func) (int))
{
  plot_func = func_name_func ? func_name_func : default_plot_func;

  bdd_plot_vec (fp, f_vec, size, max_x, max_y, node_name_func);
}

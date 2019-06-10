/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : template.c
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1996 G.L.J.M. Janssen
 date	   : 10-APR-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* This is a template for using the BDD package.

*/

/* Link with bdd.o bdd_fns.o libutils.a
   or create the archive bddlib.a with "make lib" and
   link with the archive. Also link with the archive "libutils.a".
*/

#include <stdio.h>
/* bdd_fns.h already includes bdd.h */
#include "bdd_fns.h"

FILE *global_fp;

static char *tab[] = { "a", "b", "c", "d" };

static void cube_action (int index, int neg, int first)
{
  fprintf (global_fp, " %s%s", tab[index], neg ? "'" : " ");
}

static void elem_action (FILE *fp, void *elem)
{
  BDDPTR cube = (BDDPTR) elem;

  global_fp = fp;
  bdd_traverse_cube (cube, cube_action);
}

static void my_bdd_print (FILE *fp, BDDPTR f, char *name)
{
  BDD_LIST soc = bdd_sum_of_cubes_as_list (f);

  fprintf (fp, "%s\n", name);
  bdd_list_print (fp, "(", soc, elem_action, " | ", ")\n");
}

int main (int argc, char *argv[])
{
  /* Declare some variables that will hold Boolean functions. */
  BDDPTR f;
  BDDPTR a, b, c, d;
  BDDPTR tmp1, tmp2, tmp3;

  /* Set the package parameters to the preferred values: */
  /* Here we rely on the defaults, see bdd.h and bdd_extern.h for
     the various possibilities.
  */

  /* This is a must! And should also be done prior to any BDD operation. */
  BDD_bdd_init ();
  fprintf (stdout, "BDD Package Initialised.\n");
  bdd_print_stats (stdout);

  /* Create some projection functions, i.e. f (a,b,c,d) = a, etc. */
  /* Boolean Variables: a, b, c, d. */
  /* BDD Varid:         0, 1, 2, 3  */
  a = bdd_create_var (0);
  fprintf (stdout, "Created BDD for variable `%s' with rank number %d.\n",
	   tab[0], bdd_top_var_rank (a));

  /* Protecting BDD nodes from garbage collecting can be very tricky.
     In general, all arguments to a bdd_* operation must have been protected.
     BDDPTR return values of all BDD functions in this package
     will be protected. 
     What is returned must thus be bdd_free-d by the user when it is no longer
     needed, especially if it's only an intermediate result. For instance:

   	-- Make sure f1 and f2 are protected! --
   	R = BDD_AND (f1, f2);
	-- If no longer needed, the arguments can be freed: --
	bdd_free (f1);
	bdd_free (f2);

     BDD constants (BDD_0, BDD_1, BDD_X) officially need not be
     protected; it should be done anyway for uniformity and also to make
     sure that the count of dead and alive nodes is correct.

     The number of references (pointers) to a BDD node is counted.
     When a node is created the count is set to 0. Any new reference
     to that node increments the count with 1. The node itself has 2
     children. The count of the children reflects the fact that they are
     referenced by the father node. When a node is freed by bdd_free,
     first it is checked whether its reference count is 0, if so the node
     is considered to be already dead and nothing happens; otherwise
     (count > 0) the reference count is decremented and both its children
     are freed in a recursive fashion. This means that to actually free
     a complete bdd the top node must have a reference count of exactly 1.
     For a life node the following must hold:
     number of BDD_GC_PROTECT's > number of time node is freed.

     It could be useful, especially when one wants to program in a
     functional way, to write some new functions and/or macros that
     encapsulate the standard ones supplied by the package.

     For instance:

     BDDPTR make_var (int i)
     {
       return bdd_freeze (bdd_create_var (i));
     }

     BDDPTR not (BDDPTR f)
     {
       BDDPTR R;

       R = bdd_not (f);
       bdd_free (f);
       return R;
     }

     BDDPTR and (BDDPTR f1, BDDPTR f2)
     {
       BDDPTR R;

       R = bdd_and (f1, f2);
       bdd_free (f1);
       bdd_free (f2);
       return R;
     }

     BDDPTR or (BDDPTR f1, BDDPTR f2)
     {
       BDDPTR R;

       R = bdd_or (f1, f2);
       bdd_free (f1);
       bdd_free (f2);
       return R;
     }

     (f1, f2, f3 must be protected.)
     If you need them again afterwards, protect once more since all
     arguments will be freed by the function calls:
     BDD_GC_PROTECT (f1);
     BDD_GC_PROTECT (f2);
     BDD_GC_PROTECT (f3);
     R = and (or (f1, f2), not (f3))
     R is protected.

     Make BDD for f(a,b,c,d) = (a.b + a'.c)':
     BDD_GC_PROTECT (a);
     BDD_GC_PROTECT (a);
     BDD_GC_PROTECT (b);
     BDD_GC_PROTECT (c);
     R = not (or (and (a, b), and (not (a), c)));

     The easiest way to handle garbage protection is to make sure
     that all the BDD functions you like to keep are frozen in the sense
     that they will never become dead even when explicitly bdd_free-d.
     For instance, usually you like to keep all BDD's for the variables,
     i.e. the values returned by bdd_create_var. The thing to do is
     simply use the function bdd_freeze.

     Example:
     a = make_var (0);
     b = make_var (1);
     c = make_var (2);
     d = make_var (3);
     Now whatever operations you do on these variables they will always
     be protected from garbage collection.
     R = not (or (and (a, b), and (not (a), c)));
     If you want to save this result R then bdd_freeze (R) it.
     FREEZEing the same BDD more than once has no effect.
     However, a frozen BDD can never be actually freed: calling bdd_free
     on it has no effect! (There is also no defrost operation!)
*/

  b = bdd_create_var (1);
  fprintf (stdout, "Created BDD for variable `%s' with rank number %d.\n",
	   tab[1], bdd_top_var_rank (b));
  c = bdd_create_var (2);
  fprintf (stdout, "Created BDD for variable `%s' with rank number %d.\n",
	   tab[2], bdd_top_var_rank (c));
  d = bdd_create_var (3);
  fprintf (stdout, "Created BDD for variable `%s' with rank number %d.\n",
	   tab[3], bdd_top_var_rank (d));

  /* Make BDD for f(a,b,c,d) = (a.b + c.d)': */
  tmp1 = bdd_and (a, b);
  tmp2 = bdd_and (c, d);
  tmp3 = bdd_or (tmp1, tmp2);
  bdd_free (tmp1);
  bdd_free (tmp2);
  f = bdd_not (tmp3);
  bdd_free (tmp3);
  my_bdd_print (stdout, f, "f(a,b,c,d) = (a.b + c.d)':");

  /* Set a = 1 in f: */
  tmp1 = bdd_subst (BDD_1, bdd_top_var_id (a), f);
  bdd_free (f);
  f = tmp1;
  my_bdd_print (stdout, f, "f(a,b,c,d)|a=1 = (b + c.d)':");

  tmp1 = bdd_subst (BDD_0, bdd_top_var_id (b), f);
  bdd_free (f);
  f = tmp1;
  my_bdd_print (stdout, f, "f(a,b,c,d)|a=1|b=0 = (c.d)':");

  tmp1 = bdd_subst (BDD_1, bdd_top_var_id (c), f);
  bdd_free (f);
  f = tmp1;
  my_bdd_print (stdout, f, "f(a,b,c,d)|a=1|b=0|c=1 = d':");

  tmp1 = bdd_subst (BDD_1, bdd_top_var_id (d), f);
  bdd_free (f);
  f = tmp1;
  my_bdd_print (stdout, f, "f(a,b,c,d)|a=1|b=0|c=1|d=1 = 0:");
  bdd_free (f);

  bdd_print_stats (stdout);

  /* Optional: */
  fprintf (stdout, "Quitting the BDD Package.\n");
  bdd_quit ();
  bdd_print_stats (stdout);
  return EXIT_SUCCESS;
}

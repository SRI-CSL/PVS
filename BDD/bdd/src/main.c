/*
 DOCUMENTATION INFORMATION				          module: MAIN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : main.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1990-1998 G.L.J.M. Janssen
 date	   : 29-JAN-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
/*#include <setjmp.h>*/

#include "alloc.h"
#include "hash.h"
#include "bdd_fns.h"
#include "bdd_vfns.h"
#include "appl.h"
#include "bdd_factor.h"

extern int RT_DEBUG;		/* see hash.c */
extern int yyparse ();
extern int yylineno;

/* Program parameter flags: */
int debug = 0;
int dump = 0;
int verbose = 0;
int negate = 0;
int warnings = 1;
int output_as_sum_of_cubes = 0;
int output_as_irr_sum_of_cubes = 0;

bdd_factor_interface *current_interface;

char *filename = "standard input";

static void print_info (BDDPTR *F)
{
  static char buf[BUFSIZ];
  static char buf2[BUFSIZ];
  int size = BDDVEC_SIZE (F);

  if (negate) {
    ComplBDDVec (F);

    if (debug)
      fprintf (stderr, "Negating input formula...done\n");
  }

  /* Testing the dump/load facility: */
  if (dump) {
    /*FILE *tst_fp;*/
    unsigned char *saved;

    /*tst_fp = fopen ("BDDs", "w");*/
    saved = bdd_dump_to_chars_vec (F, size);
    /*fclose (tst_fp);*/
    FreeBDDVec2 (F, size);

    /*tst_fp = fopen ("BDDs", "r");*/
    bdd_restore_from_chars_vec (saved, F, 0);
    bdd_free_dumped_chars (saved);
    /*fclose (tst_fp);*/
  }

  if (debug) {
    int i;

    for (i = 0; i < size; i++) {
      sprintf (buf, "F[%d]", i);
      bdd_print (stderr, F[i], buf);
    }
  }

#ifdef COMMENT
  fprintf (stdout, "/* Sum of Prime Implicants: */\n");
  fprintf (stdout, "/* Irredundant Sum of Prime Implicants: */\n");
#endif

  if (size == 1) {
    BDDPTR f = F[0];

    bdd_print_as_sum_of_cubes (stdout, f, output_as_irr_sum_of_cubes);

    /* Note: we don't count the dummy 0 variable! */
    fprintf (stdout, "/* Size: %d, (%s minterms, %s X terms, %d vars), Depth: %d, Alive: %d */\n",
	     bdd_size (f),
	     D_sprintf (buf, bdd_count_sat_assignments (f, BDD_0), 0),
	     D_sprintf (buf2, bdd_count_X_terms (f), 0),
	     bdd_nr_vars,
	     bdd_depth (f),
	     bdd_nodes_alive ());
  }
  else
    bdd_print_vec_as_sum_of_cubes (stdout, F, size,
				   output_as_irr_sum_of_cubes);

  /* Testing the factor routine:
  bdd_factor_vec (F, size);
  */

  FreeBDDVec (F);

  if (verbose)
    bdd_print_stats (stderr);
}

/* Called by parser after successfully parsing 1 formula. */
void action (BDDPTR *F)
{
  if (verbose)
    fprintf (stdout, "Parsed %d line%s of file %s\n",
	     yylineno, yylineno != 1 ? "s" : "", filename);

  print_info (F);
/*  reinit_hashtab (var_table);*/

  if (debug) fprintf (stderr, "Cleaning up...\n");
  if (debug) fprintf (stderr, "Parsing...\n");
}

/* Called by parser after successfully parsing whole file. */
void parse_complete (void)
{
  if (verbose)
    bdd_print_stats (stderr);

  if (debug)
    print_hashtab (stderr, var_table);

  if (verbose)
    fprintf (stdout, "Processing file %s...done (%d line%s)\n",
	     filename, yylineno, yylineno != 1 ? "s" : "");
}

/*static jmp_buf Context;*/

void my_memfull_handler (void)
{
  fprintf (stderr, "BDD Package Memory Limit (%d Kb) Exceeded.\n",
	   bdd_memsize_limit () >> 10);

  exit (1);
/*  _longjmp (Context, 1);*/
}

/* Our language has following priorities of operators: */
static int prec[4] =
{
/* BDD_NO_OP	0 */ 0,
/* BDD_OR_OP	1 */ 2,
/* BDD_XOR_OP	2 */ 1,
/* BDD_AND_OP	3 */ 3
};

static void bdd_print_left (int op, int context)
{
  if (prec[op] < prec[context])
    fputc ('(', current_interface->out);
}

static void bdd_print_right (int op, int context)
{
  if (prec[op] < prec[context])
    fputc (')', current_interface->out);
}

int main (int argc, char *argv[])
{
  extern char *optarg;
  extern int optind;
  int option;
  char *opt_str = "dDgGhiIl:L:M:nN:oSt:vVw";
  char usage_str[80];
  int stdin_fd = dup (fileno (stdin));

  /* Install memory limit handler: */
  bdd_set_memsize_limit_and_handler (INT_MAX, my_memfull_handler);

  sprintf (usage_str, "usage: %%s [ -%s ] [ f1 f2 ... fn ]\n", opt_str);

  /* Process arguments: */
  while ((option = getopt (argc, argv, opt_str)) != EOF)
    switch (option) {
    case 'd':
      debug = verbose = bdd_verbose = RT_DEBUG = 1;
      break;

    case 'D':
      dump = 1;
      break;

    case 'g':
      bdd_do_gc = 0;
      break;

    case 'G':
      bdd_dyna_monitor = 1;
      break;

    case 'h':
fprintf (stdout, usage_str, argv[0]);
fprintf (stdout,
"-d    : prints debug info to stderr; implies -v ON.\n"
"-g    : inhibits garbage collection.\n"
"-h    : prints just this text to stdout and stops.\n"
"-i    : don't use input inverters in the BDDs.\n"
"-I    : outputs functions as irredundant sum-of-cubes to stdout.\n"
"-l<n> : set load factor for BDD unique table (4).\n"
"-M<n> : sets memory limit (in Kbytes).\n"
"-n    : negates formula.\n"
"-N<n> : sets BDD nodes limit (default none).\n"
"-o    : don't use output inverters in the BDDs.\n"
"-t<n> : set initial BDD unique table size (12799).\n"
"-S    : do not use sum-of-cubes cache.\n"
"-v    : verbose, prints action summary to stdout.\n"
"-V    : switches off dynamic variable ordering.\n"
"-w    : inhibits warning messages.\n");
      return 0;

    case 'i':
      bdd_use_inv_edges = 0;
      break;

    case 'I':
      output_as_irr_sum_of_cubes = 1;
      break;

    case 'l':
      BDD_LOAD_FACTOR = atoi (optarg);
      if (verbose)
	fprintf (stdout, "Load factor set to: %d\n",
		 BDD_LOAD_FACTOR);
      break;

    case 'L':
      {
	int limit = atoi (optarg);

	bdd_size_limit = limit;
	if (verbose)
	  fprintf (stdout, "BDD size limit set to: %d nodes\n", limit);
      }
      break;

    case 'M':
      {
	int limit = atoi (optarg) << 10;

	bdd_set_memsize_limit_and_handler (limit, 0);
	if (verbose)
	  fprintf (stdout, "Memory limit set to: %d Kbytes\n", limit >> 10);
      }
      break;

    case 'n':
      negate = 1;
      break;

    case 'N':
      {
	int limit = atoi (optarg);

	bdd_nr_nodes_allowed = limit;
	if (verbose)
	  fprintf (stdout, "Max. number BDD nodes set to: %d.\n", limit);
      }
      break;

    case 'o':
      bdd_use_neg_edges = 0;
      break;

    case 'S':
      bdd_use_sop_cache_switch (0);
      break;

    case 't':
      BDD_COMPUTED_TABLE_SIZE = atoi (optarg);
      if (verbose)
	fprintf (stdout, "Initial computed table size set to: %d\n",
		 BDD_COMPUTED_TABLE_SIZE);
      break;

    case 'v':
      verbose = bdd_verbose = 1;
      break;

    case 'V':
      bdd_do_dynamic_ordering = 0;
      break;

    case 'w':
      warnings = 0;
      break;

    case '?':
    default:
      fprintf (stderr, "Error: unknown option. Stop.\n");
      fprintf (stderr, usage_str, argv[0]);
      return 1;
    } /*switch-while*/

  var_table = make_hashtab (3);
  /* Let's not use entry nr. 0 for fun: */
/*  lookup (var_table, "", 0, NULL, INSERT);*/

  aux_table = make_hashtab (0);

/*  if (!_setjmp (Context))*/
  if (1) {

    bdd_init ();

    current_interface = bdd_get_factor_interface ();
/*    current_interface->out = stdout;*/
    current_interface->handle_lit   = bdd_print_var_name;
    current_interface->handle_left  = bdd_print_left;
    current_interface->handle_right = bdd_print_right;

    current_interface->use_impl_check = 1;
    current_interface->maxI = -1;
/*
    current_interface->A[0].in = 1000;
    current_interface->A[0].limitsize = 1000;
    current_interface->A[1].in = 1000;
    current_interface->A[1].limitsize = 1000;
    current_interface->A[2].in = 1000;
    current_interface->A[2].limitsize = 1000;
    current_interface->A[3].in = 1000;
    current_interface->A[3].limitsize = 1000;
*/
    current_interface->style = 0;

    /* Create the dummy 0 var: */
/*    bdd_free (bdd_create_var_last ());*/

#ifdef COMMENT
    {				/* Testing ranking of variables. */
      int i;

      for (i = 0; i < 10; i++) {
	BDDPTR R;

	fprintf (stderr, "i = %d.\n", i);
	R = bdd_create_var_first ();
	bdd_print (stderr, R, "New First");
	bdd_free (R);
	R = bdd_create_var_last ();
	bdd_print (stderr, R, "New Last");
	bdd_free (R);
      }
    }
#endif

    if (optind == argc)
      goto doit;

    do {
      filename = argv[optind];
      if (!strcmp (filename, "-")) {
	/* Read from stdin. */
	dup2 (stdin_fd, 0);
	filename = "standard input";
      }
      else
      if (!freopen (filename, "r", stdin)) {
	fprintf (stderr, "Cannot read file: %s\n", filename);
	continue;
      }

      doit:
	if (verbose) fprintf (stdout, "Processing file %s...\n", filename);

	if (yyparse () == 1)
	  fprintf (stderr, "Syntax error in: %s\n", filename);

      if (optind + 1 < argc) {
	/* Reinit: */
	yylineno = 1;

#ifdef COMMENT
	/* Just for testing purposes: */
	bdd_quit ();
	bdd_init ();
	free_hashtab (var_table);
	var_table = make_hashtab (3);
	/* Let's not use entry nr. 0 for fun: */
/*	lookup (var_table, "", 0, NULL, INSERT);*/
	free_hashtab (aux_table);
	aux_table = make_hashtab (0);
	/* Create the dummy 0 var: */
/*	bdd_free (bdd_create_var_last ());*/
#endif
      }
    } while (++optind < argc);
  }
  else {
    fprintf (stderr, "Quitting package.\n");
    bdd_quit ();
    return 1;
  }
  return 0;
}

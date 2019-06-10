/*
 DOCUMENTATION INFORMATION				          module: MAIN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : main.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1991-1998 G.L.J.M. Janssen
 date	   : 16-APR-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>

#include "alloc.h"
#include "hash.h"
#include "bdd_fns.h"
#include "bdd_vfns.h"
#include "appl.h"
#include "plot.h"

extern int RT_DEBUG;		/* see hash.c */
extern int yyparse ();
extern int yylineno;

/* Program parameter flags: */
int debug = 0;
int dump = 0;
int verbose = 0;
int negate = 0;
int warnings = 1;
int compact = 1;
int charsinname = 3;
int output_as_irr_sum_of_cubes = 0;
int rank_leveling = 0;

char *filename = "standard input";

static char *plot_name (BDDPTR v)
{
  int id = BDD_VARID (v);
  static char buf[16];

  /* Watch out when id is not associated with a name in the
     var_table. This could happen when generating nameless bdd labels.
     The macro VAR_NAME may then not be used.
  */
  if (id < var_table->size && OCCUPIED_BUCKET (var_table, id)) {
    char *name = VAR_NAME (id);

    if (strlen (name) <= charsinname)
      return name;
  }
  sprintf (buf, "%d", id);
  return buf;
}

static void print_info (BDDPTR *F)
{
  int size = BDDVEC_SIZE (F);

  if (negate) {
    ComplBDDVec (F);

    if (debug)
      fprintf (stderr, "Negating input formula...done\n");
  }

/*
  if (debug) {
    bdd_print (stderr, f, "BDD");
    bdd_print_as_sum_of_cubes (stdout, f, output_as_irr_sum_of_cubes);
  }
*/

  fprintf (stderr, "/* Size: %d */\n", bdd_size_vec (F, size));

  {
    int max_x, max_y;

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

    if (verbose)
      fprintf (stderr, "Start BDD graph placement...");
    bdd_prepare_for_plot_vec (F, size, &max_x, &max_y, compact, rank_leveling);
    if (verbose)
      fprintf (stderr, "done.\n");

    if (verbose)
      fprintf (stderr, "Start BDD graph plotting...");
    bdd_plot_vec2 (stdout, F, size, max_x, max_y, plot_name, 0);
    if (verbose)
      fprintf (stderr, "done.\n");

    if (!bdd_use_inv_edges) {
    bdd_do_dynamic_ordering = 1;
    bdd_dynamic_order_exhaustive ();
    bdd_do_dynamic_ordering = 0;

    fprintf (stderr, "/* Size: %d */\n", bdd_size_vec (F, size));

    if (verbose)
      fprintf (stderr, "Start BDD graph placement...");
    bdd_prepare_for_plot_vec (F, size, &max_x, &max_y, compact, rank_leveling);
    if (verbose)
      fprintf (stderr, "done.\n");

    if (verbose)
      fprintf (stderr, "Start BDD graph plotting...");
    bdd_plot_vec2 (stdout, F, size, max_x, max_y, plot_name, 0);
    if (verbose)
      fprintf (stderr, "done.\n");
  }
  }

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
    fprintf (stdout, "Processing file %s...done (%d line%s)\n",
	     filename, yylineno, yylineno != 1 ? "s" : "");
}

int main (int argc, char *argv[])
{
  extern char *optarg;
  extern int opterr;
  extern int optind;
  extern FILE *freopen ();
  int option;
  char *opt_str = "cdDghiIl:L:M:nN:ort:vVw";
  char usage_str[80];
  int stdin_fd = dup (fileno (stdin));

  sprintf (usage_str, "usage: %%s [ -%s ] [ f1 f2 ... fn ]\n", opt_str);

  /* Process arguments: */
  while ((option = getopt (argc, argv, opt_str)) != EOF)
    switch (option) {
    case 'c':
      compact = 0;
      break;

    case 'd':
      debug = verbose = bdd_verbose = RT_DEBUG = 1;
      break;

    case 'D':
      dump = 1;
      break;

    case 'g':
      bdd_do_gc = 0;
      break;

    case 'h':
fprintf (stdout, usage_str, argv[0]);
fprintf (stdout,
"-c    : inhibits compaction of the BDD graph drawing.\n"
"-d    : prints debug info to stderr; implies -v ON.\n"
"-g    : inhibits garbage collection.\n"
"-h    : prints just this text to stdout and stops.\n"
"-i    : don't use input inverters in the BDDs.\n"
"-I    : outputs functions as irredundant sum-of-cubes to stdout.\n"
"-l<n> : set load factor for BDD unique table (4).\n"
"-M<n> : sets memory limit (in Kbytes).\n"
"-N<n> : set nr. chars printed for variable names (3).\n"
"-n    : negates formula.\n"
"-o    : don't use output inverters in the BDDs.\n"
"-r    : use node leveling algorithm based on ranks.\n"
"-t<n> : set initial BDD unique table size (12799).\n"
"-v    : verbose, prints action summary to stdout.\n"
"-V    : switches off dynamic variable ordering.\n"
"-w    : inhibits warning messages.\n");
      exit (0);

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
      charsinname = atoi (optarg);
      break;

    case 'o':
      bdd_use_neg_edges = 0;
      break;

    case 'r':
      rank_leveling = 1;
      break;

    case 't':
      BDD_COMPUTED_TABLE_SIZE = atoi (optarg);
      if (verbose)
	fprintf (stdout, "Initial table size set to: %d\n",
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
    usage:
      fprintf (stderr, usage_str, argv[0]);
      exit (1);
    }

  var_table = make_hashtab (3);
  /* Let's not use entry nr. 0 for fun: */
/*  lookup (var_table, "", 0, NULL, INSERT);*/

  aux_table = make_hashtab (0);
/*  bdd_sizeof_user_data = sizeof (XYPOS);*/
  BDD_bdd_init ();

  /* Create the dummy 0 var: */
/*  bdd_free (bdd_create_var_last ());*/

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

      bdd_do_dynamic_ordering = 0;

      if (yyparse () == 1)
	fprintf (stderr, "Syntax error in: %s\n", filename);

    /* Reinit: */
    yylineno = 1;

  } while (++optind < argc);
  return 0;
}

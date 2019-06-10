/*
 DOCUMENTATION INFORMATION				          module: MAIN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S750
 file	   : main.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1992-1995 G.L.J.M. Janssen
 date	   : 27-JUL-1995
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>

#include "alloc.h"
#include "hash.h"
#include "bdd_fns.h"

#include "mu.h"

extern int yyparse ();
extern int yylineno;

/* Program parameter flags: */
int debug = 0;
int negate = 0;
int warnings = 1;

char *filename = "stdin";

int main (int argc, char *argv[])
{
  extern char *optarg;
  extern int opterr;
  extern int optind;
  extern FILE *freopen ();
  int option;
  char *opt_str = "acdeghiIl:M:N:oPrsSt:vVw";
  char usage_str[80];
  int stdin_fd = dup (fileno (stdin));

  sprintf (usage_str, "usage: %%s [ -%s ] [ f1 f2 ... fn - ]\n", opt_str);

  /* Process arguments: */
  while ((option = getopt (argc, argv, opt_str)) != EOF)
    switch (option) {
    case 'a':
      mu_use_and_smooth = 1;
      break;

    case 'c':
      mu_use_constrain = 1;
      break;

    case 'd':
      mu_debug = mu_verbose = bdd_verbose = mu_echo = 1;
      break;

    case 'e':
      mu_echo = 1;
      break;

    case 'g':
      bdd_do_gc = 0;
      break;

    case 'h':
fprintf (stdout, usage_str, argv[0]);
fprintf (stdout,
"-a    : use and_smooth operation when applicable.\n"
"-c    : use constrain.\n"
"-d    : prints debug info to stderr; implies -v ON.\n"
"-e    : causes input to be echoed to stdout.\n"
"-g    : inhibits garbage collection.\n"
"-h    : prints just this text to stdout and stops.\n"
"-i    : don't use input inverters in the BDDs.\n"
"-I    : outputs functions as irredundant sum-of-cubes to stdout.\n"
"-l<n> : set load factor for BDD unique table (4).\n"
"-M<n> : sets memory limit (in Kbytes).\n"
"-N<n> : sets BDD nodes limit (default none).\n"
"-o    : don't use output inverters in the BDDs.\n"
"-r    : use restrict.\n"
"-s    : do frontier set simplification during Reachable calculation.\n"
"-S    : do not use sum-of-cubes cache.\n"
"-t<n> : set initial BDD unique table size (12799).\n"
"-v    : verbose, prints action summary to stdout.\n"
"-V    : switches dynamic variable ordering OFF.\n"
"-w    : inhibits warning messages.\n");
      return 0;

    case 'i':
      bdd_use_inv_edges = 0;
      break;

    case 'I':
      break;

    case 'l':
      BDD_LOAD_FACTOR = atoi (optarg);
      if (mu_verbose)
	fprintf (stdout, "Load factor set to: %d\n",
		 BDD_LOAD_FACTOR);
      break;

    case 'M':
      {
	int limit = atoi (optarg) << 10;

	bdd_set_memsize_limit_and_handler (limit, 0);
	if (mu_verbose)
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
	if (mu_verbose)
	  fprintf (stdout, "Max. number BDD nodes set to: %d.\n", limit);
      }
      break;

    case 'o':
      bdd_use_neg_edges = 0;
      break;

    case 'P':
      bdd_dyna_monitor = 1;
      break;

    case 'r':
      mu_use_restrict = 1;
      break;

    case 's':
      mu_simplify_frontier = 1;
      break;

    case 'S':
      bdd_use_sop_cache_switch (0);
      break;

    case 't':
      BDD_COMPUTED_TABLE_SIZE = atoi (optarg);
      if (mu_verbose)
	fprintf (stdout, "Initial computed table size set to: %d\n",
		 BDD_COMPUTED_TABLE_SIZE);
      break;

    case 'v':
      mu_verbose = 1;
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
      return 1;
    }

  BDD_bdd_init ();
  mu_init ();

  if (optind == argc)
    goto doit;

  do {
    filename = argv[optind];
    if (!strcmp (filename, "-")) {
      /* Read from stdin. */
      dup2 (stdin_fd, 0);
      filename = "stdin";
    }
    else {
      if (!freopen (filename, "r", stdin)) {
	fprintf (stderr, "Cannot read file: %s\n", filename);
	continue;
      }
    }

  doit:
    if (mu_verbose) fprintf (stdout, "Processing file %s...\n", filename);

  restart:
    if (yyparse () == 1 && !feof (stdin)) {
/*      fprintf (stderr, "Syntax error in: %s\n", filename);*/
      goto restart;
    }

    if (mu_verbose) {
      bdd_print_stats (stderr);
      mu_print_stats (stderr);
    }

    /* Reinit: */
    yylineno = 1;

  } while (++optind < argc);

  mu_quit ();
  bdd_quit ();

  return 0;
}

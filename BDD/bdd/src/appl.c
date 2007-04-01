/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : appl.c
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1996 G.L.J.M. Janssen
 date	   : 10-APR-1996
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include <stdio.h>

#include "alloc.h"
#include "hash.h"
#include "bdd_fns.h"
#include "appl.h"

extern int debug;		/* see main.c */
extern int warnings;		/* see main.c */

#define FALSE		0
#define TRUE		1
#define DONTCARE	2

HASHTAB *var_table;		/* primary variable names */
HASHTAB *aux_table;		/* secondary (function) names */

/* Number of primary variables in user formula: */
int var_count = 0;
/* Number of variables introduced by LET construct: */
int def_count = 0;

#define POSTFIX_NOT_STR		bdd_output_strings[BDD_POSTFIX_NOT_S]
#define NOT_STR			bdd_output_strings[BDD_NOT_S]
#define AND_STR			bdd_output_strings[BDD_AND_S]
#define OR_STR			bdd_output_strings[BDD_OR_S]
#define VOID_STR		bdd_output_strings[BDD_VOID_S]
#define TRUE_STR		bdd_output_strings[BDD_TRUE_S]
#define FALSE_STR		bdd_output_strings[BDD_FALSE_S]
#define X_STR			bdd_output_strings[BDD_X_S]
#define BEG_STR			bdd_output_strings[BDD_BEG_S]
#define END_STR			bdd_output_strings[BDD_END_S]
#define FILL_STR		bdd_output_strings[BDD_FILL_S]
#define LPAR_STR		bdd_output_strings[BDD_LPAR_S]
#define RPAR_STR		bdd_output_strings[BDD_RPAR_S]
#define SEP_STR			bdd_output_strings[BDD_SEP_S]

static const char *bdd_output_strings[] = {
/* POSTFIX_NOT_STR */		"'",
/* NOT_STR */			"!",
/* AND_STR */			" ",
/* OR_STR */			"\n+ ",
/* VOID_STR */			"void",
/* TRUE_STR */			"1",
/* FALSE_STR */			"0",
/* X_STR */			"X",
/* BEG_STR */			"  ",
/* END_STR */			".\n",
/* FILL_STR */			" ",
/* LPAR_STR */			"[ ",
/* RPAR_STR */			" ]",
/* SEP_STR */			", "
};

void bdd_set_output_string (int idx, const char *str)
{
  bdd_output_strings[idx] = str;
}

const char *bdd_get_output_string (int idx)
{
  return bdd_output_strings[idx];
}

static const char *bdd_var_name (int index)
{
  char *name;

  /* Watch out when index is not associated with a name in the
     var_table. This could happen when generating nameless bdd labels.
     The macro VAR_NAME may then not be used.
  */
  if (index >= var_table->size || EMPTY_BUCKET (var_table, index)) {
    /* Think up dummy name, say _<index>, but no check is made whether
       this name already exists!
    */
    static char buf[16];

    sprintf (buf, "_%d", index);
    name = buf;
  }
  else
    name = VAR_NAME (index);

  return name;
}

/* Returns BDD for this variable. Secondary variables have priority. */
BDDPTR var_access (char *s, int len)
{
  int index;
  union bddptr info = { BDD_VOID };
  int flag;

  /* LET introduced names come first! */
  if (lookup (aux_table, s, len, &info.voidptr, LOOKUP) != NOT_PRESENT) {
    /* This name is indeed used for a secondary variable. */
    if (!BDD_VOID_P (info.bddptr)) {
      BDD_GC_PROTECT (info.bddptr);
      return info.bddptr;
    }
    /* No definition established yet. */
    if (warnings)
    fprintf (stderr,
     "Warning: defining secondary variable with same name %s.\n", s);
  }

  /* Here define as primary variable: */
  flag = (int) INSERT;
  index = lookup (var_table, s, len, NULL, &flag);
  if (flag == INDEED_INSERTED) {
    var_count++;
    return bdd_create_var_last ();
  }
  return bdd_create_var (index);
}

BDDPTR make_user_var (char *s, int len)
{
  int index;
  int flag;

  if (lookup (aux_table, s, len, NULL, LOOKUP) != NOT_PRESENT)
    if (warnings)
    fprintf (stderr,
     "Warning: secondary variable %s already exists.\n", s);

  flag = (int) INSERT;
  index = lookup (var_table, s, len, NULL, &flag);
  if (flag == INDEED_INSERTED) {
    var_count++;
    return bdd_create_var_last ();
  }
  return bdd_create_var (index);
}

int make_sub_var (char *s, int len)
{
  if (lookup (var_table, s, len, NULL, LOOKUP) != NOT_PRESENT)
    if (warnings)
    fprintf (stderr,
     "Warning: primary variable %s already exists.\n", s);

  return lookup (aux_table, s, len, NULL, INSERT);
}

BDDPTR make_definition (int id_index, BDDPTR function)
{
  if (debug) {
    fprintf (stderr, "Defining %s as ", DEF_NAME (id_index));
    bdd_print (stderr, function, NULL);
  }

  if (KEYINFO (aux_table, id_index)) {
    if (warnings)
    fprintf (stderr,
	     "Warning: redefining %s.\n", DEF_NAME (id_index));
    bdd_free ((BDDPTR) KEYINFO (aux_table, id_index));
  }
  else
    def_count++;

  KEYINFO (aux_table, id_index) = bdd_assign (function);

  return bdd_assign (function);
}

static char *tvalue[] = { "0", "1", "X" };
FILE *bdd_output_stream; /* = stdout; 1; */

static void bdd_output_stream_construct (void) __attribute__((constructor));
static void bdd_output_stream_construct (void) { bdd_output_stream = stdout; }

static void sat_1 (BDDPTR f, BYTE *pi, int negate_result)
{
  BDDPTR temp;

  if (BDD_1_P (f)) {
    if (!negate_result) {
      int i;

      fputs ("Satisfying truth-assignment is:\n", bdd_output_stream);
      for (i = 0; i < var_count; i++) {
	fprintf (bdd_output_stream, "%s\t:= %s\n",
		 bdd_var_name (i), tvalue [pi[i]]);
      }
    }
    return;
  }

  /* Only happens when !use_neg_edges. */
  if (BDD_0_P (f)) return;

  if (BDD_X_P (f)) return;

  /* Assign a value: */
  pi[BDD_VARID (f)] = BDD_I_INV_EDGE_P (f) ? TRUE : FALSE;

  temp = BDD_ELSE (f);
  if (BDD_NEG_P (temp)) {
    sat_1 (BDD_O_OFF (temp), pi, !negate_result);
  }
  else
    sat_1 (temp, pi, negate_result);

  /* Not successful, try assigning the opposite: */
  pi[BDD_VARID (f)] = BDD_I_INV_EDGE_P (f) ? FALSE : TRUE;

  temp = BDD_THEN (f);
  /* Can never have BDD_NEG_P (temp)! */
  sat_1 (temp, pi, negate_result);

  pi[BDD_VARID (f)] = DONTCARE;
}

void print_sat_assignment (FILE *fp, BDDPTR f)
{
  BYTE *pi;
  int i;
  int negate_result;

  bdd_output_stream = fp;

  pi = MALLOC_ARRAY (var_count, BYTE);
  for (i = 0; i < var_count; i++)
    pi[i] = DONTCARE;

  if (BDD_NEG_P (f)) {
    negate_result = 1;
    f = BDD_O_OFF (f);
  }
  else
    negate_result = 0;

  sat_1 (f, pi, negate_result);

  free (pi);
}

void bdd_print_var_name (int v, int neg)
{
  fputs (bdd_var_name (v), bdd_output_stream);
  fputs (neg ? POSTFIX_NOT_STR : FILL_STR, bdd_output_stream);
}

static void default_print_cube_action (int index, int neg, int first)
{
  if (!first)
    fputs (AND_STR, bdd_output_stream);

  bdd_print_var_name (index, neg);
}

void (*bdd_print_cube_action) (int index, int neg, int first) =
     default_print_cube_action;

/* Assumes cube is indeed a BDD cube. */
static void print_cube (FILE *fp, BDDPTR cube)
{
  bdd_output_stream = fp;
  bdd_traverse_cube (cube, bdd_print_cube_action);
}

void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant)
{
  fputs (BEG_STR, fp);

  if (BDD_VOID_P (f))
    fputs (VOID_STR, fp);
  else
  if (BDD_0_P (f))
    fputs (FALSE_STR, fp);
  else
  if (BDD_1_P (f))
    fputs (TRUE_STR, fp);
  else
  if (BDD_X_P (f))
    fputs (X_STR, fp);
  else {
    BDD_LIST list;

    list = irredundant ? bdd_irredundant_sum_of_cubes_as_list (f)
	               :             bdd_sum_of_cubes_as_list (f);
    /* Could return empty list because of don't cares in f. */
    if (list) {

      bdd_list_print (fp, "", list, (void (*)(FILE *, void *)) print_cube,
		      (char *) OR_STR, "");

      bdd_list_free (list, (void (*)(void *)) bdd_free);
    }
    else
      fputs (FALSE_STR, fp);
  }
  fputs (END_STR, fp);
}

void bdd_print_vec_as_sum_of_cubes (
     FILE *fp,
     BDDPTR *f_vec,
     int size,			/* > 0 */
     int irredundant)
{
  if (!f_vec || size <= 0) {
    fputs ("/* Oops, NULL BDD Vector. */\n", fp);
    return;
  }
    
  /* From MSB to LSB. */
  fputs (BEG_STR, fp);
  fputs (LPAR_STR, fp);
  while (size--) {
    BDDPTR f = *f_vec++;

    if (BDD_VOID_P (f))
      fputs (VOID_STR, fp);
    else
    if (BDD_0_P (f))
      fputs (FALSE_STR, fp);
    else
    if (BDD_1_P (f))
      fputs (TRUE_STR, fp);
    else
    if (BDD_X_P (f))
      fputs (X_STR, fp);
    else {
      BDD_LIST list;

      fprintf (fp, "%s",
	       bdd_has_dontcare (f) ? "/* Note: X interpreted as 0 */" : "");
      if (irredundant)
	list = bdd_irredundant_sum_of_cubes_as_list (f);
      else
	list =             bdd_sum_of_cubes_as_list (f);

      /* Could return empty list because of don't cares in f. */
      if (list) {

	bdd_list_print (fp, "", list, (void (*)(FILE *, void *)) print_cube,
			(char *) OR_STR, "");

	bdd_list_free (list, (void (*)(void *)) bdd_free);
      }
      else
	fputs (FALSE_STR, fp);
    }
    if (size) fputs (SEP_STR, fp);
  }
  fputs (RPAR_STR, fp);
  fputs (END_STR, fp);
}

static void in_ite_form (FILE *fp, BDDPTR f)
{
  if (BDD_0_P (f))
    fputs (FALSE_STR, fp);
  else
  if (BDD_1_P (f))
    fputs (TRUE_STR, fp);
  else
  if (BDD_X_P (f))
    fputs (X_STR, fp);
  else {
    fprintf (fp, "ite (%s%s, ",
	     bdd_var_name (BDD_VARID (f)),
	     BDD_I_INV_EDGE_P (f) ? POSTFIX_NOT_STR : "");
    in_ite_form (fp, BDD_THEN (f));
    fputs (", ", fp);
    in_ite_form (fp, BDD_ELSE (f));
    fprintf (fp, ")%s", BDD_NEG_P (f) ? POSTFIX_NOT_STR : "");
  }
}

void bdd_print_in_ite_form (FILE *fp, BDDPTR f)
{
  fputs (BEG_STR, fp);
  if (BDD_VOID_P (f))
    fputs (VOID_STR, fp);
  else
    in_ite_form (fp, f);
  fputs (END_STR, fp);
}

/* Like to have routine to print BDD in gate file format, i.e.
   a list of logic expressions in terms of primary inputs and
   auxiliary variables.
*/

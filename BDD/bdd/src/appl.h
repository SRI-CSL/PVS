/*
 DOCUMENTATION INFORMATION				         module: BDD
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system	   : Apollo DN3000, HP 9000/S500
 file	   : appl.h
 unit-title: 
 ref.	   : Efficient Implementation of a BDD Package, Karl S. Brace. DAC'90
 author(s) : Copyright (c) 1990-1994 G.L.J.M. Janssen
 date	   : 12-OCT-1994
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#define BDD_POSTFIX_NOT_S	0
#define BDD_NOT_S		1
#define BDD_AND_S		2
#define BDD_OR_S		3
#define BDD_VOID_S		4
#define BDD_TRUE_S		5
#define BDD_FALSE_S		6
#define BDD_X_S			7
#define BDD_BEG_S		8
#define BDD_END_S		9
#define BDD_FILL_S	       10
#define BDD_LPAR_S	       11
#define BDD_RPAR_S	       12
#define BDD_SEP_S	       13

/* Note: use only when `i' is sure to be index of OCCUPIED_BUCKET in table! */
#define VAR_NAME(i)		KEYSTR (var_table, i)
#define VAR_NAME_LEN(i)		KEYLEN (var_table, i)
#define DEF_NAME(i)		KEYSTR (aux_table, i)
#define DEF_NAME_LEN(i)		KEYLEN (aux_table, i)

extern int var_count;
extern int def_count;
extern HASHTAB *var_table;
extern HASHTAB *aux_table;

extern FILE *bdd_output_stream;

extern void bdd_set_output_string (int idx, const char *str);
extern const char *bdd_get_output_string (int idx);

extern BDDPTR var_access (char *name, int len);
extern BDDPTR make_user_var (char *name, int len);
extern int make_sub_var (char *name, int len);
extern BDDPTR make_definition (int index, BDDPTR f);
extern void print_sat_assignment (FILE *fp, BDDPTR f);
extern void bdd_print_var_name (int v, int neg);
extern void (*bdd_print_cube_action) (int index, int neg, int first);
extern void bdd_print_as_sum_of_cubes (FILE *fp, BDDPTR f, int irredundant);
extern void bdd_print_vec_as_sum_of_cubes
            (FILE *fp, BDDPTR *f_vec, int size, int irredundant);
extern void bdd_print_in_ite_form (FILE *fp, BDDPTR f);

#include "bdd_fns.h" 
#include "mu.h"

Formula mu___mu_mk_false_formula (void) {return mu_mk_false_formula ();}

Formula mu___mu_mk_true_formula (void) {return mu_mk_true_formula ();}

Formula mu___mu_mk_bool_var (char *name) {return mu_mk_bool_var (name);}

int mu___mu_check_bool_var (char *var) {return mu_check_bool_var (var);}

Formula mu___mu_check_mk_bool_var (char *name)
{return (Formula) mu_check_mk_bool_var (name);}
 
Formula mu___mu_mk_ite_formula (Formula cond, Formula then_part,
				Formula else_part)
  {return mu_mk_ite_formula (cond, then_part, else_part);}

Formula mu___mu_mk_curry_application (Term fml1, LIST listargs)
{return (Formula) mu_mk_curry_application (fml1, listargs);}
 
Formula mu___mu_mk_application (Term R, LIST subs, int curried)
  {return mu_mk_application (R, subs, curried);}

Formula mu___mu_mk_forall (LIST listvars, Formula fml)
{return (Formula) mu_mk_forall (listvars, fml);}
 
Formula mu___mu_mk_exists (LIST listvars, Formula fml)
  {return (Formula) mu_mk_exists (listvars, fml);}
 
Formula mu___mu_mk_implies_formula (Formula fml1, Formula fml2)
  {return (Formula) mu_mk_implies_formula (fml1, fml2);}
 
Formula mu___mu_mk_equiv_formula (Formula fml1, Formula fml2)
  {return (Formula) mu_mk_equiv_formula (fml1, fml2);}
 
Formula mu___mu_mk_or_formula  (Formula fml1, Formula fml2)
  {return (Formula) mu_mk_or_formula (fml1, fml2);}
 
Formula mu___mu_mk_and_formula (Formula fml1, Formula fml2)
  {return (Formula) mu_mk_and_formula (fml1, fml2);}
 
Formula mu___mu_mk_not_formula (Formula fml1)
  {return (Formula) mu_mk_not_formula (fml1);}
 
Formula mu___mu_mk_cofactor (Formula fml1 , Formula fml2)
  {return (Formula) mu_mk_cofactor (fml1, fml2);}
 
Term mu___mu_mk_abstraction (LIST vars, Formula f1)
  {return mu_mk_abstraction (vars, f1);}

Term mu___mu_mk_l_fixed_point (int relvar, Term fml1)
  {return (Term) mu_mk_l_fixed_point (relvar, fml1);}
 
Term mu___mu_mk_g_fixed_point (int relvar, Term fml1)
{return (Term) mu_mk_g_fixed_point (relvar, fml1);}
 
Term mu___mu_mk_reach (Term Next, Term S0, Term Inv)
  {return mu_mk_reach (Next, S0, Inv);}

int mu___mu_mk_rel_var_dcl (char *name) {return mu_mk_rel_var_dcl (name);}

Term mu___mu_mk_rel_var_ (char *name) {return (Term) mu_mk_rel_var_ (name);}

Term mu___mu_mk_true_term (void) {return mu_mk_true_term ();}

Term mu___mu_mk_false_term (void) {return mu_mk_false_term ();}

Term mu___mu_mk_not_term (Term fml1) {return (Term) mu_mk_not_term (fml1);}
 
Term mu___mu_mk_and_term (Term fml1, Term fml2)
  {return (Term) mu_mk_and_term (fml1, fml2);}
 
Term mu___mu_mk_or_term (Term fml1, Term fml2)
  {return (Term) mu_mk_or_term (fml1, fml2);}
 
Term mu___mu_mk_equiv_term (Term fml1, Term fml2)
  {return (Term) mu_mk_equiv_term (fml1, fml2);}
 
Term mu___mu_mk_implies_term (Term fml1, Term fml2)
  {return (Term) mu_mk_implies_term (fml1, fml2);}

char* mu___get_mu_bool_var_name (bdd_idx)
  {return (char *) get_mu_bool_var_name (bdd_idx);}

LIST mu___append_cont(void *p, LIST list)
  {return (LIST) append_cont (p, list);}

LIST mu___empty_list () {return (LIST) empty_list ();}

int mu___set_mu_warnings (int flag) {return set_mu_warnings (flag);}
 
int mu___set_mu_simplify_frontier (int flag)
  {return set_mu_simplify_frontier (flag);}

int mu___set_mu_verbose (int flag) {return set_mu_verbose (flag);}

void mu___mu_init (void) {mu_init ();}

void mu___mu_quit (void) {mu_quit ();}

void mu___pvs_mu_print_formula (Formula fml) {pvs_mu_print_formula (fml);}

void mu___pvs_mu_print_term (Term t) {pvs_mu_print_term (t);}

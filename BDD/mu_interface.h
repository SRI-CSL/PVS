#ifndef MU_INTERFACE_H
#define MU_INTERFACE_H

#include "mu.h"

extern int set_mu_warnings           (int flag);
extern int set_mu_simplify_frontier  (int flag);
extern int set_mu_verbose            (int flag);
extern Term mu_mk_not_term (Term fml1);
extern Formula mu_mk_not_formula (Formula fml1);
extern Term mu_mk_and_term (Term fml1, Term fml2);
extern Formula mu_mk_and_formula (Formula fml1, Formula fml2);
extern Term mu_mk_or_term (Term fml1, Term fml2);
extern Formula mu_mk_or_formula  (Formula fml1, Formula fml2);
extern Term mu_mk_equiv_term (Term fml1, Term fml2);
extern Formula mu_mk_equiv_formula (Formula fml1, Formula fml2);
extern Term mu_mk_implies_term (Term fml1, Term fml2);
extern Formula mu_mk_implies_formula (Formula fml1, Formula fml2);
extern Formula mu_mk_cofactor (Formula fml1 , Formula fml2);
extern Formula mu_mk_forall (LIST listvars, Formula fml);
extern Formula mu_mk_exists (LIST listvars, Formula fml);
extern Formula mu_mk_curry_application (Term fml1, LIST listargs);
extern Term mu_mk_l_fixed_point (int relvar , Term fml1);
extern Term mu_mk_g_fixed_point (int relvar, Term fml1);
extern Term mu_mk_rel_var_ (char *name);
extern Formula mu_check_mk_bool_var (char *name);
extern const char* get_mu_bool_var_name (int bdd_idx);
extern LIST empty_list ();
extern void pvs_mu_print_formula (Formula fml);
extern void pvs_mu_print_term (Term t);

#endif /* MU_INTERFACE_H */

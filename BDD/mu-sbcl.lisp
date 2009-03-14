;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------
(in-package :pvs)


;;;;;;;;;;;;;;;;;
;;;  Formula  ;;;
;;;;;;;;;;;;;;;;;

;;; Formula mu_mk_false_formula (void)
(sb-alien:define-alien-routine ("mu___mu_mk_false_formula" mu_mk_false_formula)
			       (* t))

;;; Formula mu_mk_true_formula (void)
(sb-alien:define-alien-routine ("mu___mu_mk_true_formula" mu_mk_true_formula)
			       (* t))

;;; Formula mu_mk_bool_var (char *name)
(sb-alien:define-alien-routine ("mu___mu_mk_bool_var" mu_mk_bool_var)
			       (* t)
  (name sb-alien:c-string))

;;; int mu_check_bool_var (char *name)
(sb-alien:define-alien-routine ("mu___mu_check_bool_var" mu_check_bool_var)
			       (integer 32)
  (var sb-alien:c-string))

;;; Formula mu_check_mk_bool_var (char *name)
(sb-alien:define-alien-routine
 ("mu___mu_check_mk_bool_var" mu_check_mk_bool_var)
			       (* t)
  (name sb-alien:c-string))

;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
(sb-alien:define-alien-routine ("mu___mu_mk_ite_formula" mu_mk_ite_formula)
			       (* t)
  (cnd (* t))
  (then_part (* t))
  (else_part (* t)))

;;; Formula mu_mk_curry_application (Term R, LIST subs)
(sb-alien:define-alien-routine
 ("mu___mu_mk_curry_application" mu_mk_curry_application)
			       (* t)
  (R (* t))
  (subs (* t)))

;;; Formula mu_mk_application (Term R, LIST subs, int curried)
(sb-alien:define-alien-routine ("mu___mu_mk_application" mu_mk_application)
			       (* t)
  (R (* t))
  (subs (* t))
  (curried (integer 32)))

;;; Formula mu_mk_forall (LIST listvars, Formula fml)
(sb-alien:define-alien-routine ("mu___mu_mk_forall" mu_mk_forall)
			       (* t)
  (listvars (* t))
  (fml (* t)))

;;; Formula mu_mk_exists (LIST listvars, Formula fml)
(sb-alien:define-alien-routine ("mu___mu_mk_exists" mu_mk_exists)
			       (* t)
  (listvars (* t))
  (fml (* t)))

;;; Formula mu_mk_implies_formula (Formula fml1, Formula fml2)
(sb-alien:define-alien-routine
 ("mu___mu_mk_implies_formula" mu_mk_implies_formula)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))

;;; Formula mu_mk_equiv_formula (Formula fml1, Formula fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_equiv_formula" mu_mk_equiv_formula)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))

;;; Formula mu_mk_or_formula (Formula fml1, Formula fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_or_formula" mu_mk_or_formula)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))

;;; Formula mu_mk_and_formula (Formula fml1, Formula fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_and_formula" mu_mk_and_formula)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))

;;; Formula mu_mk_not_formula (Formula fml)
(sb-alien:define-alien-routine ("mu___mu_mk_not_formula" mu_mk_not_formula)
			       (* t)
  (fml (* t)))

;;; Formula mu_mk_cofactor (Formula fml1, Formula fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_cofactor" mu_mk_cofactor)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))

;;;;;;;;;;;;;;;
;;;  Term   ;;;
;;;;;;;;;;;;;;;
;;; Term mu_mk_abstraction (LIST vars, Formula f1)
(sb-alien:define-alien-routine ("mu___mu_mk_abstraction" mu_mk_abstraction)
			       (* t)
  (vars (* t))
  (f1 (* t)))
;;; Term mu_mk_l_fixed_point (int relvar, Term fml1)
(sb-alien:define-alien-routine ("mu___mu_mk_l_fixed_point" mu_mk_l_fixed_point)
			       (* t)
  (relvar (integer 32))
  (fml1 (* t)))
;;; Term mu_mk_g_fixed_point (int relvar, Term fml1)
(sb-alien:define-alien-routine ("mu___mu_mk_g_fixed_point" mu_mk_g_fixed_point)
			       (* t)
  (relvar (integer 32))
  (fml1 (* t)))
;;; Term mu_mk_reach (Term Next, Term S0, Term Inv)
(sb-alien:define-alien-routine ("mu___mu_mk_reach" mu_mk_reach)
			       (* t)
  (Next (* t))
  (S0 (* t))
  (Inv (* t)))
;;; Term mu_mk_rel_var_dcl (char *name)
(sb-alien:define-alien-routine ("mu___mu_mk_rel_var_dcl" mu_mk_rel_var_dcl)
			       (* t)
  (name sb-alien:c-string))
;;; Term mu_mk_rel_var_ (char *name)
(sb-alien:define-alien-routine ("mu___mu_mk_rel_var_" mu_mk_rel_var_)
			       (* t)
  (name sb-alien:c-string))
;;; Term mu_mk_true_term (void)
(sb-alien:define-alien-routine ("mu___mu_mk_true_term" mu_mk_true_term)
			       (* t))
;;; Term mu_mk_false_term (void)
(sb-alien:define-alien-routine ("mu___mu_mk_false_term" mu_mk_false_term)
			       (* t))
;;; Term mu_mk_not_term (Term fml1)
(sb-alien:define-alien-routine ("mu___mu_mk_not_term" mu_mk_not_term)
			       (* t)
  (fml1 (* t)))
;;; Term mu_mk_and_term (Term fml1, Term fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_and_term" mu_mk_and_term)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))
;;; Term mu_mk_or_term (Term fml1, Term fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_or_term" mu_mk_or_term)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))
;;; Term mu_mk_equiv_term (Term fml1, Term fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_equiv_term" mu_mk_equiv_term)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))
;;; Term mu_mk_implies_term (Term fml1, Term fml2)
(sb-alien:define-alien-routine ("mu___mu_mk_implies_term" mu_mk_implies_term)
			       (* t)
  (fml1 (* t))
  (fml2 (* t)))
;;; const char *get_mu_bool_var_name (int bdd_idx)
(sb-alien:define-alien-routine
 ("mu___get_mu_bool_var_name" get_mu_bool_var_name)
			       sb-alien:c-string
  (bdd_idx (integer 32)))

;;;;;;;;;;;;;;;;;;;
;;;  Lists      ;;;
;;;;;;;;;;;;;;;;;;;

;;; LIST append_cont (void *p, LIST list)
(sb-alien:define-alien-routine ("mu___append_cont" append_cont)
			       (* t)
  (p (* t))
  (list (* t)))
;;; LIST empty_list (void)
(sb-alien:define-alien-routine ("mu___empty_list" empty_list)
			       (* t))

;;;
;;; Flags

;;; int set_mu_warnings (int flag)
(sb-alien:define-alien-routine ("mu___set_mu_warnings" set_mu_warnings)
			       (integer 32)
  (flag (integer 32)))
;;; int set_mu_simplify_frontier (int flag)
(sb-alien:define-alien-routine
 ("mu___set_mu_simplify_frontier" set_mu_simplify_frontier)
			       (integer 32)
  (flag (integer 32)))
;;; int set_mu_verbose (int flag)
(sb-alien:define-alien-routine ("mu___set_mu_verbose" set_mu_verbose)
			       (integer 32)
  (flag (integer 32)))

;;
;;
;; GC management: not needed, "modelcheck_formula" takes care of it.
;;

;;;;;;;;;;;;;;;;;;;
;;;  print      ;;;
;;;;;;;;;;;;;;;;;;;

;;; void pvs_mu_print_formula (Formula fml)
(sb-alien:define-alien-routine
 ("mu___pvs_mu_print_formula" pvs_mu_print_formula)
			       sb-alien:void
  (fml (* t)))
;;; void pvs_mu_print_term (Term t)
(sb-alien:define-alien-routine ("mu___pvs_mu_print_term" pvs_mu_print_term)
			       sb-alien:void
  (term (* t)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; void mu_init (void)
(sb-alien:define-alien-routine ("mu___mu_init" mu_init)
			       sb-alien:void)
(sb-alien:define-alien-routine ("mu___mu_quit" mu_quit)
			       sb-alien:void)
;;; BDDPTR mu___modelcheck_formula (Formula fml)
(sb-alien:define-alien-routine ("mu___modelcheck_formula" modelcheck_formula)
			       (* t)
  (fml (* t)))

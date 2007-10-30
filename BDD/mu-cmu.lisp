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

(eval-when (eval load)
  ;; (use-package :alien)
  (use-package :c-call))


;;;;;;;;;;;;;;;;;
;;;  Formula  ;;;
;;;;;;;;;;;;;;;;;

;;; Formula mu_mk_false_formula (void)
(alien:def-alien-routine ("mu___mu_mk_false_formula" mu_mk_false_formula)
			 unsigned-int)

;;; Formula mu_mk_true_formula (void)
(alien:def-alien-routine ("mu___mu_mk_true_formula" mu_mk_true_formula)
			 unsigned-int)

;;; Formula mu_mk_bool_var (char *name)
(alien:def-alien-routine ("mu___mu_mk_bool_var" mu_mk_bool_var)
			 unsigned-int
  (name c-string))

;;; Formula int mu_check_bool_var (char *var)
(alien:def-alien-routine ("mu___mu_check_bool_var" mu_check_bool_var)
			 unsigned-int
  (var c-string))

;;; Formula mu_check_mk_bool_var
(alien:def-alien-routine ("mu___mu_check_mk_bool_var" mu_check_mk_bool_var)
			 unsigned-int
  (name c-string))

;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
(alien:def-alien-routine ("mu___mu_mk_ite_formula" mu_mk_ite_formula)
			 unsigned-int
  (cond unsigned-int)
  (then_part unsigned-int)
  (else_part unsigned-int))

;;; Formula mu_mk_curry_application (Term R, LIST subs)
(alien:def-alien-routine ("mu___mu_mk_curry_application" mu_mk_curry_application)
			 unsigned-int
  (R unsigned-int)
  (subs unsigned-int))
  
;;; Formula mu_mk_application (Term R, LIST subs, int curried)
(alien:def-alien-routine ("mu___mu_mk_application" mu_mk_application)
			 unsigned-int
			 (R unsigned-int)
			 (subs unsigned-int)
			 (curried unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_forall" mu_mk_forall)
			 unsigned-int
			 (listvars unsigned-int)
			 (fml1 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_exists" mu_mk_exists)
			 unsigned-int
			 (listvars unsigned-int)
			 (fml1 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_implies_formula" mu_mk_implies_formula)
			 unsigned-int
			 (fml1 unsigned-int)
			 (fml2 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_equiv_formula" mu_mk_equiv_formula)
			 unsigned-int
			 (fml1 unsigned-int)
			 (fml2 unsigned-int))
;; (alien:def-alien-routine ("mu___mu_mk_xor_formula" mu_mk_xor_formula) void);; (fml1 fml2)
(alien:def-alien-routine ("mu___mu_mk_or_formula" mu_mk_or_formula)
			 unsigned-int
			 (fml1 unsigned-int)
			 (fml2 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_and_formula" mu_mk_and_formula)
			 unsigned-int
			 (fml1 unsigned-int)
			 (fml2 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_not_formula" mu_mk_not_formula)
			 unsigned-int
			 (fml unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_cofactor" mu_mk_cofactor)
			 unsigned-int
			 (fml1 unsigned-int)
			 (fml2 unsigned-int))

;;;;;;;;;;;;;;;
;;;  Term   ;;;
;;;;;;;;;;;;;;;
(alien:def-alien-routine ("mu___mu_mk_abstraction" mu_mk_abstraction)
			 unsigned-int
			 (vars unsigned-int)
			 (f1 unsigned-int))
;;; Term mu_mk_abstraction (LIST vars, Formula f1)
(alien:def-alien-routine ("mu___mu_mk_l_fixed_point" mu_mk_l_fixed_point)
			 unsigned-int
			 (relvar unsigned-int)
			 (fml1 unsigned-int))
;;; Term mu_mk_fixed_point 
(alien:def-alien-routine ("mu___mu_mk_g_fixed_point" mu_mk_g_fixed_point)
			 unsigned-int
			 (relvar unsigned-int)
			 (fml1 unsigned-int))
;;; Term mu_mk_g_fixed_point 
(alien:def-alien-routine ("mu___mu_mk_reach" mu_mk_reach)
			 unsigned-int
			 (Next unsigned-int)
			 (S0 unsigned-int)
			 (Inv unsigned-int))
;;; Term mu_mk_reach (Term Next, Term S0, Term Inv)
(alien:def-alien-routine ("mu___mu_mk_rel_var_dcl" mu_mk_rel_var_dcl)
			 unsigned-int
			 (char c-string))
;;; Term mu_mk_rel_var_dcl (char *name) 
(alien:def-alien-routine ("mu___mu_mk_rel_var_" mu_mk_rel_var_)
			 unsigned-int
			 (name c-string))
;;; Term  mu_mk_rel_var_ (R_Interpret Ip, char *name)
(alien:def-alien-routine ("mu___mu_mk_true_term" mu_mk_true_term)
			 unsigned-int)
;;; Term  mu_mk_true_term (void)
(alien:def-alien-routine ("mu___mu_mk_false_term" mu_mk_false_term)
			 unsigned-int)
;;; Term  mu_mk_false_term (void)
(alien:def-alien-routine ("mu___mu_mk_not_term" mu_mk_not_term)
			 unsigned-int
			 (fml1 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_and_term" mu_mk_and_term)
			 unsigned-int
			  (fml1 unsigned-int)
			  (fml2 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_or_term" mu_mk_or_term)
			 unsigned-int
			  (fml1 unsigned-int)
			  (fml2 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_equiv_term" mu_mk_equiv_term)
			 unsigned-int
			  (fml1 unsigned-int)
			  (fml2 unsigned-int))
(alien:def-alien-routine ("mu___mu_mk_implies_term" mu_mk_implies_term)
			 unsigned-int
			  (fml1 unsigned-int)
			  (fml2 unsigned-int))
;; (alien:def-alien-routine ("mu___mu_mk_xor_term" mu_mk_xor_term) void) ;; (fml1 fml2)

;;(alien:def-alien-routine ("mu___get_bdd_var_id" get_bdd_var_id) void) ;; (int)
(alien:def-alien-routine ("mu___get_mu_bool_var_name" get_mu_bool_var_name)
			 c-string
			  (bdd_idx unsigned-int))
;;;;;;;;;;;;;;;;;;;
;;;  Lists      ;;;
;;;;;;;;;;;;;;;;;;;
;; 

(alien:def-alien-routine ("mu___append_cont" append_cont)
			 unsigned-int
			 (p unsigned-int)
			 (list unsigned-int))
(alien:def-alien-routine ("mu___empty_list" empty_list)
			 unsigned-int)

;;;
;;; Flags

;; (alien:def-alien-routine ("mu___set_mu_bdd_ordering" set_mu_bdd_ordering) void)
(alien:def-alien-routine ("mu___set_mu_warnings" set_mu_warnings)
			 void)
(alien:def-alien-routine ("mu___set_mu_simplify_frontier" set_mu_simplify_frontier)
			 void)
(alien:def-alien-routine ("mu___set_mu_verbose" set_mu_verbose)
			 void)
;; (alien:def-alien-routine ("mu___set_mu_bdd_use_neg_edges" set_mu_bdd_use_neg_edges) void)

;;
;;
;; GC management: not needed, "modelcheck_formula" takes care of it.
;;

;;;;;;;;;;;;;;;;;;;
;;;  print      ;;;
;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine ("mu___pvs_mu_print_formula" pvs_mu_print_formula)
			 void)
(alien:def-alien-routine ("mu___pvs_mu_print_term" pvs_mu_print_term)
			 void)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(alien:def-alien-routine ("mu___mu_init" mu_init)
			 void)
(alien:def-alien-routine ("mu___mu_quit" mu_quit)
			 void)
(alien:def-alien-routine ("mu___modelcheck_formula" modelcheck_formula)
			 unsigned-int
			 (fml unsigned-int))

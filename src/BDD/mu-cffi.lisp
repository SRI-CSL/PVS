;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mu-allegro.lisp -- Interface to the Mu-calculus model-checker

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;(cffi:defcvar Ip)

;;; Formula mu_mk_false_formula (void)
(cffi:defcfun (mu_mk_false_formula "mu___mu_mk_false_formula") :pointer
  )
;;; Formula mu_mk_true_formula (void)
(cffi:defcfun (mu_mk_true_formula "mu___mu_mk_true_formula") :pointer
  )
;;; Formula mu_mk_bool_var (char *name)
(cffi:defcfun (mu_mk_bool_var "mu___mu_mk_bool_var") :pointer
  )
;;; Formula mu_check_bool_var
(cffi:defcfun (mu_check_bool_var "mu___mu_check_bool_var") :pointer
  (x :pointer))
;;; Formula mu_check_mk_bool_var
(cffi:defcfun (mu_check_mk_bool_var "mu___mu_check_mk_bool_var") :pointer
    (x :pointer))
;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
(cffi:defcfun (mu_mk_ite_formula "mu___mu_mk_ite_formula") :pointer
  (cond :pointer)
  (then_part :pointer)
  (else_part :pointer))
;;; Formula mu_mk_curry_application (Term fml1, LIST listargs)
(cffi:defcfun (mu_mk_curry_application "mu___mu_mk_curry_application") :pointer
  (fml1 :pointer)
  (listargs :pointer))
;;; Formula mu_mk_application (Term R, LIST subs, int curried)
(cffi:defcfun (mu_mk_application "mu___mu_mk_application") :pointer
  (R :pointer)
  (subs :pointer)
  (curried :unsigned-int))

(cffi:defcfun (mu_mk_forall "mu___mu_mk_forall") :pointer
  (listvars :pointer)
  (fml :pointer))

(cffi:defcfun (mu_mk_exists "mu___mu_mk_exists") :pointer
  (listvars :pointer)
  (fml :pointer))

(cffi:defcfun (mu_mk_implies_formula "mu___mu_mk_implies_formula") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_equiv_formula "mu___mu_mk_equiv_formula") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

;; Doesn't seem to exist in C
(cffi:defcfun (mu_mk_xor_formula "mu___mu_mk_xor_formula") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_or_formula "mu___mu_mk_or_formula") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_and_formula "mu___mu_mk_and_formula") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_not_formula "mu___mu_mk_not_formula") :pointer
  (fml1 :pointer))

(cffi:defcfun (mu_mk_cofactor "mu___mu_mk_cofactor") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

;;;;;;;;;;;;;;;
;;;  Term   ;;;
;;;;;;;;;;;;;;;
(cffi:defcfun (mu_mk_abstraction "mu___mu_mk_abstraction") :pointer
  (vars :pointer)
  (f1 :pointer))
;;; Term mu_mk_abstraction (LIST vars, Formula f1)
(cffi:defcfun (mu_mk_l_fixed_point "mu___mu_mk_l_fixed_point") :pointer
  (relvar :pointer)
  (fml1 :pointer))
;;; Term mu_mk_fixed_point 
(cffi:defcfun (mu_mk_g_fixed_point "mu___mu_mk_g_fixed_point") :pointer
  (relvar :pointer)
  (fml1 :pointer))
;;; Term mu_mk_g_fixed_point 
(cffi:defcfun (mu_mk_reach "mu___mu_mk_reach") :pointer
  (Next :pointer)
  (S0 :pointer)
  (Inv :pointer))
;;; Term mu_mk_reach (Term Next, Term S0, Term Inv)
(cffi:defcfun (mu_mk_rel_var_dcl "mu___mu_mk_rel_var_dcl") :pointer
  (name :pointer))
;;; Term mu_mk_rel_var_dcl (char *name) 
(cffi:defcfun (mu_mk_rel_var_ "mu___mu_mk_rel_var_") :pointer
  (name :pointer))
;;; Term  mu_mk_rel_var_ (char *name)
(cffi:defcfun (mu_mk_true_term "mu___mu_mk_true_term") :pointer
  )
;;; Term  mu_mk_true_term (void)
(cffi:defcfun (mu_mk_false_term "mu___mu_mk_false_term") :pointer
  )
;;; Term  mu_mk_false_term (void)
(cffi:defcfun (mu_mk_not_term "mu___mu_mk_not_term") :pointer
  (fml1 :pointer))

(cffi:defcfun (mu_mk_and_term "mu___mu_mk_and_term") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_or_term "mu___mu_mk_or_term") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_equiv_term "mu___mu_mk_equiv_term") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_implies_term "mu___mu_mk_implies_term") :pointer
  (fml1 :pointer)
  (fml2 :pointer))

(cffi:defcfun (mu_mk_xor_term "mu___mu_mk_xor_term") :pointer
  (fml1 :pointer)
  (fml2 :pointer))
;; C source doesn't exist
(cffi:defcfun (get_bdd_var_id "mu___get_bdd_var_id") :pointer
  (var :pointer))

(cffi:defcfun (get_mu_bool_var_name "mu___get_mu_bool_var_name") :pointer
  (bdd_idx :pointer))

;;;;;;;;;;;;;;;;;;;
;;;  Lists      ;;;
;;;;;;;;;;;;;;;;;;;
;; 

(cffi:defcfun (append_cont "mu___append_cont") :pointer
  (p :pointer)
  (list :pointer))

(cffi:defcfun (empty_list "mu___empty_list") :pointer
  )

;;;
;;; Flags

;; Not in C sources
(cffi:defcfun (set_mu_bdd_ordering "mu___set_mu_bdd_ordering") :void
  (flag :unsigned-int))

(cffi:defcfun (set_mu_warnings "mu___set_mu_warnings") :void
  (flag :unsigned-int))

(cffi:defcfun (set_mu_simplify_frontier "mu___set_mu_simplify_frontier") :void
  (flag :unsigned-int))

(cffi:defcfun (set_mu_verbose "mu___set_mu_verbose") :void
  (flag :unsigned-int))

(cffi:defcfun (set_mu_bdd_use_neg_edges "mu___set_mu_bdd_use_neg_edges") :void
  (flag :unsigned-int))

;;
;;
;; GC management: not needed, "modelcheck_formula" takes care of it.
;;

;;;;;;;;;;;;;;;;;;;
;;;  print      ;;;
;;;;;;;;;;;;;;;;;;;

(cffi:defcfun (pvs_mu_print_formula "mu___pvs_mu_print_formula") :void
  (fml :pointer))

(cffi:defcfun (pvs_mu_print_term "mu___pvs_mu_print_term") :void
  (tt :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi:defcfun (mu_init "mu___mu_init") :void
  )

(cffi:defcfun (mu_quit "mu___mu_quit") :void
  )

(cffi:defcfun (modelcheck_formula "mu___modelcheck_formula") :unsigned-int
  (fml :pointer))

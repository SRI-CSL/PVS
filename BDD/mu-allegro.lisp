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

(require :foreign)

;;;;;;;;;;;;;;;;;
;;;  Formula  ;;;
;;;;;;;;;;;;;;;;;

;;; Formula mu_mk_false_formula (void)
(ff:def-foreign-call (mu_mk_false_formula "mu___mu_mk_false_formula")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_mk_true_formula (void)
(ff:def-foreign-call (mu_mk_true_formula "mu___mu_mk_true_formula")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_mk_bool_var (char *name)
(ff:def-foreign-call (mu_mk_bool_var "mu___mu_mk_bool_var")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_check_bool_var
(ff:def-foreign-call (mu_check_bool_var "mu___mu_check_bool_var")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_check_mk_bool_var
(ff:def-foreign-call (mu_check_mk_bool_var "mu___mu_check_mk_bool_var")
    ((x :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
(ff:def-foreign-call (mu_mk_ite_formula "mu___mu_mk_ite_formula")
    ((cond :unsigned-int integer)
     (then_part :unsigned-int integer)
     (else_part :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_mk_curry_application (Term R, LIST subs, int curried)
(ff:def-foreign-call (mu_mk_curry_application "mu___mu_mk_curry_application")
    ((R :unsigned-int integer)
     (subs :unsigned-int integer)
     (curried :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Formula mu_mk_application (Term R, LIST subs, int curried)
(ff:def-foreign-call (mu_mk_application "mu___mu_mk_application")
    ((R :unsigned-int integer)
     (subs :unsigned-int integer)
     (curried :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
(ff:def-foreign-call (mu_mk_forall "mu___mu_mk_forall")
    ((listvars :unsigned-int integer)
     (fml :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (listvars fml1) always formula
(ff:def-foreign-call (mu_mk_exists "mu___mu_mk_exists")
    ((listvars :unsigned-int integer)
     (fml :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (listvars fml1) always formula

(ff:def-foreign-call (mu_mk_implies_formula "mu___mu_mk_implies_formula")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1 fml2)
(ff:def-foreign-call (mu_mk_equiv_formula "mu___mu_mk_equiv_formula")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int);; (fml1 fml2)
;; Doesn't seem to exist in C
(ff:def-foreign-call (mu_mk_xor_formula "mu___mu_mk_xor_formula")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int);; (fml1 fml2)
(ff:def-foreign-call (mu_mk_or_formula "mu___mu_mk_or_formula")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int);; (fml1 fml2)
(ff:def-foreign-call (mu_mk_and_formula "mu___mu_mk_and_formula")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int);; (fml1 fml2)
(ff:def-foreign-call (mu_mk_not_formula "mu___mu_mk_not_formula")
    ((fml1 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int);; (fml1)
(ff:def-foreign-call (mu_mk_cofactor "mu___mu_mk_cofactor")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int);; (fml1 fml2) always formula

;;;;;;;;;;;;;;;
;;;  Term   ;;;
;;;;;;;;;;;;;;;
(ff:def-foreign-call (mu_mk_abstraction "mu___mu_mk_abstraction")
    ((vars :unsigned-int integer)
     (f1 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term mu_mk_abstraction (LIST vars, Formula f1)
(ff:def-foreign-call (mu_mk_l_fixed_point "mu___mu_mk_l_fixed_point")
    ((relvar :unsigned-int integer)
     (fml1 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term mu_mk_fixed_point 
(ff:def-foreign-call (mu_mk_g_fixed_point "mu___mu_mk_g_fixed_point")
    ((relvar :unsigned-int integer)
     (fml1 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term mu_mk_g_fixed_point 
(ff:def-foreign-call (mu_mk_reach "mu___mu_mk_reach")
    ((Next :unsigned-int integer)
     (S0 :unsigned-int integer)
     (Inv :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term mu_mk_reach (Term Next, Term S0, Term Inv)
(ff:def-foreign-call (mu_mk_rel_var_dcl "mu___mu_mk_rel_var_dcl")
    ((name :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term mu_mk_rel_var_dcl (char *name) 
(ff:def-foreign-call (mu_mk_rel_var_ "mu___mu_mk_rel_var_")
    ((Ip :unsigned-int integer)
     (name :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term  mu_mk_rel_var_ (R_Interpret Ip, char *name)
(ff:def-foreign-call (mu_mk_true_term "mu___mu_mk_true_term")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term  mu_mk_true_term (void)
(ff:def-foreign-call (mu_mk_false_term "mu___mu_mk_false_term")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
;;; Term  mu_mk_false_term (void)
(ff:def-foreign-call (mu_mk_not_term "mu___mu_mk_not_term")
    ((fml1 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1)
(ff:def-foreign-call (mu_mk_and_term "mu___mu_mk_and_term")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1 fml2)
(ff:def-foreign-call (mu_mk_or_term "mu___mu_mk_or_term")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1 fml2)
(ff:def-foreign-call (mu_mk_equiv_term "mu___mu_mk_equiv_term")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1 fml2)
(ff:def-foreign-call (mu_mk_implies_term "mu___mu_mk_implies_term")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1 fml2)
(ff:def-foreign-call (mu_mk_xor_term "mu___mu_mk_xor_term")
    ((fml1 :unsigned-int integer)
     (fml2 :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (fml1 fml2)
;; C source doesn't exist
(ff:def-foreign-call (get_bdd_var_id "mu___get_bdd_var_id")
    ((var :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (int)
(ff:def-foreign-call (get_mu_bool_var_name "mu___get_mu_bool_var_name")
    ((bdd_idx :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int) ;; (char)
;;;;;;;;;;;;;;;;;;;
;;;  Lists      ;;;
;;;;;;;;;;;;;;;;;;;
;; 

(ff:def-foreign-call (append_cont "mu___append_cont")
    ((p :unsigned-int integer)
     (list :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)
(ff:def-foreign-call (empty_list "mu___empty_list")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

;;;
;;; Flags

;; Not in C sources
(ff:def-foreign-call (set_mu_bdd_ordering "mu___set_mu_bdd_ordering")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (set_mu_warnings "mu___set_mu_warnings")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (set_mu_simplify_frontier "mu___set_mu_simplify_frontier")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (set_mu_verbose "mu___set_mu_verbose")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (set_mu_bdd_use_neg_edges "mu___set_mu_bdd_use_neg_edges")
    ((flag :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)

;;
;;
;; GC management: not needed, "modelcheck_formula" takes care of it.
;;

;;;;;;;;;;;;;;;;;;;
;;;  print      ;;;
;;;;;;;;;;;;;;;;;;;

(ff:def-foreign-call (pvs_mu_print_formula "mu___pvs_mu_print_formula")
    ((fml :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (pvs_mu_print_term "mu___pvs_mu_print_term")
    ((t :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main function   ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(ff:def-foreign-call (mu_init "mu___mu_init")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (mu_quit "mu___mu_quit")
    (:void)
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :void)
(ff:def-foreign-call (modelcheck_formula "mu___modelcheck_formula")
    ((fml :unsigned-int integer))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :arg-checking nil
  :call-direct t
  :returning :unsigned-int)

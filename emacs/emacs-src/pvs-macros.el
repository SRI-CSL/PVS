;;;
;;; pvs-macros.el   dave_sc, 12/3/98
;;;
;;; Defines macros which are used in other pvs emacs files.
;;; These are defined here so they are available in byte compilation
;;;


;;;
;;; originally in pvs-load.el
;;;
(if (not (fboundp 'save-match-data))
    (defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
	  (list 'unwind-protect
		(cons 'progn body)
		(list 'store-match-data original))))))

(defmacro defpvs (name class arglist docstring &rest body)
  "(defpvs NAME CLASS ARGLIST DOCSTRING BODY...):
define NAME as a PVS command."
  (list 'progn
	(list 'put (list 'quote name) ''pvs-command (list 'quote class))
	(cons 'defun (cons name (cons arglist (cons docstring body))))))

(provide 'pvs-macros)




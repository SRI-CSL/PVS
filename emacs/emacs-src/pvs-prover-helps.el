;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-prover-helps.el -- Helps for PVS prover
;; Author          : C. Michael Holloway <c.m.holloway@LaRC.NASA.GOV>
;; Created On      : Tue May 25 07:41:17 1993 (shemesh)
;; Last Modified By: Sam Owre
;; Last Modified On: Sun May 28 22:20:17 1995
;; Update Count    : 127
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; This is a U.S. Government work and thus not protected by U.S. copyright.
;; I would appreciate it if you did not copy or modify this code without
;; keeping all of the introductory comments intact.  Also, if you modify the
;; code, I would appreciate it if you would send me your modifications (using
;; the command provided below for making suggestions).
;;
;; pvs-prover-helps provides some functions to simplify the entering of
;; PVS prover commands.

;;   To see a list of available commands, type M-x pvs-prover-help
;;   To report a bug, type M-x pvs-report-bug
;;   To make a suggestion, type M-x pvs-make-suggestion
;;   (Note: There is no warranty of any kind with this code, so there is no
;;          promise to either implement suggestions or to fix bugs.)
;;
;; As an example of how you might use the command provided herein, consider
;; the following proof:
;;        (INDUCT "i")
;;        (("1" (EXPAND "half") (PROPAX))
;;         ("2" (SKOLEM 1 ("JJ"))
;;              (FLATTEN)
;;              (ASSERT)
;;              (EXPAND "half" 1) 
;;              (ASSERT))))
;;
;; Using the commands and key bindings provided, you could do this proof by 
;; typing the following:
;;      TAB I    i RET RET
;;      TAB e    y RET
;;      TAB s    1 JJ RET RET
;;      TAB f 
;;      TAB a
;;      TAB e    y 1 RET
;;      TAB a

;;
;; Constants
;;

(defconst pvs-prover-helps-version "2.4")
(defconst email-address "c.m.holloway@LaRC.NASA.GOV")

(defconst pvs-3char-overloadable-ops-regexp "<=>\\|<<=\\|>>=")

(defconst pvs-2char-overloadable-ops-regexp "o \\|/=\\|==\\|<=\\|>=\\|=>\\|/\\\\\\|\\\\/\\|\\[\\]\\|<>\\|\+\+\\|\\*\\*\\|//\\|^^\\|<<\\|>>\\|@@\\|##\\||-\\||=\\|<|\\||>\\|\\[|")

(defconst pvs-1char-overloadable-ops-regexp
  "+\\|-\\|\\*\\|\\&\\|\\^\\|=\\|/\\|<\\|>\\|~\\|#")

(defconst pvs-reserved-words
  '("assuming" "axiom" "accept" "changes" "all" "array" "begin" "by"
    "case" "declare" "definition" "else" "elsif" "endif" "endassuming"
    "endcase" "end" "exists" "exporting" "exit" "forall" "function" "formula"
    "from" "importing" "in" "is" "lambda" "lemma" "loop"
    "mapping" "measure" "module" "nothing" "of" "onto" "obligation"
    "opspec" "proof" "prove" "recursive" "result" "theorem" "theory"
    "using" "var" "variable" "record" "verify" "where" "then" "type"
    "while" "with" "let" "setvariable"))


;;
;; Functions
;; 

(defun pvs-prover-report-bug ()
  "Sets up mail buffer for reporting bug."
  (interactive)
  (mail)
  (mail-to) (insert email-address)
  (mail-subject) (insert "BUG in PVS cMH Helps V"
			 pvs-prover-helps-version)  
  (search-forward (concat "\n" mail-header-separator "\n")))

(defun pvs-prover-make-suggestion ()
  "Sets up mail buffer for making a suggestion."
  (interactive)
  (mail)
  (mail-to) (insert email-address)
  (mail-subject) (insert "REQUEST for PVS cMH Helps V"
			 pvs-prover-helps-version )
  (search-forward (concat "\n" mail-header-separator "\n")))
      
(defun help-pvs-prover-emacs ()
  "Emacs extensions for the PVS prover commands.
Developed by C. Michael Holloway, NASA Langley Research Center, Hampton VA.

These extensions make it easier to use the PVS prover, by providing
key bindings for some of the more useful prover commands.  For example,
the assert command can be invoked by typing TAB a.

For a list of available prover commands, type M-x help-pvs-prover (C-c C-h p).
For help with a specific prover command, type M-x help-pvs-prover-command
   (C-c C-h c, or TAB H in the *pvs* buffer)
To report a bug, type M-x pvs-prover-report-bug.
To make a suggestion, type M-x pvs-prover-make-suggestion.

All the following commands work only in the active proof buffer (*pvs*).
They are all prefixed by TAB (\C-i), and they are listed in alphabetical
order, indexed by the prover command.
     
PVS prover command    Key      Comments
------------------    ---      --------
Any Command           TAB      Prompts for command name
apply-extensionality  E        Prompts for formula number
assert                a        
auto-rewrite          A        Uses formula at point, or prompts
auto-rewrite-theory   C-a      Prompts for theory
bddsimp               B        
beta                  b        Prompts for formula number
case                  c        Prompts for expression
case-replace          C        Prompts for expression
copy                  2        Prompts for formula number
decompose-equality    =        Prompts for formula number
delete                d        Prompts for formula number
do-rewrite            D        Prompts for formula number
expand                e        Expand definition at point
extensionality        x        Prompts for type
flatten               f        
grind                 G        
ground                g        
hide                  C-h      Prompts for formula number
iff                   F        Prompts for formula number
induct                I        Prompts for variable name
induct-and-simplify   C-s      Prompts for variable name
inst                  i        Prompts for formula number and expressions
inst?                 ?        Prompts for formula number, additional arguments
lemma                 L        Prompts for lemma name
lift-if               l        Prompts for formula number
model-check           M
musimp                m        Prompts for formula number
name                  n        Prompts for name and expression
name-replace          N        Prompts for name and expression
postpone              P        
prop                  p        
quit                  C-q      Prefix arg quits without saving
replace               r        Prompts for formula number
replace-eta           8        Prompts for expression
rewrite               R        Prompts for lemma name and formula number
skolem!               !        Prompts for formula number
skosimp               S        Prompts for formula number
skosimp*              *        
split                 s        Prompts for formula number
tcc                   T        
then                  C-t      Prompts for strategies
typepred              t        Uses expression of region, or prompts
undo                  u        Prefix arg is number of steps to undo


The following commands provide a proof stepping facility, and require that
a Proof buffer exist.  If it doesn't, use M-x show-proof on a formula to
bring one up.

Key          Effect
---          ------
TAB 1        Runs next proof step.  Prefix arg is number of steps.
TAB U        Undoes previous proof step.  Prefix arg is number of steps.
TAB #        Skips the next step.  Prefix arg is number to skip,
                                   negative skips backwards.

The following are not directly associated with PVS prover commands:

Key          Effect
---          ------
TAB '        Inserts double quotes, leaving point in between 
TAB C-j      Wraps current line in parentheses and runs it

Also, typing \\C-j (LFD) alone will invoke pvs-prover-wrap-with-parens.
"
  (interactive)
  (let ((buf (get-buffer-create "Prover Emacs Help")))
    (set-buffer buf)
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert (documentation 'help-pvs-prover-emacs))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (unless buffer-read-only (toggle-read-only))
    (pop-to-buffer buf)
    (pvs-view-mode)))

(defun pvs-prover-any-command (command)
  "Insert the beginning part of any PVS prover command.
Prompts for the command to insert."
  (interactive (complete-strategy-name "Prover command to insert: "))
  (goto-pvs-proof-buffer)
  (insert "(" command " )")
  (forward-char -1))

(defun pvs-prover-apply-extensionality (&optional num)
  "Insert and send the APPLY-EXTENSIONALITY prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Apply extensionality to formula: "))))
    (insert "(apply-extensionality " fnum " :hide? t)")
    (return-ilisp)))

(defun pvs-prover-decompose-equality (&optional num)
  "Insert and send the DECOMPOSE-EQUALITY prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Decompose equality for formula: "))))
    (insert "(decompose-equality " fnum ")")
    (return-ilisp)))

(defun pvs-prover-assert ()
  "Insert and send the ASSERT prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(assert)")
  (return-ilisp))

(defun pvs-prover-auto-rewrite ()
  "Insert and send the AUTO-REWRITE prover command.
If looking at a formula, installs an auto-rewrite for it.
Otherwise, prompts for a formula for which to install an auto-rewrite."
  (interactive)
  (goto-pvs-proof-buffer)
  (let ((def2arw ""))
    (skip-chars-backward "a-zA-Z0-9_?")
    (if (looking-at "[a-zA-Z0-9_?]")
	(let ((bpt (point))
	      (tried-alist nil)
	      (ept nil))
	  (skip-chars-forward "a-zA-Z0-9_?")
	  (setq ept (point))
	  (setq def2arw (buffer-substring bpt ept)))
      (setq def2arw (read-from-minibuffer "Auto-rewrite: ")))
    (end-of-buffer)
    (insert "(auto-rewrite " ?\" def2arw ?\" ")")
    (return-ilisp)))

(defun pvs-prover-auto-rewrite-theory (theory)
  "Insert and send the AUTO-REWRITE-THEORY prover command.
Prompts for the theory name."
  (interactive (complete-theory-name "Theory for auto-rewrite:"))
  (goto-pvs-proof-buffer)
  (insert "(auto-rewrite-theory " ?\" theory ?\" ")")
  (return-ilisp))

(defun pvs-prover-bddsimp ()
  "Insert and send the BDDSIMP prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(bddsimp)")
  (return-ilisp))

(defun pvs-prover-beta (&optional num)
  "Insert and send the BETA prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer
		   "Beta reduce formulas [CR for default]: " "*" ))))
    (insert "(beta " fnum ")")
    (return-ilisp)))

(defun pvs-prover-case (on-what)
  "Insert and send the CASE prover command.
Prompts for the expression on which to split."
  (interactive "sCase expression: ")
  (goto-pvs-proof-buffer)
  (insert "(case \"" on-what "\")")
  (return-ilisp))

(defun pvs-prover-case-replace (on-what)
  "Insert and send the CASE-REPLACE prover command.
Prompts for the expression on which to split."
  (interactive "sCase-replace expression: ")
  (goto-pvs-proof-buffer)
  (insert "(case-replace \"" on-what "\")")
  (return-ilisp))

(defun pvs-prover-copy (&optional num)
  "Insert and send the COPY prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Copy formula: "))))
    (insert "(copy " fnum ")")
    (return-ilisp)))

(defun pvs-prover-delete (&optional num)
  "Insert and send the DELETE prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Delete formulas: "))))
    (insert "(delete " fnum ")")
    (return-ilisp)))

(defun pvs-prover-do-rewrite (&optional num)
  "Insert and send the DO-REWRITE prover command.
Prompts for formula numbers.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer
		   "Apply do-rewrite to formula [CR for default]: "))))
    (insert "(do-rewrite " fnum ")")
    (return-ilisp)))

(defun pvs-prover-expand (&optional hereonly)
  "Uses the EXPAND prover command to expand the formula at point.
Move cursor to beginning of formula you want to expand, before calling
this function.  If not looking at a formula, guesses at what to expand.
Give prefix arg if you want expansion to apply to one item in the sequent
only."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (skip-chars-backward "a-zA-Z_0-9?")
  (let ((def2expand "")
	(fnum "")
	(bpt (point))
	ept)
    (cond
     ((looking-at pvs-3char-overloadable-ops-regexp)
      (setq def2expand (buffer-substring (point) (+ (point) 3))))
     ((looking-at pvs-2char-overloadable-ops-regexp)
      (setq def2expand (buffer-substring (point) (+ (point) 2)))
      (cond ((string-equal def2expand "/\\")
	     (setq def2expand "/\\\\"))
	    ((string-equal def2expand "\\/")
	     (setq def2expand "\\\\/"))
	    ((string-equal def2expand "\[|")
	     (setq def2expand "\[||\]"))))
     ((looking-at pvs-1char-overloadable-ops-regexp)
      (setq def2expand (buffer-substring (point) (1+ (point)))))
     ((looking-at "[a-zA-Z_0-9?]")
      (skip-chars-forward "a-zA-Z_0-9?")
      (setq ept (point))
      (setq def2expand (buffer-substring bpt ept)))
     (t (pvs-prover-guess-at-expand)))
    (if (not (string= def2expand ""))
	(progn
	  (if hereonly
	      (if (re-search-backward "^[[{]" nil t)
		  (progn
		    (forward-char 1)
		    (setq bpt (point))
		    (re-search-forward "]\\|}" nil t)
		    (setq ept (1- (point)))
		    (setq fnum (buffer-substring bpt ept)))
		  (setq fnum (read-from-minibuffer
			      "in formula [CR for default]# " ""))))
	  (end-of-buffer)
	  (insert "(expand " ?\" def2expand ?\" " " fnum ")")
	  (return-ilisp)))))

(defun pvs-prover-extensionality ()
  "Insert and send the EXTENSIONALITY prover command.
Prompts for the type."
  (interactive)
  (goto-pvs-proof-buffer)
  (let ((type (read-from-minibuffer "Extensionality on type: ")))
    (insert (format "(extensionality \"%s\")" type))
    (return-ilisp)))

(defun pvs-prover-flatten ()
  "Insert and send the FLATTEN prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(flatten)")
  (return-ilisp))

(defun pvs-prover-grind ()
  "Insert and send the GRIND prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(grind)")
  (forward-char -1)
  (if current-prefix-arg
      (insert " ")
      (return-ilisp)))

(defun pvs-prover-ground ()
  "Insert and send the GROUND prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(ground)")
  (return-ilisp))

(defun pvs-prover-hide (&optional num)
  "Insert and send the HIDE prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Hide formulas: "))))
    (insert "(hide " fnum ")")
    (return-ilisp)))
  
(defun pvs-prover-iff (&optional num)
  "Insert and send the IFF prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "iff on formulas: " ""))))
    (insert "(iff " fnum ")")
    (return-ilisp)))

(defun pvs-prover-induct (var &optional num)
  "Insert and send the INDUCT prover command.
Prompts for the variable name.  Prefix arg gives formula number."
  (interactive "sVariable on which to induct: \nP")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Formula number: " "1")))
	(ischeme (read-from-minibuffer "Induction scheme [CR for default]: "
				       "")))
    (insert "(induct " ?\" var ?\" " " fnum) 
    (if (not (string= ischeme "" )) 
	(insert " " ?\" ischeme ?\" ")")
	(insert ")"))
    (return-ilisp)))

(defun pvs-prover-induct-and-simplify (var &optional num)
  "Insert and send the INDUCT-AND-SIMPLIFY command.
Prompts for the variable name.  Prefix arg gives formula number."
  (interactive "sVariable on which to induct-and-simplify: \nP")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Formula number: " "1")))
	(ischeme (read-from-minibuffer "Induction scheme [CR for default]: "
				       ""))
	(defs (read-from-minibuffer "DEFS flag: " "T"))
	(if-match (read-from-minibuffer "IF-MATCH flag: " "best")))
    (insert (format "(induct-and-simplify \"%s\" %d %s :defs %s :if-match %s)"
		var (or num 1)
		(if (not (string= ischeme "" ))
		    (format "\"%s\"" ischeme)
		    "")
		defs if-match))
    (return-ilisp)))

(defun pvs-prover-inst (&optional num)
  "Insert and send the INST prover command.
Prompts for formula number and expressions.
If present, prefix arg NUM gives the formula to apply to."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Formula to instantiate: "))))
    (pvs-soriorq-internal "inst" "expression" fnum nil)
    (return-ilisp)))

(defun pvs-prover-inst-question (&optional num)
  "Insert and send the INST? prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number.  Prompts for additional arguments as well."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Inst? in formulas [CR for default]: "
					"*")))
	(args (read-from-minibuffer "Additional arguments [CR for none]: "
				    "")))
    (insert "(inst? "
	    (if (not (or (null fnum) (string= fnum "*"))) fnum "")
	    (if (not (or (null args) (string= args ""))) (concat " " args) "")
	    ")")
    (return-ilisp)))

(defun pvs-prover-lemma (name)
  "Insert and send the LEMMA prover command.
Prompts for the lemma name."
  (interactive "sLemma Name: ")
  (goto-pvs-proof-buffer)
  (insert "(lemma " ?\" name ?\" ")")
  (return-ilisp))

(defun pvs-prover-lift-if (&optional num)
  "Insert and send the LIFT-IF prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "Lift-if in formula [CR for all]: "))))
    (insert "(lift-if " fnum ")")
    (return-ilisp)))

(defun pvs-prover-model-check ()
  "Insert and send the MODEL-CHECK prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(model-check)")
  (return-ilisp))

(defun pvs-prover-musimp (&optional num)
  "Insert and send the MUSIMP prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "musimp on formulas: " ""))))
    (insert "(musimp " fnum ")")
    (return-ilisp)))

(defun pvs-prover-name (name expr)
  "Insert and send the NAME prover command.
Prompts for the name and expression."
  (interactive "sName for expression: \nsExpression to name: ")
  (goto-pvs-proof-buffer)
  (insert "(name " ?\" name ?\" " " ?\" expr ?\" ")")
  (return-ilisp))

(defun pvs-prover-name-replace (name expr)
  "Insert and send the NAME-REPLACE prover command.
Prompts for the name and expression."
  (interactive "sName for expression: \nsExpression to name: ")
  (goto-pvs-proof-buffer)
  (insert "(name-replace " ?\" name ?\" " " ?\" expr ?\" ")")
  (return-ilisp))

(defun pvs-prover-postpone ()
  "Insert and send the POSTPONE prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(postpone)")
  (return-ilisp))

(defun pvs-prover-prop ()
  "Insert and send the PROP prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(prop)")
  (return-ilisp))

(defun pvs-prover-quit (&optional nosave)
  "Insert and send the QUIT prover command.
With prefix arg, forces quit with no save of the partial proof."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (insert "(quit)")
  (return-ilisp)
  (if (not (null nosave))
      (progn
	(insert "Y")
	(return-ilisp)
	(insert "no")
	(return-ilisp))))

(defun pvs-prover-replace (&optional num)
  "Insert and send the REPLACE prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		 (int-to-string (prefix-numeric-value num))
		(read-from-minibuffer "Replace using formula: ")))
	(nums (read-from-minibuffer "in the formulas: " "*"))
	(dir  (read-from-minibuffer "with direction: " "LR")))
    (insert "(replace " fnum " " nums " " dir ")")
    (return-ilisp)))

(defun pvs-prover-replace-eta ()
  "Insert and send the REPLACE-ETA prover command.
Prompts for the expression."
  (interactive)
  (goto-pvs-proof-buffer)
  (let ((term (read-from-minibuffer "Replace-eta on term: ")))
    (insert (format "(replace-eta \"%s\")" term))
    (return-ilisp)))

(defun pvs-prover-rewrite (lem &optional num)
  "Insert and send the REWRITE prover command.
Prompts for the lemma name and the formula to use.  Prefix arg may also be
used to give the formula number."
  (interactive "sLemma to Rewrite: \nP")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		  (read-from-minibuffer "in formulas [CR for default]: " "*")))
	(args (read-from-minibuffer "Additional arguments [CR for none]: "
				    "")))
    (insert (format "(rewrite \"%s\" %s %s)" lem
	      (if (not (or (null fnum) (string= fnum "*"))) num "")
	      (if (not (or (null args) (string= args ""))) args "")))
    (return-ilisp)))

(defun pvs-prover-skolem-bang (&optional num)
  "Insert and send the SKOLEM! prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		  (int-to-string (prefix-numeric-value num))
		(read-from-minibuffer
		 "Apply skolem! to formula [CR for default]: "))))
    (insert "(skolem! " fnum ")")
    (return-ilisp)))

(defun pvs-prover-skosimp (&optional num)
  "Insert and send the SKOSIMP prover command.
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		 (int-to-string (prefix-numeric-value num))
		(read-from-minibuffer
		 "Apply skosimp to formula [CR for default]: "))))
    (insert "(skosimp " fnum ")")
    (return-ilisp)))

(defun pvs-prover-skosimp-star ()
  "Insert and send the SKOSIMP* prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(skosimp*)")
  (return-ilisp))

(defun pvs-prover-split (&optional num)
  "Insert and send the SPLIT prover command
Prompts for the formula to use.  Prefix arg may also be used to give the
formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let ((fnum (if (not (null num))
		 (int-to-string (prefix-numeric-value num))
		(read-from-minibuffer "Split formula [CR for all]: "))))
    (insert "(split " fnum ")")
    (return-ilisp)))

(defun pvs-prover-tcc ()
  "Insert and send the TCC prover command."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert "(tcc)")
  (return-ilisp))

(defun pvs-prover-then ()
  "Insert and send the THEN prover strategy.
Prompts for the strategies comprising it."
  (interactive)
  (goto-pvs-proof-buffer)
  (let ((arglist "")
	(nextarg (read-from-minibuffer "Strategy (no outer parens): " "")))
    (while (not (string= nextarg ""))
      (setq arglist (concat arglist "(" nextarg ") "))
      (setq nextarg (read-from-minibuffer "Strategy (no outer parens): " "")))
    (if (not (string= arglist ""))
	(progn
	  (insert "(then " arglist ")")
	  (return-ilisp)))))

(defun pvs-prover-typepred (&optional prompt start end)
  "Insert and send the TYPEPRED prover command
Uses expression between point and mark.  With prefix arg prompts for the
expression.  In Lucid Emacs, must set mark before calling this function,
even if you give a prefix argument"
  (interactive "P\nr")
  (goto-pvs-proof-buffer)
  (let ((expr ""))
    (if (or prompt (eobp) (at-pvs-rule-prompt))
	(setq expr (read-from-minibuffer "Expression to typepred: "))
      (setq expr (buffer-substring start end))
      (if (not (y-or-n-p (concat "Typepred for " expr)))
	  (error "typepred aborted.")))
    (end-of-buffer)
    (insert "(typepred " ?\" expr ?\" ")")
    (return-ilisp)))

(defun at-pvs-rule-prompt ()
  (save-excursion
    (set-buffer "*pvs*")
    (beginning-of-line)
    (looking-at "Rule\?")))

(defun pvs-prover-undo (num)
  "Insert and send the UNDO prover command.
Prefix arg may be used to give number of steps to undo."
  (interactive "p")
  (goto-pvs-proof-buffer)
  (insert (format "(undo %d)y" num))
  (return-ilisp))

(defun pvs-prover-wrap-with-parens ()
  "Puts parentheses around the current line and sends a return."
  (interactive)
  (goto-pvs-proof-buffer)
  (let ((pt (point)))
    (search-backward "Rule? ") (search-forward " ")
    (insert "(")
    (goto-char (1+ pt))
    (insert ")")
    (return-ilisp)))

(if (string-match "XEmacs" (emacs-version))
    (progn
      (defun my-delete-extent (ext dummy)
	(delete-extent ext))
      (if (x-color-display-p)
	  (progn
	    (copy-face 'default 'current-proof-step-face)
	    (set-face-foreground 'current-proof-step-face "red")
	    (copy-face 'default 'completed-proof-steps-face)
	    (set-face-foreground 'completed-proof-steps-face "blue"))
	  (copy-face 'bold 'completed-proof-steps-face)
	  (copy-face 'bold-italic 'completed-proof-steps-face))))


;;; Proof stepper functions

(defun pvs-prover-run-proof-step (num)
  "Execute one step of proof.
Assumes that a Proof buffer exists."
  (interactive "p")
  (goto-pvs-proof-buffer)
  (let ((pvsbuf (current-buffer))
	(editprfbuf (get-buffer "Proof"))
	(cmd nil))
    (unless editprfbuf
      (error "Must have a Proof Buffer."))
    (while (and pvs-in-checker (> num 0))
      (save-excursion
	(set-buffer editprfbuf)
	(pvs-prover-goto-next-step)
	(let ((beg (point))
	      (end (progn (forward-sexp 1) (point))))
	  (pvs-prover-goto-next-step)
	  (let ((editprfwin (get-buffer-window editprfbuf)))
	    (if editprfwin
		(set-window-point editprfwin (point))))
	  (setq cmd (buffer-substring beg end))))
      (end-of-buffer)
      (insert cmd)
      (return-ilisp)
      (hilit-next-prover-command)
      (setq num (- num 1))
      (when (> num 0)
	(accept-process-output (ilisp-process))
	(sit-for 0)
	(while (and pvs-in-checker
		    (not (at-pvs-rule-prompt)))
	  (accept-process-output (ilisp-process))
	  (sit-for 0))))))

(defun pvs-prover-skip-proof-step (num)
  "Skips one step of proof.
Assumes that an Proof buffer exists."
  (interactive "p")
  (goto-pvs-proof-buffer)
  (let ((pvsbuf (current-buffer))
	(editprfbuf (get-buffer "Proof"))
	(cmd nil))
    (unless editprfbuf
      (error "Must have an Edit Proof Buffer."))
    (save-excursion
      (set-buffer editprfbuf)
      (if (< num 0)
	  (dotimes (i (- num))
	    (pvs-prover-goto-prev-step))
	  (dotimes (i num)
	    (pvs-prover-goto-next-step)
	    (forward-sexp 1)
	    (pvs-prover-goto-next-step)))
      (hilit-next-prover-command)
      (let ((editprfwin (get-buffer-window editprfbuf)))
	(if editprfwin
	    (set-window-point editprfwin (point)))))))

(defun pvs-prover-undo-proof-step (num)
  "Undoes the previous proof step.  Assumes an Edit Proof buffer exists."
  (interactive "p")
  (goto-pvs-proof-buffer)
  (if (<= num 0)
      (message "Must provide a positive integer to undo")
      (let ((pvsbuf (current-buffer))
	    (editprfbuf (get-buffer "Proof")))
	(unless editprfbuf
	  (error "Must have a Proof Buffer."))
	(pvs-prover-undo num)
	(switch-to-buffer-other-window editprfbuf)
	(dotimes (i num)
	  (pvs-prover-goto-prev-step t))
	(hilit-next-prover-command)
	(switch-to-buffer pvsbuf)
	(end-of-buffer)
	(switch-to-buffer editprfbuf)
	(pop-to-buffer pvsbuf)
	(end-of-buffer))))


;;; pvs-prover-goto-next-step puts the cursor at the beginning of the next
;;; proof command in the buffer.  The next proof command is determined by
;;; looking for a left paren followed by anything but a left paren or a
;;; \".  If it is already at a proof command, this function does nothing.

(defun pvs-prover-goto-next-step ()
  "Called from an Edit Proof buffer, goes to next step in proof.  NOTE:
The regexp looks for a substring consisting of a left paren followed by
anything but a left paren or a \", ignoring whitespace."
  (interactive "p")
  (let ((case-fold-search t))
    (if (not (re-search-forward "([ \t\n\r]*[^(\" \t\n\r]" (point-max) t))
	(progn
	  (goto-char (point-max))
	  (message "No more proof statements."))
	(goto-char (match-beginning 0))
	(if (looking-at "([ \t\n\r]*[Pp][Rr][Oo][Pp][Ax][Xx][ \t\n\r]*)")
	    (progn (forward-char 4) (pvs-prover-goto-next-step))
	    (if (save-excursion
		  (or (in-comment) (in-string)))
		(progn (forward-char 1) (pvs-prover-goto-next-step)))))))


;;; Puts the cursor at the beginning of the previous command in the Proof
;;; buffer.  It has special handling for branches, so that it may be used
;;; to support undo.
		    
(defun pvs-prover-goto-prev-step (&optional undop)
  "Called from an Edit Proof buffer, goes to prev step in proof.
The regexp looks for a substring consisting of a left paren followed by
anything but a left paren or a \", ignoring whitespace."
  (interactive "p")
  (if (not (re-search-backward "([ \t\n\r]*[^(\" \t\n\r]" (point-min) t))
      (progn
	(goto-char (1+ (point-min)))
	(pvs-prover-goto-next-step)
	(message "No earlier proof statements."))
      (when undop
	(forward-sexp 1)
	(if (not (looking-at "[ \n\t\r]*\)"))
	    (forward-sexp -1)
	    (progn
	      (re-search-forward "[ \n\t\)]*")
	      (forward-sexp -1)
	      (unless (re-search-backward "([ \t\n\r]*[^(\" \t\n\r]" (point-min) t)
		(goto-char (1+ (point-min)))
		(pvs-prover-goto-next-step)
		(message "No earlier proof statements.")))))
      (if (looking-at "([ \t\n\r]*[Pp][Rr][Oo][Pp][Ax][Xx][ \t\n\r]*)")
	  (pvs-prover-goto-prev-step undop))))

(defun pvs-prover-goto-prev-step (&optional undop)
  "Called from an Edit Proof buffer, goes to prev step in proof."
  (interactive "p")
  (let ((cpoint (point)))
    (goto-char (point-min))
    (pvs-prover-goto-next-step)
    (let ((ppoint (point)))
      (while (< (point) cpoint)
	(setq ppoint (point))
	(forward-sexp 1)
	(pvs-prover-goto-next-step))
      (goto-char ppoint))))

(defun pvs-prover-quotes ()
  "Inserts double quote marks."
  (interactive)
  (goto-pvs-proof-buffer)
  (insert ?\")
  (save-excursion
    (insert ?\")))
    

;;; Non-interactive Functions
    
(defun pvs-soriorq-internal (which prompt num paren)
  "Internal function to insert a skolem or inst command."
  (let* ((count 1)
	 (arglist "")
	 (nextarg (read-from-minibuffer (concat which " " prompt " "
						(number-to-string count)
						" [CR to quit]: ")
					"")))
    (while (not (string= nextarg ""))
      (setq count (1+ count))
      (setq arglist (concat arglist "\"" nextarg "\" ")) 
      (setq nextarg (read-from-minibuffer (concat which " " prompt " "
						  (number-to-string count)
						  " [CR to quit]: ")
					  "")))
    (if (string= arglist "")
	(insert (concat "(" which "! " num ")"))
	(insert (concat "(" which " " num 
			(if paren " ("  " ")
			arglist
			(if paren "))" ")")
			)))))


(defun pvs-prover-guess-at-expand ()
  (let ((def2expand "")
	(cpt (point))
	(tried-alist nil))
    (if (re-search-backward "^{[0-9]*}" (point-min) t)
	(let ((found-expansion nil))
	  (goto-char (match-end 0))
	  (skip-chars-forward "^a-zA-Z")
	  (let ((sbeg (point)))
	    (skip-chars-forward "a-zA-Z0-9_?$")
	    (setq def2expand (buffer-substring sbeg (point))))
	  (setq found-expansion
		(and 
		 (not (member (downcase def2expand) pvs-reserved-words))
		 (y-or-n-p (concat "Expand " def2expand " "))))
	  (while (not found-expansion)
	    (setq tried-alist (cons (list (downcase def2expand))
				    tried-alist))
	    (skip-chars-forward "^a-zA-Z")
	    (let ((sbeg (point)))
	      (skip-chars-forward "a-zA-Z0-9_?$")
	      (setq def2expand (buffer-substring sbeg (point))))
	    (setq found-expansion
		  (and 
		   (not (member (downcase def2expand) pvs-reserved-words))
		   (not (assoc (downcase def2expand) tried-alist))
		   (y-or-n-p (concat "Expand " def2expand " ")))))
	  (goto-char cpt)))
    (let ((fnum (read-from-minibuffer "in formula [CR for default]# " "")))
      (insert "(expand " ?\" def2expand ?\" " " fnum ")")
      (return-ilisp))))

(defun hilit-next-prover-command ()
  (let ((editprfbuf (get-buffer "Proof")))
    (save-excursion
      (set-buffer editprfbuf)
      (unless (= (buffer-size) 0)
	(cond ((string-match "GNU Emacs" (emacs-version))
	       (let ((beg (point))
		     (end (save-excursion (forward-sexp 1) (point))))
		 (hilit-proof-region beg end)))
	      ((string-match "XEmacs" (emacs-version))
	       (let ((beg (point))
		     (end (save-excursion (forward-sexp 1) (point))))
		 (map-extents 'my-delete-extent (current-buffer) (point-min)
			      (1- beg))
		 (set-extent-face (make-extent (point-min) (1- beg))
				  'completed-proof-steps-face)
		 (set-extent-face (make-extent beg end)
				  'current-proof-step-face))))))))

(defun hilit-proof-region (start end)
  (delete-hilit-overlays)
  (overlay-put (make-overlay start end) 'face 'highlight))

(defun delete-hilit-overlays ()
  (let ((start 0)
	(endb (point-max)))
    (while (< start endb)
      (dolist (overlay (overlays-at start))
	(when (eq (plist-get (overlay-properties overlay) 'face) 'highlight)
	  (delete-overlay overlay)))
      (setq start (1+ start)))))

;;
;; Key Bindings
;;

(defvar pvs-prover-helps-map (make-sparse-keymap)
  "Key map for help functions.")

(define-key pvs-prover-helps-map "h"     'help-pvs-prover-emacs)
(define-key pvs-prover-helps-map "H"     'pvs-help-prover-command)

(define-key pvs-prover-helps-map "\C-i"  'pvs-prover-any-command)
(define-key pvs-prover-helps-map "E"     'pvs-prover-apply-extensionality)
(define-key pvs-prover-helps-map "a"     'pvs-prover-assert)
(define-key pvs-prover-helps-map "A"     'pvs-prover-auto-rewrite)
(define-key pvs-prover-helps-map "\C-a"  'pvs-prover-auto-rewrite-theory)
(define-key pvs-prover-helps-map "B"     'pvs-prover-bddsimp)
(define-key pvs-prover-helps-map "b"     'pvs-prover-beta)
(define-key pvs-prover-helps-map "c"     'pvs-prover-case)
(define-key pvs-prover-helps-map "C"     'pvs-prover-case-replace)
(define-key pvs-prover-helps-map "2"     'pvs-prover-copy)
(define-key pvs-prover-helps-map "="     'pvs-prover-decompose-equality)
(define-key pvs-prover-helps-map "d"     'pvs-prover-delete)
(define-key pvs-prover-helps-map "D"     'pvs-prover-do-rewrite)
(define-key pvs-prover-helps-map "e"     'pvs-prover-expand)
(define-key pvs-prover-helps-map "x"     'pvs-prover-extensionality)
(define-key pvs-prover-helps-map "f"     'pvs-prover-flatten)
(define-key pvs-prover-helps-map "G"     'pvs-prover-grind)
(define-key pvs-prover-helps-map "g"     'pvs-prover-ground)
(define-key pvs-prover-helps-map "\C-h"  'pvs-prover-hide)
(define-key pvs-prover-helps-map "F"     'pvs-prover-iff)
(define-key pvs-prover-helps-map "I"     'pvs-prover-induct)
(define-key pvs-prover-helps-map "\C-s"  'pvs-prover-induct-and-simplify)
(define-key pvs-prover-helps-map "i"     'pvs-prover-inst)
(define-key pvs-prover-helps-map "?"     'pvs-prover-inst-question)
(define-key pvs-prover-helps-map "L"     'pvs-prover-lemma)
(define-key pvs-prover-helps-map "l"     'pvs-prover-lift-if)
(define-key pvs-prover-helps-map "M"     'pvs-prover-model-check)
(define-key pvs-prover-helps-map "m"     'pvs-prover-musimp)
(define-key pvs-prover-helps-map "n"     'pvs-prover-name)
(define-key pvs-prover-helps-map "N"     'pvs-prover-name-replace)
(define-key pvs-prover-helps-map "P"     'pvs-prover-postpone)
(define-key pvs-prover-helps-map "p"     'pvs-prover-prop)
(define-key pvs-prover-helps-map "\C-q"  'pvs-prover-quit)
(define-key pvs-prover-helps-map "r"     'pvs-prover-replace)
(define-key pvs-prover-helps-map "8"     'pvs-prover-replace-eta)
(define-key pvs-prover-helps-map "R"     'pvs-prover-rewrite)
(define-key pvs-prover-helps-map "!"     'pvs-prover-skolem-bang)
(define-key pvs-prover-helps-map "S"     'pvs-prover-skosimp)
(define-key pvs-prover-helps-map "*"     'pvs-prover-skosimp-star)
(define-key pvs-prover-helps-map "s"     'pvs-prover-split)
(define-key pvs-prover-helps-map "T"     'pvs-prover-tcc)
(define-key pvs-prover-helps-map "\C-t"  'pvs-prover-then)
(define-key pvs-prover-helps-map "t"     'pvs-prover-typepred)
(define-key pvs-prover-helps-map "u"     'pvs-prover-undo)

(define-key pvs-prover-helps-map "1"     'pvs-prover-run-proof-step)
(define-key pvs-prover-helps-map "U"     'pvs-prover-undo-proof-step)
(define-key pvs-prover-helps-map "#"     'pvs-prover-skip-proof-step)

(define-key pvs-prover-helps-map "'"     'pvs-prover-quotes)
(define-key pvs-prover-helps-map "\C-j"  'pvs-prover-wrap-with-parens)

(define-key edit-proof-mode-map "\C-i" pvs-prover-helps-map)

(defun pvs-do-key-binding ()
  "This allows this file to be loaded before PVS is started if necessary."
  (interactive)
  (define-key ilisp-mode-map "\C-c\C-i"  'indent-line-ilisp)
  (define-key ilisp-mode-map "\C-i"      pvs-prover-helps-map)
  (define-key ilisp-mode-map "\C-j"      'pvs-prover-wrap-with-parens))

(if (boundp 'ilisp-mode-map)
    (pvs-do-key-binding))

(provide 'pvs-prover-helps)

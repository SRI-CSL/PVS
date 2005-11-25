;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-key.el --
;;; ILISP keybinding definitions.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

;;; ilisp-where-is --
;;; New version provided by yusuf@SPD-13.ils.nwu.edu (Yusuf Pisan)
;;; Note: this used to be in 'ilisp-cpat'. Its definition did not make
;;;       much sense. Yusuf noted this and I decided to move it in
;;;       this file (where I think is more approriate).
;;;       11/24/94: Marco Antoniotti

(defun ilisp-where-is (command)
  (let ((cmd (where-is-internal command nil t)))
    (when cmd
      (key-description cmd))))


;;;
;;;%Bindings
(defun ilisp-safe-define-key (keymap key command &optional fsf-key)
  "In KEYMAP, bind KEY to COMMAND.
If optional fourth argument FSF-KEY is non-nil, then iff
`ilisp-*use-fsf-compliant-keybindings*' is non-nil, bind FSF-KEY
instead of KEY, unless FSF-KEY is a symbol, in which case do nothing."
  ;; Check boundp as well as nilp -- paranoia always pays, and this
  ;; code only gets run at setup time anyway:
  (if (and fsf-key
           (boundp 'ilisp-*use-fsf-compliant-keybindings*)
           ilisp-*use-fsf-compliant-keybindings*)
      (setq key fsf-key))
  (unless (symbolp key)
    (define-key keymap key command)))

(defun ilisp-bind-ilisp-key-for-map (keymap key command &optional fsf-key)
  "In KEYMAP, bind ilisp-*prefix*+KEY to COMMAND.
If optional fourth argument FSF-KEY is non-nil, then iff
`ilisp-*use-fsf-compliant-keybindings*' is non-nil, bind FSF-KEY
instead of KEY, unless FSF-KEY is a symbol, in which case do nothing."
  (let ((prefix-map (lookup-key keymap ilisp-*prefix*)))
    (unless (keymapp prefix-map)
      (setq prefix-map
	    (define-key keymap ilisp-*prefix* (make-sparse-keymap))))
    (ilisp-safe-define-key prefix-map key command fsf-key)))

(defun defkey-ilisp (key command &optional inferior-only-p fsf-key)
  "Define KEY as COMMAND in 'ilisp-mode-map' and 'lisp-mode-map'.
The change happens only if optional INFERIOR-ONLY-P is NIL.  If the maps
do not exist they will be created.  This should only be called after
ilisp-*prefix* is set to the desired prefix."
  (unless ilisp-mode-map (ilisp-bindings))
  (ilisp-safe-define-key ilisp-mode-map key command fsf-key)
  (unless inferior-only-p
    (ilisp-safe-define-key lisp-mode-map key command fsf-key)))

;;;
(defun lisp-bindings (keymap &optional inferior-p)
  "Sets up the bindings for interacting with an inferior LISP in KEYMAP."
  (cond (inferior-p
	 (define-key keymap "\C-m" 'return-ilisp)
	 (define-key keymap "\C-a" 'bol-ilisp)
	 (define-key keymap "\C-c\C-c" 'interrupt-subjob-ilisp)
	 (define-key keymap "\C-d" 'delete-char-or-pop-ilisp)
         ;; note: "#" is technically a violation of FSF keybinding
         ;; conventions, but we won't pass an alternate here because
         ;; it's not likely to cause a conflict in practice:
	 (ilisp-bind-ilisp-key-for-map keymap "#" 'raw-keys-ilisp))
	(t
	 (ilisp-bind-ilisp-key-for-map
          keymap "\C-c" 'compile-defun-and-go-lisp "\M-c")
	 (define-key keymap "\C-m" 'newline-and-indent-lisp)))

  ;; 19990901 Martin Atzmueller
  ;; 20000203 Karl Fogel: it's already bound to M-TAB anyway:
  (ilisp-safe-define-key keymap "\C-c\t" 'complete-lisp 'no-fsf-key)
  (define-key keymap [?\C-c return] `complete)

  ;; 20000401 Martin Atzmueller
  ;; Reinstated the ilisp-arglist-message-lisp-space by adding
  ;; a customization. C-c C-SPACE is _not_ the intended behavior.
  
  ;; 19991214 Martin Atzmueller

  ;; 20000203 Karl Fogel: C-c C-SPACE in the FSF-universe, I guess.
  ;; (ilisp-safe-define-key
  ;; keymap " "  'ilisp-arglist-message-lisp-space [?\C-c?\C- ])
  (when ilisp-bindings-*bind-space-p*
    (define-key keymap " "  'ilisp-arglist-message-lisp-space))

  ;; 20000203 Karl Fogel
  ;; This binding of ] causes many complaints, because lisp hackers
  ;; frequently need literal square braces in their code.  The
  ;; 'close-all-lisp function is a neat idea, but I think it needs to
  ;; be bound to something not used for any other purpose.  -karl
  ;; (define-key   keymap "]"        'close-all-lisp)
  ;;
  ;; 20000213 Marco Antoniotti
  ;; Reinstated the 'close-all' lisp by adding a programmable
  ;; customization.
  (when ilisp-bindings-*bind-right-bracket-p*
    (define-key   keymap "]"        'close-all-lisp))

  (define-key   keymap "\M-q"              'reindent-lisp)
  (ilisp-safe-define-key keymap "\C-]"     'close-and-send-lisp 'no-fsf-key)
  (define-key   keymap "\t"                'indent-line-ilisp)
  (define-key   keymap "\n"                'newline-and-indent-lisp)
  (define-key   keymap "\M-\C-q"           'indent-sexp-ilisp)
  (ilisp-bind-ilisp-key-for-map keymap ";" 'comment-region-lisp)

  ;; note: again, a technical violation of FSF keybinding policy, but
  ;; safe & useful enough that I think it's best to leave it as is:
  (ilisp-bind-ilisp-key-for-map keymap ")"        'find-unbalanced-lisp)

  (define-key   keymap "\M-\C-a"  'beginning-of-defun-lisp)
  (define-key   keymap "\M-\C-e"  'end-of-defun-lisp)
  (ilisp-safe-define-key keymap "\C-\M-r" 'reposition-window-lisp 'no-fsf-key)

  ;; This series of bindings was very non-FSF-compliant, but was also
  ;; hard to fit into any consistent binding scheme.  I saved them for
  ;; last and then bound them to whatever was available.  -Karl Fogel
  (ilisp-bind-ilisp-key-for-map keymap "i" 'describe-lisp      "\C-i")
  (ilisp-bind-ilisp-key-for-map keymap "I" 'inspect-lisp       "\M-i")
  (ilisp-bind-ilisp-key-for-map keymap "a" 'arglist-lisp       "\C-q")
  (ilisp-bind-ilisp-key-for-map keymap "d" 'documentation-lisp "\C-f")
  (ilisp-bind-ilisp-key-for-map keymap "m" 'macroexpand-1-lisp "\M-1")
  (ilisp-bind-ilisp-key-for-map keymap "M" 'macroexpand-lisp   "\M-0")

  (ilisp-safe-define-key keymap "\M-," 'next-definition-lisp 'no-fsf-key)
  (ilisp-safe-define-key keymap "\M-." 'edit-definitions-lisp 'no-fsf-key)
  (ilisp-safe-define-key keymap "\M-?" 'search-lisp 'no-fsf-key)
  (ilisp-safe-define-key keymap "\M-\"" 'replace-lisp 'no-fsf-key)
  (ilisp-bind-ilisp-key-for-map keymap "^" 'edit-callers-lisp 'no-fsf-key)
  (ilisp-safe-define-key keymap "\M-`" 'next-caller-lisp 'no-fsf-key)
  (define-key keymap "\M-\t" 'complete-lisp)

  ;; note: another technical fsf keybinding policy violation.  But
  ;; M-return is unbound in the FSF Emacs 20.5 distribution, and I
  ;; think a lot of people might like this binding.  I don't know,
  ;; really, it's just a judgement call.  -karl
  (define-key keymap "\M-\C-m"  'complete)

  (ilisp-bind-ilisp-key-for-map keymap "r"       'eval-region-lisp "\C-r")
  (ilisp-safe-define-key        keymap "\M-\C-x" 'eval-defun-lisp) ; like Gnu
  (ilisp-bind-ilisp-key-for-map keymap "e"       'eval-defun-lisp "\C-e")
  (ilisp-bind-ilisp-key-for-map keymap "n"       'eval-next-sexp-lisp "\C-n")
  
  ;; Changed as per Martin Atzmueller suggestions.
  ;; Original version
  ;; (ilisp-bind-ilisp-key-for-map keymap "p"        'package-lisp)
  ;;
  ;; todo: will there ever be `*-previous-*' functions defined,
  ;; analogous to `eval-next-sexp' etc?  If so, then the binding of
  ;; p/C-p below will be problematic.  -karl
  (ilisp-bind-ilisp-key-for-map keymap "p" 'set-buffer-package-lisp "\C-p")

  (ilisp-bind-ilisp-key-for-map keymap "P" 'set-package-lisp "\M-p")
  (ilisp-bind-ilisp-key-for-map keymap "w" 'compile-region-lisp "\C-w")
  ;; MA 09/01/1999:
  (ilisp-bind-ilisp-key-for-map keymap "\C-b" 'ilisp-compile-buffer)
  (ilisp-bind-ilisp-key-for-map keymap "c"    'compile-defun-lisp     "\C-c")
  (ilisp-bind-ilisp-key-for-map keymap "\C-r" 'eval-region-and-go-lisp "\M-r")
  (ilisp-bind-ilisp-key-for-map keymap "\C-e" 'eval-defun-and-go-lisp "\M-e")
  (ilisp-bind-ilisp-key-for-map keymap "\C-n"
                                'eval-next-sexp-and-go-lisp
                                "\M-n")
  (ilisp-bind-ilisp-key-for-map keymap "\C-w"
                                'compile-region-and-go-lisp
                                "\M-w")
  (ilisp-bind-ilisp-key-for-map keymap "t" 'trace-defun-lisp "\C-t")
  (ilisp-bind-ilisp-key-for-map keymap "!" 'default-directory-lisp 'no-fsf-key)
  (ilisp-bind-ilisp-key-for-map keymap " " 'mark-change-lisp 'no-fsf-key)

  ;; These four are under the further "*"/"8" prefix:
  (ilisp-bind-ilisp-key-for-map keymap "*l" 'list-changes-lisp "8l")
  (ilisp-bind-ilisp-key-for-map keymap "*e" 'eval-changes-lisp "8e")
  (ilisp-bind-ilisp-key-for-map keymap "*c" 'compile-changes-lisp "8c")
  (ilisp-bind-ilisp-key-for-map keymap "*0" 'clear-changes-lisp "80")

  (ilisp-bind-ilisp-key-for-map keymap "b" 'switch-to-lisp "\M-b")
  (ilisp-bind-ilisp-key-for-map keymap "y" 'call-defun-lisp "\C-y")
  (ilisp-bind-ilisp-key-for-map keymap "z" 'reset-ilisp "\C-z")
  (ilisp-bind-ilisp-key-for-map keymap "g" 'abort-commands-lisp "\C-g")
  (ilisp-bind-ilisp-key-for-map keymap "s" 'status-lisp "\C-s")
  (ilisp-bind-ilisp-key-for-map keymap "S" 'select-ilisp "\M-s")
  (define-key   keymap "\C-x\C-f" 'find-file-lisp)
  (ilisp-bind-ilisp-key-for-map keymap "l" 'load-file-lisp "\C-l")
  (ilisp-bind-ilisp-key-for-map keymap "k" 'compile-file-lisp "\C-k")

  ;; Conditionalized definitions of these keybindings, using the
  ;; appropriate flags.
  ;;
  ;; 19990824 Marco Antoniotti

  (when ilisp-*use-fi-clman-interface-p*
    (ilisp-bind-ilisp-key-for-map keymap "A" 'fi:clman-apropos "\M-a")
    (ilisp-bind-ilisp-key-for-map keymap "D" 'fi:clman "\M-d"))
  (when ilisp-*use-hyperspec-interface-p*
    (ilisp-bind-ilisp-key-for-map keymap "H" 'hyperspec-lookup "\M-h"))
  (when ilisp-*use-cltl2-interface-p*
    (ilisp-bind-ilisp-key-for-map keymap "L" 'cltl2-lookup "\M-l")))



;;
(defun ilisp-lispm-bindings ()
  "Setup additional Lisp Machine-like bindings for some ilisp commands"
  (interactive)
  ;; Note: Changed the 'ilisp-emacs-version-id' to
  ;;       '+ilisp-emacs-version-id+' and the 'gnu-*' to 'fsf-*'.
  ;;       25/11/94 Marco Antoniotti
  ;;
  ;; Note: these bindings do not have to be FSF-compliant, because the
  ;;       user doesn't get them unless she asks for them, in which
  ;;       case she presumably knows what she wants. -Karl Fogel, 3 Feb 2000
  (cond ((eq +ilisp-emacs-version-id+ 'fsf-18))
	((or (eq +ilisp-emacs-version-id+ 'fsf-19)
	     (eq +ilisp-emacs-version-id+ 'fsf-20)
	     (eq +ilisp-emacs-version-id+ 'fsf-21))
	 (defkey-ilisp (read "[?\\S-\\C-a]") 'arglist-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-c]") 'compile-defun-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-d]") 'documentation-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-e]") 'eval-defun-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-m]") 'macroexpand-1-lisp)
	 (defkey-ilisp (read "[?\\M-M]") 'macroexpand-lisp))
	(t
	 (defkey-ilisp '(control A) 'arglist-lisp)
	 (defkey-ilisp '(control C) 'compile-defun-lisp)
	 (defkey-ilisp '(control D) 'documentation-lisp)
	 (defkey-ilisp '(control E) 'eval-defun-lisp)
	 (defkey-ilisp '(control M) 'macroexpand-1-lisp)
	 (defkey-ilisp '(meta M) 'macroexpand-lisp))))

;; Unfortunately, the read kludges are needed for this function to work
;; for GNU emacs 19 when it was compiled by Lucid.




;;;
(defun ilisp-bindings ()
  "Set up the key bindings for LISP and ILISP buffers."
  (cond ((fboundp 'set-keymap-parent) 
	 (setq ilisp-mode-map (make-sparse-keymap))
	 (set-keymap-parent ilisp-mode-map comint-mode-map))
	(t (setq ilisp-mode-map (copy-keymap comint-mode-map))))

  ;; Remove stop and quit subjob from comint
  (define-key ilisp-mode-map "\C-c\C-z" nil)
  (define-key ilisp-mode-map "\C-c\C-\\" nil)

  (when (fboundp 'lisp-mode-commands)
    (lisp-mode-commands ilisp-mode-map))
  (lisp-bindings ilisp-mode-map t)
  (when (boundp 'lisp-mode-map)
    (lisp-bindings lisp-mode-map))
  (when (boundp 'scheme-mode-map) 
    (lisp-bindings scheme-mode-map))
  (ilisp-bind-ilisp-key-for-map emacs-lisp-mode-map ";" 'comment-region-lisp)

  (ilisp-bind-ilisp-key-for-map global-map "\C-t"
                                'trace-defun-lisp-break
                                "\M-t")
  (ilisp-bind-ilisp-key-for-map global-map "b" 'switch-to-lisp 'no-fsf-key)

  ;; Globally defined output-control commands.
  (ilisp-bind-ilisp-key-for-map global-map "1" 'pvs-bury-output)
  (ilisp-bind-ilisp-key-for-map global-map "v" 'ilisp-scroll-output "\C-v")
  (ilisp-bind-ilisp-key-for-map global-map "G" 'ilisp-grow-output "\M-g")

  ;; Added test to conditionalize the loading of the fi:clman map.
  ;;
  ;; 19990824 Marco Antoniotti

  (when ilisp-*use-fi-clman-interface-p*
    (unless (boundp 'fi:clman-mode-map)
      (setq fi:clman-mode-map (make-sparse-keymap)))
    (ilisp-bind-ilisp-key-for-map fi:clman-mode-map "D"
                                  'fi:clman
                                  "\M-d")
    (ilisp-bind-ilisp-key-for-map fi:clman-mode-map "A"
                                  'fi:clman-apropos
                                  "\M-a")))

(provide 'ilisp-key)

;;; end of file -- ilisp-key.el --

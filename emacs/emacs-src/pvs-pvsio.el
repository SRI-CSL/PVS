;; pvsio.el
;; Emacs Lisp code
;; Release : PVSio-2.d (11/09/05)

; pvsio
(defpvs pvsio prove (theory)
  "Invokes PVSio in the context of the given PVS theory"
  (interactive (progn (confirm-not-in-checker)
		      (complete-theory-name "Use context of theory: ")))
  (confirm-not-in-checker)
  (unless (interactive-p) (pvs-collect-theories))
  (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" theory)
			     nil 'tc nil)
    (error "%s is not typechecked" theory)) 
  (pvs-send-and-wait "(load-pvs-attachments)" nil nil 'dont-care)
  (pvs-evaluator-busy)
  (save-some-pvs-buffers)
  (pvs-bury-output)
  (ilisp-send (format "(evaluation-mode-pvsio \"%s\")" theory) nil 'pr t))

; pvs-ground-evaluator
(defpvs pvs-ground-evaluator prove (theory)
  "Invokes the ground evaluator in the context of the given PVS theory"
  (interactive (complete-theory-name "Use context of theory: "))
  (unless (interactive-p) (pvs-collect-theories))
  (confirm-not-in-checker)
  (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" theory)
			     nil 'tc nil)
    (error "%s is not typechecked" theory)) 
  (pvs-send-and-wait "(load-pvs-attachments)" nil nil 'dont-care)
  (pvs-evaluator-busy)
  (save-some-pvs-buffers)
  (pvs-bury-output)
  (ilisp-send (format "(evaluation-mode \"%s\")" theory) nil 'pr t))

; Auxiliary functions
(defun complete-attachment-name (prompt)
  (let* ((attachments (pvs-send-and-wait "(attachment-names)"
					 nil nil 'list))
	 (attachment (completing-read prompt 
				      (mapcar 'list attachments) nil t)))
    (if (equal attachment "")
	(error "Must provide a semantic attachment name")
      (list attachment))))

(defun print-info-buffer (name info)
  (when info
    (let ((buffer (get-buffer-create name)))
      (save-excursion
	(set-buffer buffer)
	(erase-buffer)
	(insert info)
	(fill-region (point-min) (point-max))
	(goto-char (point-min)))
      (display-buffer buffer))))

(defun complete-attachment-theory (prompt)
  (let* ((theories (pvs-send-and-wait "(attachment-theories)"
				      nil nil 'list))
	 (theory (completing-read prompt (mapcar 'list theories))))
    (if (equal theory "")
	(error "Must provide a theory name")
      (list theory))))

; load-pvs-attachments
(defpvs load-pvs-attachments environment ()
  "Reloads .pvs-attachments and pvs-attachments files

The load-pvs-attachments command forces a reload of semantic attachments 
from the current and imported contexts."
  (interactive)
  (pvs-send-and-wait "(load-pvs-attachments t)" nil nil 'dont-care))

; reload-pvsio
(defpvs reload-pvsio environment ()
  "Restarts PVSio

The reload-pvsio command restarts PVSio."
  (interactive)
  (pvs-send-and-wait "(reload-pvsio)" nil nil 'dont-care))

; pvsio-version
(defpvs pvsio-version environment ()
  "Shows PVSio version

The pvsio-version command shows the current version of PVSio."
  (interactive)
  (pvs-send-and-wait "(pvsio-version)" nil nil 'dont-care))

; list-attachments 
(defpvs list-attachments environment ()
  "Lists semantic attachments 

The list-attachments command lists the semantic attachments loaded in 
the current context."
  (interactive)
  (let ((info (pvs-send-and-wait "(list-attachments-str)" nil nil 'string)))
    (print-info-buffer "PVS Semantic Attachments" info)))

; help-pvs-attachment
(defpvs help-pvs-attachment help (attachment)
  "Displays help for a semantic attachment

The help-pvs-attachment command displays help for a semantic attachment 
in the Prover Help buffer."
  (interactive (complete-attachment-name "Help for semantic attachment: "))
  (let ((doc (pvs-send-and-wait (format "(help-attachment-str \"%s\")" 
					attachment)
				nil nil 'string)))
    (print-info-buffer "PVS Semantic Attachments Help" doc)))

(defpvs help-pvs-theory-attachments help (theory)
  "Displays help for semantic attachments in a theory

The help-pvs-theory-attachments command displays help for semantic attachments 
in a theory in the Prover Help buffer."
  (interactive (complete-attachment-theory 
		"Help for semantic attachments in theory: "))
  (let ((doc (pvs-send-and-wait 
	      (format "(help-theory-attachments-str \"%s\")" theory)
	      nil nil 'string)))
    (print-info-buffer "PVS Semantic Attachments Help" doc)))

; Miscellaneous

(global-set-key "\C-c\C-ha" 'help-pvs-attachment)

(global-set-key "\C-c\C-ht" 'help-pvs-theory-attachments)

(setq auto-mode-alist
      (cons '("pvs-attachments\\'" . lisp-mode)
	    auto-mode-alist))

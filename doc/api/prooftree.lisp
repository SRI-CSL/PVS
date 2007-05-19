(in-package :pvs)

(pvs-init)
(setq *pvs-context-path* (shortpath (working-directory)))

(defun pvs-wish-source (file)
  (format t "~%PT:~a" file))

(prove-file-at "prelude" nil 4820 t "prelude" "prelude.pvs" 0 nil t)

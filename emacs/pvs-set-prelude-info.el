;;; This is used to create pvs-prelude-files-and-regions.el, which is just
;;; defines *pvs-files-and-regions* so that extracting prelude theories
;;; (e.g., using M-x vpt) is reasonably quick.

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

(defvar start-pvs nil) ; don't need to run pvs, just load the files

(if (getenv "PVSPATH")
    (defconst pvs-path (getenv "PVSPATH"))
    (error "PVSPATH must be set"))

(setq load-path
  (append (list pvs-path
		(concat pvs-path "/emacs")
		(concat pvs-path "/emacs/ilisp"))
          load-path))

(defvar region-file
  "emacs/pvs-prelude-files-and-regions.el")

(defun set-prelude-files-and-regions ()
  ;; (pvs-message "Creating pvs-prelude-files-and-regions.el")
  (let* ((files (directory-files (concat pvs-path "/lib")
				 t "^prelude\\.pvs$\\|^pvsio_prelude\\.pvs$\\|.*_adt\\.pvs$"))
	 (files-and-regions
	  (mapcar #'(lambda (file)
		      (save-excursion
			(let ((noninteractive t)) ;; Shut up about read-only
			  (set-buffer (find-file-noselect file)))
			(cons (file-name-nondirectory file) (theory-regions*))))
	    files)))
    (set-buffer (find-file-noselect (concat pvs-path "/" region-file)))
    (erase-buffer)
    (insert ";;; Generated from pvs-set-prelude-info.el - do not edit
(defvar *prelude-files-and-regions*
  (mapcar
      #'(lambda (x)
	 (cons (format \"%s/lib/%s\" pvs-path (car x)) (cdr x)))
    '")
    (insert (format "%S" files-and-regions))
    (insert "))")
    (write-file (buffer-file-name))))

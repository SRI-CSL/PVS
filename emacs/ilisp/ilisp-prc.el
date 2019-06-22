;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-prc.el --
;;; ILISP process handling.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(require 'cl)

(defun ilisp-process ()
  "Return the current ILISP process."
  (get-buffer-process (ilisp-buffer)))


(defvar ilisp-buffer-function 'ilisp-recent-buffer
  "A function of no arguments which returns the current ilisp buffer")


;;;%Buffer and process selection
(defun ilisp-buffer ()
  "Return the current ILISP buffer.  This is the buffer to whose process requests are sent."
  (if (memq major-mode ilisp-modes)
      (current-buffer)
    (let ((buffer (funcall ilisp-buffer-function)))
      (or buffer
	  (error "The PVS process has died.  Please start a new one with M-x pvs")))))


(defun ilisp-recent-buffer ()
  "Return the most-recently selected ilisp buffer." 
  (if ilisp-buffer 
      (or (get-buffer ilisp-buffer)
	  (get-buffer
	   (setq ilisp-buffers
		 (delete* (substring ilisp-buffer
				     1 
				     (1- (length ilisp-buffer)))
			  ilisp-buffers 
			  :test (function (lambda (s1 s2)
					    (string= s1 (car s2)))))
		 ilisp-buffer
		 (format "*%s*" (car (car ilisp-buffers))))))))


;;;
(defun select-ilisp ()
  "Select the current ILISP buffer."
  (interactive)
  (let ((new (completing-read
	      (if ilisp-buffer
		  (format "Buffer [%s]: "
			  (substring ilisp-buffer 1
				     (1- (length ilisp-buffer))))
		  "Buffer: ")
	      ilisp-buffers nil t)))
    (if (not (zerop (length new)))
	(setq ilisp-buffer (format "*%s*" new)))))

;;; end of file -- ilisp-prc.el --

;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-prc.el --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.



;;;
;;; ILISP process handling
;;;
;;;
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
	  (error "You must start an inferior LISP with run-ilisp.")))))


(defun ilisp-recent-buffer ()
  "Return the most-recently selected ilisp buffer." 
  (if ilisp-buffer 
      (or (get-buffer ilisp-buffer)
	  (get-buffer
	   (setq ilisp-buffers
		 (lisp-del (substring ilisp-buffer 1 
				      (1- (length ilisp-buffer)))
			   ilisp-buffers 
			   (function (lambda (s1 s2)
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

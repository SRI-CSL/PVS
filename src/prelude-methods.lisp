;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prelude-methods.lisp -- Methods that must be loaded after the prelude
;;                         is processed. 
;; Author          : Sam Owre
;; Created On      : Fri Oct 30 11:36:44 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 11:37:49 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(eval-when (load eval)
  (defmethod store-object* ((obj (eql *boolean*)))
    (reserve-space 1
      (push-word (store-obj '*boolean*))))

  (setf (get '*boolean* 'fetcher) '(lambda () *boolean*))

  (defmethod store-object* ((obj (eql *number*)))
    (reserve-space 1
      (push-word (store-obj '*number*))))

  (setf (get '*number* 'fetcher) '(lambda () *number*))

  (defmethod store-object* ((obj (eql *real*)))
    (reserve-space 1
      (push-word (store-obj '*real*))))

  (setf (get '*real* 'fetcher) '(lambda () *real*))

  (defmethod store-object* ((obj (eql *rational*)))
    (reserve-space 1
      (push-word (store-obj '*rational*))))

  (setf (get '*rational* 'fetcher) '(lambda () *rational*))

  (defmethod store-object* ((obj (eql *integer*)))
    (reserve-space 1
      (push-word (store-obj '*integer*))))

  (setf (get '*integer* 'fetcher) '(lambda () *integer*))

  (defmethod store-object* ((obj (eql *naturalnumber*)))
    (reserve-space 1
      (push-word (store-obj '*naturalnumber*))))

  (setf (get '*naturalnumber* 'fetcher) '(lambda () *naturalnumber*))

  (defmethod store-object* ((obj (eql *ordinal*)))
    (reserve-space 1
      (push-word (store-obj '*ordinal*))))

  (setf (get '*ordinal* 'fetcher) '(lambda () *ordinal*))
  )

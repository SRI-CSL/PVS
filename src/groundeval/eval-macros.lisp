
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

(in-package :pvs)

(defvar *eval-array-bound* 10000)    ;shouldn't be used
(defvar *eval-verbose* nil)          ;turn on for verbose translation trace

;; helpful macros
(defmacro in-info (decl)
  `(internal (eval-info ,decl)))

(defmacro ex-info (decl)
  `(external (eval-info ,decl)))

(defmacro in-defn (decl)
  `(unary (in-info ,decl)))

(defmacro in-defn-m (decl)
  `(multiary (in-info ,decl)))

(defmacro in-defn-d (decl)
  `(destructive (in-info ,decl)))

(defmacro ex-defn (decl)
  `(unary (ex-info ,decl)))

(defmacro ex-defn-m (decl)
  `(multiary (ex-info ,decl)))

(defmacro ex-defn-d (decl)
  `(destructive (ex-info ,decl)))

(defmacro in-name (decl)
  `(name (in-defn ,decl)))

(defmacro in-name-m (decl)
  `(name (in-defn-m ,decl)))

(defmacro in-name-d (decl)
  `(name (in-defn-d ,decl)))

(defmacro ex-name (decl)
  `(name (ex-defn ,decl)))

(defmacro ex-name-m (decl)
  `(name (ex-defn-m ,decl)))

(defmacro ex-name-d (decl)
  `(name (ex-defn-d ,decl)))



;;unary primitives
(defun |pvs_=| (x) (pvs_equalp (svref x 0)(svref x 1)))
(defun |pvs_/=| (x)(not (pvs_equalp (svref x 0)(svref x 1))))
(defun |pvs_IMPLIES| (x) (or (not (svref x 0)) (svref x 1)))
(defun |pvs_WHEN| (x) (or (svref x 0)(not (svref x 1))))
(defun |pvs_=>| (x) (|pvs_IMPLIES| x))
(defun |pvs_IFF| (x) (equal (svref x 0)(svref x 1)))
(defun |pvs_<=>| (x) (equal (svref x 0)(svref x 1)))
(defun |pvs_AND| (x)(and (svref x 0)(svref x 1)))
(defun |pvs_&| (x)(and (svref x 0)(svref x 1)))
(defun |pvs_OR| (x) (or (svref x 0)(svref x 1)))
(defun |pvs_NOT| (x) (not x))
(defun |pvs_+| (x) (+ (svref x 0)(svref x 1)))
(defun |pvs_-| (x) (- x))  ;;temporary (svref x 0)(svref x 1)))
(defun |pvs_--| (x) (- (svref x 0)(svref x 1)))
(defun |pvs_*| (x) (* (svref x 0)(svref x 1)))
(defun |pvs_/| (x) (/ (svref x 0)(svref x 1)))
(defun |pvs_<| (x) (< (svref x 0)(svref x 1)))
(defun |pvs_<=| (x) (<= (svref x 0)(svref x 1)))
(defun |pvs_>| (x) (> (svref x 0)(svref x 1)))
(defun |pvs_>=| (x) (>= (svref x 0)(svref x 1)))
(defun |pvs_floor| (x) (nth-value 0 (floor x)))
(defun |pvs_ceiling| (x) (nth-value 0 (ceiling x)))
(defun |pvs_rem| (x) #'(lambda (y) (nth-value 0 (rem y x))))
(defmacro |pvs_ndiv| (x)
  (let ((xx (gentemp))
	(yy (gentemp)))
    `(let ((,xx ,(svref x 0))
	   (,yy ,(svref x 1)))
       (if (eq (< ,xx 0)(< ,yy 0))
	   (nth-value 0 (floor ,xx ,yy))
	   (nth-value 0 (ceiling ,xx ,yy))))))
(defun |pvs_even?| (x) (evenp x))
(defun |pvs_odd?| (x) (oddp x))
(defun |pvs_cons| (x) (cons (svref x 0)(svref x 1)))
(defun |pvs_car| (x) (car x))
(defun |pvs_cdr| (x) (cdr x))
(defun |pvs_cons?| (x) (consp x))
(defun |pvs_null?| (x) (null x))
(defun |pvs_restrict| (f) f)
(defun |pvs_length| (x) (length x))   ;; should this be list-length?
(defun |pvs_member| (x) (not (null (member (svref x 0) (svref x 1) :test #'pvs_equalp))))
(defun |pvs_nth| (x) (nth (svref x 1) (svref x 0)))
(defun |pvs_append| (x) (append (svref x 0) (svref x 1)))
(defun |pvs_reverse| (x) (reverse x))
(defun |pvs_number_field_pred| (x) (numberp x))
(defun |pvs_real_pred| (x) (realp x))
(defun |pvs_rational_pred| (x) (rationalp x))
(defun |pvs_integer_pred| (x) (integerp x))
(defun |pvs_integer?| (i) (integerp i))

;;multiary macro versions of primitives
(defmacro |pvs__=| (x y)
  `(pvs_equalp ,x ,y))
(defmacro |pvs__/=| (x y) `(not (pvs_equalp ,x ,y)))

(defmacro |pvs__member| (x y) `(not (null (member ,x ,y :test #'pvs_equalp))))

(defun pvs_equalp (x y)
  "From CMULisp's equalp definition - adds pvs-array-closure-p test"
  (cond ((eq x y) t)
        ((characterp x) (and (characterp y) (char-equal x y)))
        ((numberp x) (and (numberp y) (= x y)))
        ((consp x)
         (and (consp y)
              (pvs_equalp (car x) (car y))
              (pvs_equalp (cdr x) (cdr y))))
        ((hash-table-p x)
         (and (hash-table-p y)
              (eql (hash-table-count x) (hash-table-count y))
              (eql (hash-table-test x) (hash-table-test y))
              (with-hash-table-iterator (next x)
                (loop
                 (multiple-value-bind (more x-key x-value)
                     (next)
                   (cond (more
                          (multiple-value-bind (y-value foundp)
                              (gethash x-key y)
                            (unless (and foundp (pvs_equalp x-value y-value))
                              (return nil))))
                         (t
                          (return t))))))))
	((pvs-array-closure-p x)
	 (and (pvs-array-closure-p y)
	      (let ((x-size (pvs-array-closure-size x))
		    (y-size (pvs-array-closure-size y)))
		(= x-size y-size)
		(pvs_equalp (mk-fun-array (pvs-array-closure-closure x) x-size)
			    (mk-fun-array (pvs-array-closure-closure y) y-size)))))
        ((vectorp x)
         (let ((length (length x)))
           (and (vectorp y)
                (= length (length y))
                (dotimes (i length t)
                  (let ((x-el (aref x i))
                        (y-el (aref y i)))
                    (unless (or (eq x-el y-el)
                                (pvs_equalp x-el y-el))
                      (return nil)))))))
        ((arrayp x)
         (and (arrayp y)
              (= (array-rank x) (array-rank y))
              (dotimes (axis (array-rank x) t)
                (unless (= (array-dimension x axis)
                           (array-dimension y axis))
                  (return nil)))
              (dotimes (index (array-total-size x) t)
                (let ((x-el (row-major-aref x index))
                      (y-el (row-major-aref y index)))
                  (unless (or (eq x-el y-el)
                              (pvs_equalp x-el y-el))
                    (return nil))))))
        (t nil)))

(defmacro |pvs__IMPLIES| (x y) `(or (not ,x) ,y))
(defmacro |pvs__WHEN| (x y) `(or (not ,y) ,x))
(defmacro |pvs__=>| (x y) `(or (not ,x) ,y))
(defmacro |pvs__IFF| (x y) `(equal ,x ,y))
(defmacro |pvs__<=>| (x y) `(equal ,x ,y))
(defmacro |pvs__AND| (x y) `(and ,x ,y))
(defmacro |pvs__&| (x y) `(and ,x ,y))
(defmacro |pvs__OR| (x y)  `(or ,x ,y))
(defmacro |pvs__+| (x y) `(+ ,x ,y))
(defmacro |pvs__-| (x y) `(- ,x ,y))
(defmacro |pvs__*| (x y) `(* ,x ,y))
(defmacro |pvs__/| (x y) `(/ ,x ,y))
(defmacro |pvs__<| (x y) `(< ,x ,y))
(defmacro |pvs__<=| (x y) `(<= ,x ,y))
(defmacro |pvs__>| (x y) `(> ,x ,y))
(defmacro |pvs__>=| (x y) `(>= ,x ,y))
(defmacro |pvs__floor| (x) `(floor ,x))
(defmacro |pvs__ceiling| (x) `(ceiling ,x))
(defmacro |pvs__rem| (y)
  (let ((xx (gentemp))
	(yy (gentemp)))
    `(function
      (lambda (x)
	(let ((,xx (the integer x))
	      (,yy (the integer ,y)))
       (if (< ,xx ,yy) ,xx (rem ,xx ,yy)))))))
(defmacro |pvs__ndiv| (x y)
  (let ((xx (gentemp))
	(yy (gentemp)))
    `(let ((,xx ,x)
	   (,yy ,y))
       (if (eq (< ,xx 0)(< ,yy 0))
	   (floor ,xx ,yy)
	   (ceiling ,xx ,yy)))))
(defmacro |pvs__even?| (x) `(evenp ,x))
(defmacro |pvs__odd?| (x) `(oddp ,x))
(defmacro |pvs__cons| (x y) `(cons ,x ,(the list y)))

(defmacro |pvs__nth| (x y) `(nth ,y ,x))
(defmacro |pvs__append| (x y) `(append ,x ,y))
(defmacro |pvs__number_field_pred| (x) `(numberp ,x))
(defmacro |pvs__real_pred| (x) `(realp ,x))
(defmacro |pvs__rational_pred| (x) `(rationalp ,x))
(defmacro |pvs__integer_pred| (x) `(integerp ,x))


(defmacro project (index tuple)
  (let ((ind (1- index)))
    `(let ((val (svref ,tuple ,ind)))
       (if (eq val 'undefined)(undefined nil) val)   ;; what can we do here?
	 )))

(defmacro pvs-funcall (fun &rest args)
  `(let ((funval ,fun))
     (if (arrayp funval)
	 (svref funval ,@args)
	 (if (pvs-outer-array-p funval)
	     (pvs-outer-array-lookup funval ,@args) 
	     (if (pvs-array-closure-p funval)
		 (funcall (pvs-array-closure-closure funval) ,@args)
		 (if (pvs-closure-hash-p funval)
		     (pvs-closure-hash-lookup funval ,args)
		     (funcall funval ,@args)))))))

(defmacro trap-undefined (expr)
  `(catch 'undefined ,expr))

(defstruct pvs-array-closure
  size closure)

(defmacro mk-pvs-array-closure (size closure)
  `(make-pvs-array-closure :size ,size
			  :closure ,closure))

(defmacro pvs-array-closure-lookup (array index)
  `(funcall (pvs-array-closure-closure ,array) ,index))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pvs-closure-hash is used to destructively evaluate non-array
;;functions.  It consists of a pair of a hash-table and a closure.
;;All updates are applied to the hash-table, and lookup goes through
;;the hash-table first and then the closure. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct pvs-closure-hash
  hash closure)

(defmacro mk-pvs-closure-hash (hash closure)
  `(make-pvs-closure-hash :hash ,hash
			  :closure ,closure))

(defmacro pvs-closure-hash-lookup (function argument)
  `(let ((funval ,function)
	 (argval ,argument))
     (multiple-value-bind (val found)
	 (gethash argval (pvs-closure-hash-hash funval))
       (if found val
	   (apply funval argval)))))

(defmacro pvs-function-update (function argument value)
    `(let ((funval ,function)
	   (argval ,argument)
	   (val ,value))
       (if (pvs-closure-hash-p funval)
	   (setf (gethash argval (pvs-closure-hash-hash funval))
		 val)
	   (let ((hash (make-hash-table :test #'pvs_equalp)))
	     (setf (gethash argval hash) val)
	     (mk-pvs-closure-hash hash funval)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct pvs-array
  contents diffs size)

(defstruct pvs-outer-array
  inner-array offset diffs size)

(defun insert-array-diffs (diffs array)
  (insert-array-diffs* diffs array))

(defun insert-array-diffs* (diffs array)  
      (if (consp diffs)
	  (let ((array (insert-array-diffs* (cdr diffs) array)))
	    (setf (svref array (caar diffs))(cdar diffs))
	    array)
	  array))

(defun copy-pvs-array! (array arraysize)
  (cond ((pvs-array-p array) (copy-pvs-array array))
	((simple-vector-p array)
	 (let ((arr (make-array arraysize :initial-element 0)))
	   (loop for i from 0 to (1- arraysize) do
		 (setf (svref arr i)(svref array i)))
	   (make-pvs-array :contents arr :size 0)))
	(t (make-pvs-array
	    :contents (mk-fun-array array arraysize)
	    :size 0))))

(defun mk-pvs-array! (array arraysize)
  (cond ((and (pvs-array-p array) (zerop (pvs-array-size array)))
	 array)
	(t (make-pvs-array
	    :contents (mk-fun-array array arraysize)
	    :size 0))))

(defun copy-pvs-outer-array! (array arraysize)
  (cond ((pvs-outer-array-p array) (copy-pvs-outer-array array))
	((simple-vector-p array)
	 (let ((arr (make-array arraysize :initial-element 0)))
	   (loop for i from 0 to (1- arraysize) do
		 (setf (svref arr i)(svref array i)))
	   (make-pvs-outer-array
	    :inner-array (make-pvs-array :contents arr :size 0)
	    :offset 0
	    :size 0)))
	(t (make-pvs-outer-array
	    :inner-array (make-pvs-array
			  :contents (mk-fun-array array arraysize)
			  :size 0)
	    :offset 0
	    :size 0))))

(defun pvs-array-update (pvs-array at with arraysize)
  (let* ((x (copy-pvs-array! pvs-array arraysize))
	 (diffs (pvs-array-diffs x))
	 (contents (pvs-array-contents x)))
    (cond ((> (pvs-array-size x)
	      (sqrt (array-total-size contents)))
	   (let ((newarray (copy-seq contents)))
	     (setf (pvs-array-contents x)
		   (insert-array-diffs (cons (cons at with)
					     diffs)
				       newarray)
		   (pvs-array-diffs x) nil
		   (pvs-array-size x) 0)
	     x))
	  ((consp diffs)
	   (push (cons at with) (pvs-array-diffs x))
	   (incf (pvs-array-size x))
	   x)
	  (t (when (pvs-array-p pvs-array)
	       (push (cons at (svref contents at))
		     (pvs-array-diffs pvs-array))
	       (incf (pvs-array-size pvs-array)))
	     (setf (svref (pvs-array-contents x) at) with)
	     x))))

(defun size-limit (x) (sqrt x))

(defun pvs-outer-array-update (pvs-outer-array at with arraysize)
  (let* ((x (copy-pvs-outer-array! pvs-outer-array arraysize))
	 (diffs (pvs-outer-array-diffs x))
	 (offset (pvs-outer-array-offset x))
	 (outer-size (pvs-outer-array-size x))
	 (inner-array (pvs-outer-array-inner-array x))
	 (inner-size (pvs-array-size inner-array))
	 (inner-diffs (pvs-array-diffs inner-array))
	 (contents (pvs-array-contents inner-array))
	 (oldval (svref contents at))
	 (main? (eql offset inner-size)))
    (cond (main?
	   (assert (not diffs))
	   (push (cons at oldval) (pvs-array-diffs inner-array))
	   (incf (pvs-array-size inner-array))
	   (setf (svref contents at) with
		 (pvs-outer-array-offset x)
		 (pvs-array-size inner-array))
	   x)
	  (t
	    (push (cons at with) (pvs-outer-array-diffs x))
	    (incf (pvs-outer-array-size x))
	    (when (> (+ inner-size
			outer-size)
		     (size-limit (array-total-size contents)))
	      (let ((newarray (copy-seq contents)))
		(loop for (x . y) in inner-diffs ;restore inner values
		      as i from (1+ offset) to inner-size
		      do (setf (svref newarray x) y))
		(setf (pvs-outer-array-inner-array pvs-outer-array)
		      (make-pvs-array ;;restore outer diffs
			:contents 
			(insert-array-diffs
			 diffs
			 newarray)
		      :size 0)
		      (pvs-outer-array-diffs pvs-outer-array) nil
		      (pvs-outer-array-offset pvs-outer-array) 0
		      (pvs-outer-array-size pvs-outer-array)  0)))
	    x))))

(defun assoc-last (index alist count)
  (if (and (consp alist)(> count 0))
      (let ((rest-assoc (assoc-last index (cdr alist) (1- count))))
	(if (null rest-assoc)
	    (if (eql index (caar alist))
		(car alist)
		nil)
	    rest-assoc))
      nil))

;;The nondestructive array data structure works as follows: pvs-outer-array
;;contains an alist (misleadingly called diffs), an offset, and an
;;inner-array, and the latter contains a size, diffs alist, and a
;;the contents array.  In the primary branch, the outer-array is empty,
;;the inner-array contents are looked up.  Otherwise, you look up the 
;;outer diffs.  If it doesn't appear in the outer diffs, then you look
;;at the earliest displaced value following the offset.  Since the offset
;;essentially timestamps the inner-diffs, this would be the value in the
;;array when the outer array was activated.  If none of these diffs
;;contains the entry, then the contents array has the valid value.
;;The inner array is shared by the different references but not the
;;outer one.  The outer offset and inner size match on the main branch,
;;but diverge on the secondary ones.  When there are enough accumulated
;;diffs, a new array with a fresh inner array is constructed by
;;restoring old values in the inner diffs and carrying out the updates
;;in the outer diffs.  This structure now becomes the main branch.   

(defun pvs-outer-array-lookup (outer-array index)
  (let* ((arr outer-array)
	  (ind index)
	  (inner-array (pvs-outer-array-inner-array arr)))
     (or (and (null (pvs-outer-array-diffs arr))
	      (null (pvs-array-diffs inner-array))
	      (svref (pvs-array-contents inner-array) ind))
	 (let ((lookup-diffs (assoc ind (pvs-outer-array-diffs arr))))
	   (and lookup-diffs
		(cdr lookup-diffs)))
	 (let ((lookup-inner-diffs
		(assoc-last ind (pvs-array-diffs
				inner-array)
			    (- (pvs-array-size inner-array)
			       (pvs-outer-array-offset arr)))))
	   (when lookup-inner-diffs (cdr lookup-inner-diffs)))
	 (svref (pvs-array-contents inner-array) ind))))

(defmacro pvs-array-lookup (pvs-array val)
  `(let ((arr ,pvs-array)
	 (ind ,val))
     (let ((lookup-diffs (assoc ind (pvs-array-diffs arr))))
    (or (and lookup-diffs
	     (cdr lookup-diffs))
	(svref (pvs-array-contents arr) ind)))))

(defmacro pvs2cl_tuple (&rest args)
  (let ((protected-args (loop for x in args collect `(trap-undefined ,x))))
    `(vector ,@protected-args)))

(defmacro pvs2cl_record (&rest args)
  (let ((protected-args (loop for x in args collect `(trap-undefined ,x))))
    `(vector ,@protected-args)))

(defmacro nd-rec-tup-update (rec fieldnum newval)
  `(let ((val ,newval)
	(newrec  (copy-seq ,rec)))
    (setf (svref newrec ,fieldnum) val)
    newrec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Defining Lisp representations for the types so that we can then
;;pass type parameters in the Lisp code.  These type parameters are
;;used for enumeration and destructive updates.  The main type structures
;;we need are:  subrange, scalar, subtype, and tuple/record.

(defstruct pvs-lisp-subrange  ;;represents [low, high)
  low high)

(defstruct pvs-lisp-scalar
  constructors)

(defstruct pvs-lisp-subtype
  supertype predicate)

(defstruct pvs-lisp-tuple
  elemtypes)

(defstruct pvs-lisp-array
  bound offset rangetype)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *pvs-lisp-types-alist*
  (list (cons *boolean* 'boolean)
	(cons *naturalnumber*  'natural)
	(cons *integer* 'integer)
	(cons *rational* 'rational)
	(cons *real* 'real)
	(cons *number* 'number)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;converts PVS types into a Lisp representation that can be used
;;by late-binding functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod pvs-lisp-type ((type type-name) bindings)
  (if (tc-eq type *number*)
      'number
      (when (formal-type-decl? (declaration type)) ;;must be in bindings
	(cdr (assoc (declaration type) bindings :key #'declaration)))))

(defun get-pvs-lisp-subrange (type bindings)
  (let ((range (subrange? type)))
    (if range
	(make-pvs-lisp-subrange
	 :low (pvs2cl_up* (car range) bindings nil)
	 :high `(1+ ,(pvs2cl_up* (cdr range) bindings nil)))
	(let ((below (below? type)))
	  (if below
	      (make-pvs-lisp-subrange
	       :low 0
	       :high (pvs2cl_up* below bindings nil))
	      (let ((upto (upto? type)))
		(if upto
		    (make-pvs-lisp-subrange
		     :low 0
		     :high (1+ (pvs2cl_up* upto bindings nil)))
		    nil)))))))
					    

(defmethod pvs-lisp-type ((type subtype) bindings)
  (with-slots (supertype predicate) type
    (let ((bind (assoc type *pvs-lisp-types-alist*
			:test #'tc-eq)))
      (if bind
	  (cdr bind)
	  (let ((subrange (get-pvs-lisp-subrange type bindings)))
	    (or subrange
		(make-pvs-lisp-subtype
		 :supertype (pvs-lisp-type supertype bindings)
		 :predicate (pvs2cl_up* predicate bindings nil)))))))) ;;livevars?

(defmethod pvs-lisp-type ((type enumtype) bindings)
  (declare (ignore bindings))
  (with-slots (constructors) type
    (make-pvs-lisp-scalar
     :constructors (pvs2cl-constructors constructors (adt type)))))

(defmethod pvs-lisp-type ((type tupletype) bindings)
  (make-pvs-lisp-type 
   :elemtypes (pvs-lisp-type (types type) bindings)))

(defmethod pvs-lisp-type ((type recordtype) bindings)
  (make-pvs-lisp-tuple 
   :elemtypes (pvs-lisp-type (fields type) bindings)))


(defmethod pvs-lisp-type ((type list) bindings)
  (cond ((null type) nil)
	(t (if (binding? (car type))
	       (let* ((ty1 (car type))
		      (car-binding (if (rassoc ty1 bindings)
				       (pvs2cl-newid (id ty1) bindings)
				       (lisp-id (id ty1)))))
		 (cons (pvs-lisp-type ty1 bindings)
		       (pvs-lisp-type (cdr type)
				      (acons ty1
					     car-binding
					     bindings))))
	       (cons (pvs-lisp-type (car type) bindings)
		     (pvs-lisp-type (cdr type) bindings))))))

(defmethod pvs-lisp-type ((type field-decl) bindings)
  (pvs-lisp-type (type type) bindings))

(defmethod pvs-lisp-type ((type dep-binding) bindings)
  (pvs-lisp-type (type type) bindings))

(defmethod pvs-lisp-type ((type funtype) bindings)
  (with-slots (domain range) type
    (let ((subrange (get-pvs-lisp-subrange (if (binding? domain)
					       (type domain)
					       domain)
					   bindings)))
      (if subrange
	  (make-pvs-lisp-array
	   :size `(- ,(pvs-lisp-subrange-low subrange)
		     ,(pvs-lisp-subrange-high subrange))
	   :offset (pvs-lisp-subrange-low subrange)
	   :range (if (binding? domain)
		      (pvs-lisp-type range (acons domain
						  (pvs2cl-newid (id domain)
								bindings)
						  bindings))
		      (pvs-lisp-type range bindings)))
	  nil))))

(defmethod pvs-lisp-type ((type t) bindings)
  (declare (ignore bindings))
  nil)


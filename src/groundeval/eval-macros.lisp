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
(defun pvs_= (x) (equalp (svref x 0)(svref x 1)))
(defun pvs_/= (x)(not (equalp (svref x 0)(svref x 1))))
(defun pvs_IMPLIES (x) (or (not (svref x 0)) (svref x 1)))
(defun pvs_WHEN (x) (or (svref x 0)(not (svref x 1))))
(defun pvs_=> (x) (pvs_IMPLIES x))
(defun pvs_IFF (x) (equal (svref x 0)(svref x 1)))
(defun pvs_<=> (x) (equal (svref x 0)(svref x 1)))
(defun pvs_AND (x)(and (svref x 0)(svref x 1)))
(defun pvs_& (x)(and (svref x 0)(svref x 1)))
(defun pvs_OR (x) (or (svref x 0)(svref x 1)))
(defun pvs_NOT (x) (not x))
(defun pvs_+ (x) (+ (svref x 0)(svref x 1)))
(defun pvs_- (x) (- x))  ;;temporary (svref x 0)(svref x 1)))
(defun pvs_-- (x) (- (svref x 0)(svref x 1)))
(defun pvs_* (x) (* (svref x 0)(svref x 1)))
(defun pvs_/ (x) (/ (svref x 0)(svref x 1)))
(defun pvs_< (x) (< (svref x 0)(svref x 1)))
(defun pvs_<= (x) (<= (svref x 0)(svref x 1)))
(defun pvs_> (x) (> (svref x 0)(svref x 1)))
(defun pvs_>= (x) (>= (svref x 0)(svref x 1)))
(defun pvs_floor (x) (nth-value 0 (floor x)))
(defun pvs_ceiling (x) (nth-value 0 (ceiling x)))
(defun pvs_rem (x) #'(lambda (y) (nth-value 0 (rem y x))))
(defmacro pvs_ndiv (x)
  (let ((xx (gentemp))
	(yy (gentemp)))
    `(let ((,xx ,(svref x 0))
	   (,yy ,(svref x 1)))
       (if (eq (< ,xx 0)(< ,yy 0))
	   (nth-value 0 (floor ,xx ,yy))
	   (nth-value 0 (ceiling ,xx ,yy))))))
(defun pvs_even? (x) (evenp x))
(defun pvs_odd? (x) (oddp x))
(defun pvs_cons (x) (cons (svref x 0)(svref x 1)))
(defun pvs_car (x) (car x))
(defun pvs_cdr (x) (cdr x))
(defun pvs_cons? (x) (consp x))
(defun pvs_null? (x) (null x))
(defun pvs_restrict (f) f)
(defun pvs_length (x) (length x))   ;; should this be list-length?
(defun pvs_member (x) (not (null (member (svref x 0) (svref x 1) :test #'equalp))))
(defun pvs_nth (x) (nth (svref x 1) (svref x 0)))
(defun pvs_append (x) (append (svref x 0) (svref x 1)))
(defun pvs_reverse (x) (reverse x))
(defun pvs_number_field_pred (x) (numberp x))
(defun pvs_real_pred (x) (realp x))
(defun pvs_rational_pred (x) (rationalp x))
(defun pvs_integer_pred (x) (integerp x))
(defun pvs_integer?(i) (integerp i))

;;multiary macro versions of primitives
(defmacro pvs__= (x y)
  (let ((xx (gentemp)))
    `(let ((,xx ,x))
       (if (stringp ,xx) (string= ,xx ,y) (equalp ,xx ,y)))))

(defun pvs_equalp (x y)
  (cond ((symbolp x)
	      (eq x y))
	     ((numberp x) (= x y))
	     (t (equalp x y))))
(defmacro pvs__/= (x y) `(not (equalp ,x ,y)))
(defmacro pvs__IMPLIES (x y) `(or (not ,x) ,y))
(defmacro pvs__WHEN (x y) `(or (not ,y) ,x))
(defmacro pvs__=> (x y) `(or (not ,x) ,y))
(defmacro pvs__IFF (x y) `(equal ,x ,y))
(defmacro pvs__<=> (x y) `(equal ,x ,y))
(defmacro pvs__AND (x y) `(and ,x ,y))
(defmacro pvs__& (x y) `(and ,x ,y))
(defmacro pvs__OR (x y)  `(or ,x ,y))
(defmacro pvs__+ (x y) `(+ ,x ,y))
(defmacro pvs__- (x y) `(- ,x ,y))
(defmacro pvs__* (x y) `(* ,x ,y))
(defmacro pvs__/ (x y) `(/ ,x ,y))
(defmacro pvs__< (x y) `(< ,x ,y))
(defmacro pvs__<= (x y) `(<= ,x ,y))
(defmacro pvs__> (x y) `(> ,x ,y))
(defmacro pvs__>= (x y) `(>= ,x ,y))
(defmacro pvs__floor (x) `(floor ,x))
(defmacro pvs__ceiling (x) `(ceiling ,x))
(defmacro pvs__rem (y)
  (let ((xx (gentemp))
	(yy (gentemp)))
    `(function
      (lambda (x)
	(let ((,xx (the integer x))
	      (,yy (the integer ,y)))
       (if (< ,xx ,yy) ,xx (rem ,xx ,yy)))))))
(defmacro pvs__ndiv (x y)
  (let ((xx (gentemp))
	(yy (gentemp)))
    `(let ((,xx ,x)
	   (,yy ,y))
       (if (eq (< ,xx 0)(< ,yy 0))
	   (floor ,xx ,yy)
	   (ceiling ,xx ,yy)))))
(defmacro pvs__even? (x) `(evenp ,x))
(defmacro pvs__odd? (x) `(oddp ,x))
(defmacro pvs__cons (x y) `(cons ,x ,(the list y)))

(defmacro pvs__member (x y) `(not (null (member ,x ,y :test #'equalp))))
(defmacro pvs__nth (x y) `(nth ,y ,x))
(defmacro pvs__append (x y) `(append ,x ,y))
(defmacro pvs__number_field_pred (x) `(numberp ,x))
(defmacro pvs__real_pred (x) `(realp ,x))
(defmacro pvs__rational_pred (x) `(rationalp ,x))
(defmacro pvs__integer_pred (x) `(integerp ,x))


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
	     (funcall funval ,@args)))))

(defmacro trap-undefined (expr)
  `(catch 'undefined ,expr))

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


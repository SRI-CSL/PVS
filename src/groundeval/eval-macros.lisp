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
(defun pvs_floor (x) (floor x))
(defun pvs_ceiling (x) (ceiling x))
(defun pvs_rem (x) #'(lambda (y)(rem y x))) ;;(svref x 0)(svref x 1)))
(defun pvs_ndiv (x) (let ((z (/ (svref x 0)(svref x 1))))
		     (if (< z 0)(ceiling z)(floor z))))
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


;;multiary macro versions of primitives
(defmacro pvs__= (x y)
  `(equalp ,x ,y))

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
(defmacro pvs__cons (x y) `(cons ,x ,(the list y)))

(defmacro pvs__member (x y) `(not (null (member ,x ,y :test #'equalp))))
(defmacro pvs__nth (x y) `(nth ,y ,x))
(defmacro pvs__append (x y) `(append ,x ,y))

(defmacro project (index tuple)
  (let ((ind (1- index)))
    `(let ((val (svref ,tuple ,ind)))
       (if (eq val 'undefined)(undefined nil) val)   ;; what can we do here?
	 )))

;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-

; Q - rational arithmetic package for decision procedure
;
; 21-Jul-89 - FvH - modified qzerop to handle nil argument

;(defmacro-whole den (x) (list 'caddr (cadr x)))
;(defmacro-whole num (x) (list 'cadr (cadr x)))	

(defmacro den (x) `(caddr ,x))
(defmacro num (x) `(cadr ,x))

(defun qabs (x)
 (cond
  ((integerp x) (abs x))			
  ((minusp (num x))(qmake (- (num x)) (den x)))	
  (t x)))

(defun qminusp (x)
  (cond 
   ((integerp x) (minusp x))			
   (t (minusp (num x)))))

(defun qminus (x) 
  (cond 
   ((integerp x) (- x))				
   (t (list 'QUO (- (cadr x)) (caddr x)))))	

(defun qeqp (x y)
  (cond
   ((integerp x) 				
    (cond
      ((integerp y) (= x y))			
      (t (= (* x (den y)) (num y)))))
   ((integerp y) (= (* y (den x)) (num x)))	
   (t (= (* (num x) (den y)) (* (num y) (den x))))))

(defun qgreaterp(x y)
  (cond
   ((integerp x)				
    (cond
     ((integerp y)(> x y))			
     (t (> (* x (den y))(num y)))))
   ((integerp y)(> (num x)(* y (den x))))	
   (t (> (* (num x)(den y))(* (num y)(den x))))))

(defun qlessp (x y)
  (cond
   ((integerp x)
    (cond
     ((integerp y)(< x y))
     (t (< (* x (den y))(num y)))))
   ((integerp y)(< (num x)(* y (den x))))
   (t (< (* (num x)(den y))(* (num y)(den x))))))


(defun qlesseqp (x y)
  (not (qgreaterp x y)))

(defun qgreatereqp (x y)
  (not (qlessp x y)))

(defun qmake (n d) (list 'QUO n d))

(defun qneqp (x y) (not (qeqp x y)))

(defun qnumberp(x)
  (or 
   (integerp x)					
   (and
    (consp x)					
    (eq (car x) 'QUO))))


(defun qzerop (x)
  (cond
   ((null x) nil)           ;; 21-Jul-89 - FvH - This case added because
                            ;; function is applied in timesfractpt to
                            ;; result of call to fractpt, which may be nil
   ((integerp x)(zerop x))			
   (t (zerop (num x)))))


(defun qnorm (x)
  (cond
   ((integerp x) x)				
   (t
    (prog(num den gcd)
      (setq num (num x) den (den x))
      (cond
       ((zerop den)
	(error "Division by zero: (~S)" x))
       ((zerop num)
	(return num))
       ((< den 0)
	(setq den (- den) num (- num))))	
      (setq gcd (gcd num den))			
      (cond
       ((= den gcd)(return (divide$ num den)))	
       (t (return
	    (qmake (divide$ num gcd)(divide$ den gcd)))))))))


(defun qplus (x y)
  (cond
   ((integerp x)						
    (cond
     ((integerp y)(+ x y))					
     (t (qmake (+ (* x (den y))(num y)) (den y)))))
   ((integerp y)						
    (qmake (+ (* y (den x))(num x)) (den x)))
   (t
    (qmake (+ (* (den x)(num y))(* (den y)(num x)))
	   (* (den x)(den y))))))

(defun qdifference (x y)
  (cond
   ((integerp x)						
    (cond
     ((integerp y)(- x y))					
     (t (qmake (- (* x (den y))(num y))(den y)))))
   ((integerp y)						
    (qmake (- (num x)(* y (den x))) (den x)))
   (t
    (qmake (- (* (den y)(num x))(* (den x)(num y))) (* (den x)(den y))))))

(defun qtimes (x y)
  (cond
   ((integerp x)						
    (cond
     ((integerp y)(* x y))
     (t (qmake (* x (num y))(den y)))))
   ((integerp y)		
    (qmake (* y (num x))(den x)))
   (t
    (qmake (* (num x)(num y))(* (den x)(den y))))))

(defun qquotient (x y)
  (cond
   ((integerp x)		
    (cond
     ((integerp y)		
      (cond
       ((zerop y)(error "Division by zero: (~S)" y))
       (t (qmake x y))))
     ((zerop (num y))(error "(~S) Division by zero:" y))
     (t (qmake (* x (den y))(num y)))))
   ((integerp y)					
    (cond
     ((zerop y)(error "Divison by zero: (~S) " y))	
     (t (qmake (num x)(* (den x) y)))))
   ((zerop (num y))(error "Divison by zero: (~S)" y))	
   (t (qmake (* (num x)(den y))(* (num y)(den x))))))

(defun qfractpt (x)
  (cond
   ((integerp x) 0)					
   (t (qmake (remainder$ (num x)(den x)) (den x)))))

(defun qfloor (x)
  (cond
   ((integerp x) x)
   (t (floor (num x) (den x)))))

(defun qceiling (x)
  (cond
   ((integerp x) x)
   (t (ceiling (num x) (den x)))))

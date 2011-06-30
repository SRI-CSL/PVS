;;
;; Utilities to help testing
;;

(load-nl "polyrep-totdeglex")
(load-nl "decide3_2.lisp.save")

(load-nl "front-end-parser")
(load-nl "input2polyrep")


;;
;; Declare variables
;; x y z t u v: ordinary variables
;; a b c d: positive parameters
;; e f g h: non-negative parameters
;; 

(prep:set-variables '(x y z t u v))
(prep:set-parameters '(a b c d e f g h))

(gb:set-newU '(a b c d))
(gb:set-newV '(e f g h))

;; the parser code needs this VARS
(setq gbfe::VARS (list "x" "y" "z" "t" "u" "v" "a" "b" "c" "d" "e" "f" "g" "h"))

;; set degreebound/lengthbound to something larger than the default (i.e. 1)
;; (setq gb::*degreebound* 200)
;; (setq gb::*lengthbound* 200)


;; parser: string -> polyrep
(defun poly (s) (poly2prep (gbfe::parse-poly s)))

;; construct a POL structure from a polynomial p, with witness (w)
(defun pol (p w) (gb::make-pol p (list w) nil))

;; pretty print a polynomial 
(defun show-poly (p) 
  (format t "  ~a~%" (prep:polyrepPrint p)))

;; same thing for a POL structrue
(defun show-pol (p)
  (format t "  ~a~%  witness: ~a~%" (prep:polyrepPrint (gb::POL-pol p)) (gb::POL-wit p)))

;; show a list of POL 
(defun show-list (l)
  (format t "  ~a~%" (gb::pp l :key 'gb::POL-pol)))


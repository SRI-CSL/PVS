(in-package dp)

(use-package :ff) ;; use foreign function package
(require :foreign)

;; Datatypes for polyhedral library

;; vector datatype

(def-c-type (vector :in-foreign-space) : struct
            (Size :unsigned)
            (p * :int))

;; matrix datatype

(def-c-type (matrix :in-foreign-space) : struct
            (NbRows :unsigned)
            (NbColumns :unsigned)
            (p * * :int)
            (p_Init * :int))

;; polyhedron datatype

(defstruct (domain
	    (:predicate domain?)
	    (:constructor make-domain (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "~a" (domain-address p)))))
  address
  (freed? nil))

(defun domain-eq (p1 p2)
  (= (the integer (domain-address p1))
     (the integer (domain-address p2))))

(defun domain-null-p (p1)
  (declare (type domain p1))
  (= (the integer (domain-address p1)) (the integer 0)))

(defun mk-domain (a)
  (let ((p (make-domain a)))
    (domain-finalize! p)
    p))

(defun domain-finalize! (p)
  (excl:schedule-finalization p #'domain-free))

;(def-c-type (polyhedron :in-foreign-space) : struct 
;            (next :int)    ;; for pointer 
;            (Dimension :unsigned)
;            (NbConstraints :unsigned)
;            (NbRays :unsigned)
;            (NbEq :unsigned)
;            (NbBid :unsigned)
;            (Constraint * * :int)
;            (Ray * * :int)
;            (p_Init * :int))

(defun polyhedron-next (domain)
  (mk-domain (poly::polyhedron-next (domain-address domain))))

(defun polyhedron-dimension (domain)
  (poly::polyhedron-dimension (domain-address domain)))

(defun polyhedron-NbConstraints (domain)
  (poly::polyhedron-NbConstraints (domain-address domain)))

(defun polyhedron-NbRays (domain)
  (poly::polyhedron-NbRays (domain-address domain)))

(defun polyhedron-NbEq (domain)
  (poly::polyhedron-NbEq (domain-address domain)))

(defun polyhedron-NbBid (domain)
  (poly::polyhedron-NbBid (domain-address domain)))

(defun polyhedron-Constraint (domain)
  (poly::polyhedron-Constraint (domain-address domain)))

(defun polyhedron-Ray (domain)
  (poly::polyhedron-Ray (domain-address domain)))

(defun polyhedron-p_Init (domain)
  (poly::polyhedron-p_Init (domain-address domain)))

(defun matrix_alloc (rows cols)
  (poly::matrix_alloc rows cols))

(defun matrix-free (mat)
  (poly::matrix_free mat))

(defun polyhedron_print (domain)
  (poly::polyhedron_print (domain-address domain))
  domain)

(defun domain_print (domain)
  (poly::domain_print (domain-address domain))
  domain)

(defun addconstraints (Con2 NbCon2 Pol1 NbMaxRays)
  (mk-domain
   (poly::addconstraints Con2 NbCon2 (domain-address Pol1) NbMaxRays)))

(defun addpoltodomain (pol domain)
  (mk-domain
   (poly::addpoltodomain (domain-address pol) (domain-address domain))))

(defun constraints2polyhedron (constraints maxrays)
  (mk-domain (poly::constraints2polyhedron constraints maxrays)))

(defun domainaddconstraints (Pol1 Mat2 NbMaxRays)
  (mk-domain (poly::domainaddconstraints (domain-address Pol1) Mat2 NbMaxRays)))

(defun mydomainaddconstraints (Pol1 Mat2 NbMaxRays)
  (mk-domain (poly::mydomainaddconstraints (domain-address Pol1) Mat2 NbMaxRays)))

(defun domainaddrays (Pol1 Mat2 NbMaxRays)
  (mk-domain (poly::domainaddrays (domain-address Pol1) Mat2 NbMaxRays)))

(defun domainconvex (pol maxrays)
  (mk-domain (poly::domainconvex (domain-address pol) maxrays)))

(defun domaindifference (pol1 pol2 maxrays)
  (mk-domain (poly::domaindifference (domain-address pol1)
				     (domain-address pol2)
				     maxrays)))

(defun domainimage (pol1 func maxrays)
  (mk-domain (poly::domainimage (domain-address pol1) func maxrays)))

(defun domainintersection (pol1 pol2 maxrays)
  (mk-domain (poly::domainintersection (domain-address pol1)
				       (domain-address pol2) maxrays)))

(defun domainpreimage (pol1 func maxrays)
  (mk-domain (poly::domainpreimage (domain-address pol1) func maxrays)))

(defun domainsimplify (pol1 pol2 maxrays)
  (mk-domain (poly::domainsimplify (domain-address pol1) (domain-address pol2) maxrays)))

(defun domainunion (pol1 pol2 maxrays)
  (mk-domain (poly::domainunion (domain-address pol1) (domain-address pol2) maxrays)))

(defun domain_copy (pol)
  (mk-domain (poly::domain_copy (domain-address pol))))

(defun domain-free (p)
  (unless (domain-freed? p)
    (poly::domain_free (domain-address p))
    (setf (domain-freed? p) t))
  p)

(defun empty_polyhedron (dimension)
  (poly::empty_polyhedron dimension))

(defun polyhedron2constraints (pol)
  (poly::polyhedron2constraints (domain-address pol)))

(defun polyhedron2rays (pol)
  (poly::polyhedron2rays (domain-address pol)))

(defun polyhedronincludes (pol1 pol2)
  (poly::polyhedronincludes (domain-address pol1) (domain-address pol2)))

(defun polyhedron>= (pol1 pol2)
  (poly::mypolyhedronincludes (domain-address pol1) (domain-address pol2)))

(defun domainincludespolyhedron (pol1 pol2)
  (poly::domainincludespolyhedron (domain-address pol1) (domain-address pol2)))

(defun domain>= (pol1 pol2)
  (poly::mydomainincludes (domain-address pol1) (domain-address pol2)))

(defun poly-subsumed-p (poly1 poly2)
  (= (domain>= poly1 poly2) 1))

(defun polyhedron_free (p)
  (poly::polyhedron_free (domain-address p))
  (setf (domain-freed? p) t)
  p)

(defun polyhedron_image (pol1 func maxrays)
  (mk-domain
   (poly::polyhedron_image (domain-address pol1) func maxrays)))

(defun polyhedron_preimage (pol1 func maxrays)
  (mk-domain
   (poly::polyhedron_preimage (domain-address pol1) func maxrays)))

(defun rays2polyhedron (ray maxrays)
  (mk-domain (poly::rays2polyhedron ray maxrays)))

(defun subconstraint (Con2 Pol1 NbMaxRays Pass)
  (mk-domain (poly::subconstraint con2 (domain-address pol1)
				  NbMaxRays Pass)))

(defun universe_polyhedron (dimension)
  (mk-domain (poly::universe_polyhedron dimension)))

(defun polyhedron_alloc (dimension nbconstraints nbrays)
  (mk-domain
   (poly::polyhedron_alloc dimension nbconstraints nbrays)))

(defun inconsistent_p (pol)
  (poly::inconsistent_p (domain-address pol)))

(defun universe_p (pol)
  (poly::universe_p (domain-address pol)))

(defun pol_status ()
  (poly::pol_status))

(defun setf-matrix-entry (matrix row col val)
  (poly::matrix_set matrix row col val)
  val)

(defun matrix-entry (matrix row col)
  (poly::matrix_ref matrix row col))

(defsetf matrix-entry setf-matrix-entry)


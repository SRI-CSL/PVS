;; Polyhedral library interface

(in-package dp)

(use-package :ff) ;; use foreign function package
(require :foreign)

;; load polyhedral library files

(defvar *polylib-directory* (concatenate 'string *dp-path* *poly-path*))

#-dlfcn(load (string-append *polylib-directory* "/foreign_funcs.o")
	     :foreign-files
	     `(,(string-append *polylib-directory* "/vector.o")
	       ,(string-append *polylib-directory* "/polyhedron.o")))
#+dlfcn(load (string-append *polylib-directory* "/polylib.so"))


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

(def-c-type (polyhedron :in-foreign-space) : struct
            (next :int)    ;; for pointer 
            (Dimension :unsigned)
            (NbConstraints :unsigned)
            (NbRays :unsigned)
            (NbEq :unsigned)
            (NbBid :unsigned)
            (Constraint * * :int)
            (Ray * * :int)
            (p_Init * :int))


;; Polyhedral library function definition


;; from vector.c

;;Matrix_Alloc

(defforeign 'Matrix_Alloc ;; lisp function name
             :entry-point #+dlfcn "Matrix_Alloc"  ;; C (object) function name
	                  #-dlfcn "_Matrix_Alloc"  ;; C (object) function name
             :return-type  :integer  ;; return type = integer (i.e. pointer)
             :arguments '(integer integer))      ;; arguments types

(defforeign 'Matrix_Free
             :entry-point #+dlfcn "Matrix_Free"
	                  #-dlfcn "_Matrix_Free"
	     :return-type :void
	     :arguments '(integer))


;;Matrix_Read

(defforeign 'Matrix_Read ;; lisp function name
             :entry-point #+dlfcn "Matrix_Read"  ;; C (object) function name
	                  #-dlfcn "_Matrix_Read"  ;; C (object) function name
             :return-type  :integer  ;; return type = integer (i.e. pointer)
             :arguments nil)      ;; arguments types



;; from my_funcs.c

;;Matrix_Print

(defforeign 'Matrix_Print 
             :entry-point #+dlfcn "My_Matrix_Print"
	                  #-dlfcn "_My_Matrix_Print"  
             :return-type :void  
             :arguments '(integer))     

;;Polyhedron_Print

(defforeign 'Polyhedron_Print
             :entry-point #+dlfcn "My_Polyhedron_Print"
	                  #-dlfcn "_My_Polyhedron_Print"  
             :return-type :void  
             :arguments '(integer))     

;;Domain_Print

(defforeign 'Domain_Print
             :entry-point #+dlfcn "My_Domain_Print"
	                  #-dlfcn "_My_Domain_Print"  
             :return-type :void  
             :arguments '(integer))


;; from polyhedron.c

;; AddConstraints

(defforeign 'AddConstraints 
            :entry-point #+dlfcn "AddConstraints"
	                 #-dlfcn "_AddConstraints"
            :return-type :integer
            :arguments '(integer integer integer integer))

;;AddPolyToDomain

(defforeign 'AddPolToDomain
            :entry-point #+dlfcn "AddPolyToDomain"
	                 #-dlfcn "_AddPolyToDomain"
            :return-type :integer
            :arguments '(integer integer))

;; Constraints2Polyhedron

(defforeign 'Constraints2Polyhedron
            :entry-point #+dlfcn "Constraints2Polyhedron"
	                 #-dlfcn "_Constraints2Polyhedron"
            :return-type :integer
            :arguments '(integer integer))


;; DomainAddConstraints

(defforeign 'DomainAddConstraints 
            :entry-point #+dlfcn "DomainAddConstraints"
	                 #-dlfcn "_DomainAddConstraints"
            :return-type :integer
            :arguments '(integer integer integer))

;; MyDomainAddConstraints

(defforeign 'MyDomainAddConstraints 
            :entry-point #+dlfcn "MyDomainAddConstraints"
	                 #-dlfcn "_MyDomainAddConstraints"
            :return-type :integer
            :arguments '(integer integer integer))


;; DomainAddRays

(defforeign 'DomainAddRays 
            :entry-point #+dlfcn "DomainAddRays"
	                 #-dlfcn "_DomainAddRays"
            :return-type :integer
            :arguments '(integer integer integer))

;;DomainConvex

(defforeign 'DomainConvex 
            :entry-point #+dlfcn "DomainConvex"
	                 #-dlfcn "_DomainConvex"
            :return-type :integer
            :arguments '(integer integer))

;; DomainDifference

(defforeign 'DomainDifference
            :entry-point #+dlfcn "DomainDifference"
	                 #-dlfcn "_DomainDifference"
            :return-type :integer
            :arguments '(integer integer integer))

;; DomainImage

(defforeign 'DomainImage
            :entry-point #+dlfcn "DomainImage"
	                 #-dlfcn "_DomainImage"
            :return-type :integer
            :arguments '(integer integer integer))

;; DomainIntersection

(defforeign 'DomainIntersection
            :entry-point #+dlfcn "DomainIntersection"
	                 #-dlfcn "_DomainIntersection"
            :return-type :integer
            :arguments '(integer integer integer))


;; DomainPreimage

(defforeign 'DomainPreimage
            :entry-point #+dlfcn "DomainPreimage"
	                 #-dlfcn "_DomainPreimage"
            :return-type :integer
            :arguments '(integer integer integer))

;; DomainSimplify

(defforeign 'DomainSimplify
            :entry-point #+dlfcn "DomainSimplify"
	                 #-dlfcn "_DomainSimplify"
            :return-type :integer
            :arguments '(integer integer integer))


;; DomainUnion

(defforeign 'DomainUnion
            :entry-point #+dlfcn "DomainUnion"
	                 #-dlfcn "_DomainUnion"
            :return-type :integer
            :arguments '(integer integer integer))


;; Domain_Copy

(defforeign 'Domain_Copy
            :entry-point #+dlfcn "Domain_Copy"
	                 #-dlfcn "_Domain_Copy"
            :return-type :integer
            :arguments '(integer))


;; Domain_Free

(defforeign 'Domain_Free
            :entry-point #+dlfcn "Domain_Free"
	                 #-dlfcn "_Domain_Free"
            :return-type :void
            :arguments '(integer))


;; Empty_Polyhedron

(defforeign 'Empty_Polyhedron
            :entry-point #+dlfcn "Empty_Polyhedron"
	                 #-dlfcn "_Empty_Polyhedron"
            :return-type :integer
            :arguments '(integer))




;; Polyhedron2Constraints

(defforeign 'Polyhedron2Constraints
            :entry-point #+dlfcn "Polyhedron2Constraints"
	                 #-dlfcn "_Polyhedron2Constraints"
            :return-type :integer
            :arguments '(integer))




;; Polyhedron2Rays

(defforeign 'Polyhedron2Rays
            :entry-point #+dlfcn "Polyhedron2Rays"
	                 #-dlfcn "_Polyhedron2Rays"
            :return-type :integer
            :arguments '(integer))




;; PolyhedronIncludes

(defforeign 'PolyhedronIncludes
            :entry-point #+dlfcn "PolyhedronIncludes"
	                 #-dlfcn "_PolyhedronIncludes"
            :return-type :integer
            :arguments '(integer integer))

;; MyPolyhedronIncludes

(defforeign 'MyPolyhedronIncludes
            :entry-point #+dlfcn "MyPolyhedronIncludes"
	                 #-dlfcn "_MyPolyhedronIncludes"
            :return-type :integer
            :arguments '(integer integer))


;; DomainIncludesPolyhedron

(defforeign 'DomainIncludesPolyhedron
            :entry-point #+dlfcn "DomainIncludesPolyhedron"
	                 #-dlfcn "_DomainIncludesPolyhedron"
            :return-type :integer
            :arguments '(integer integer))

;; MyDomainIncludes

(defforeign 'MyDomainIncludes
            :entry-point #+dlfcn "MyDomainIncludes"
	                 #-dlfcn "_MyDomainIncludes"
            :return-type :integer
            :arguments '(integer integer))


;; Polyhedron_Free

(defforeign 'Polyhedron_Free
            :entry-point #+dlfcn "Polyhedron_Free"
	                 #-dlfcn "_Polyhedron_Free"
            :return-type :void
            :arguments '(integer))


;; Polyhedron_Image

(defforeign 'Polyhedron_Image
            :entry-point #+dlfcn "Polyhedron_Image"
	                 #-dlfcn "_Polyhedron_Image"
            :return-type :integer
            :arguments '(integer integer integer))


;; Polyhedron_Preimage

(defforeign 'Polyhedron_Preimage
            :entry-point #+dlfcn "Polyhedron_Preimage"
	                 #-dlfcn "_Polyhedron_Preimage"
            :return-type :integer
            :arguments '(integer integer integer))


;; Rays2Polyhedron

(defforeign 'Rays2Polyhedron
            :entry-point #+dlfcn "Rays2Polyhedron"
	                 #-dlfcn "_Rays2Polyhedron"
            :return-type :integer
            :arguments '(integer integer))


;; SubConstraint

(defforeign 'SubConstraint
            :entry-point #+dlfcn "SubConstraint"
	                 #-dlfcn "_SubConstraint"
            :return-type :integer
            :arguments '(integer integer integer integer))


;; Universe_Polyhedron

(defforeign 'Universe_Polyhedron
            :entry-point #-dlfcn "_Universe_Polyhedron"
	                 #+dlfcn "Universe_Polyhedron"
            :return-type :integer
            :arguments '(integer))

(defforeign 'Polyhedron_Alloc
            :entry-point #+dlfcn "Polyhedron_Alloc"
	                 #-dlfcn "_Polyhedron_Alloc"
	    :return-type :integer
	    :arguments '(integer integer integer))

;; Inconsistent polyhedron?

(defforeign 'Inconsistent_p
            :entry-point #+dlfcn "inconsistent_p"
	                 #-dlfcn "_inconsistent_p"
	    :return-type :integer
	    :arguments '(integer))

;; Unconstrained polyhedron?

(defforeign 'Universe_p
            :entry-point #+dlfcn "universe_p"
	                 #-dlfcn "_universe_p"
	    :return-type :integer
	    :arguments '(integer))

;; Status of last polyhedral operation

(defforeign 'Pol_status
            :entry-point #+dlfcn "Pol_status_f"
	                 #-dlfcn "_Pol_status_f"
	    :return-type :integer
	    :arguments '())

;; Set a coef in a matrix constraint

(defforeign 'Matrix_set
            :entry-point #+dlfcn "Matrix_Set"
	                 #-dlfcn "_Matrix_Set"
	    :return-type :integer
	    :arguments '(integer integer integer integer))

(defforeign 'Matrix_ref
            :entry-point #+dlfcn "Matrix_Ref"
	                 #-dlfcn "_Matrix_Ref"
	    :return-type :integer
	    :arguments '(integer integer integer))

;;;;;;;;;;;;;;;;;;;;;;;;example : a linear transformation
;;           (Matrix_Print
;;              (Polyhedron2Constraints
;;	        (Polyhedron_Image
;;		   (Constraints2Polyhedron (Matrix_Read) 200)
;;		   (Matrix_Read) 200 )
;;              )
;;           )

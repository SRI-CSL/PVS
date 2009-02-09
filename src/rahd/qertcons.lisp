;;;
;;; Coefficient Constraints for the Existence of Real Roots of Multivariate
;;;   Polynomial Equations and Inequalities derived from Real Quantifier 
;;;   Elimination.
;;;
;;;
;;; Began on 18-June-2008.
;;;

(defparameter *qe-coeff-constraint-table*
  '(("(E x)(E y)[a x^2 y^2 + b x^2 y + c = 0]"
     "b /= 0 \/ c = 0 \/ [ a > 0 /\ c < 0 ] \/ [ a < 0 /\ c > 0 ]"
     'QEPCAD-B)
    ("(E x)(E y)[a x^2 y^2 + b x^2 y + c >= 0]"
     "b /= 0 \/ a > 0 \/ c >= 0"
     'QEPCAD-B)
    ("(E x)(E y)[a x^2 y^2 + b x^2 y + c > 0]"
     "b /= 0 \/ a > 0 \/ c > 0"
     'QEPCAD-B)
    ("(E x)(E y)(E z)[a x^3 y^3 z^3 + b x^3 y^3 z + c = 0]"
     "b /= 0 \/ a /= 0 \/ c = 0"
     'QEPCAD-B)
    ))

;;; Idea: Index polynomials by their Newton Polytope.
;;;  Is there a fast unification algorithm we can derive from the polytope?

Important example from my new pre-squaring procedure:

I := Ideal([(-b^2 + 4ac)z^2 - 1, (ax^2 + bx + c)]);
ReducedGBasis(I);
[x^2a + xb + c, z^2ac - 1/4z^2b^2 - 1/4, x^2z^2b^2 + x^2 + 4xz^2bc + 4z^2c^2]
-------------------------------
Use Q[a,b,c,x,y,z], DegRevLex;
I := Ideal([(-b^2 + 4ac)z^2 - 1, (ax^2 + bx + c)]);
ReducedGBasis(I);
[ax^2 + bx + c, b^2z^2 - 4acz^2 + 1]


** Note that none contained an SOS + 1.



** This does!  I accomplished this by PRE-SQUARING.

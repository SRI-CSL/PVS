(in-package :pvs)

(defvar *ws1s-signature* nil)

;; Signature

(def-pvs-term fset-of-nats "finite_set[nat]" "strings" :nt type-expr)

(def-pvs-term empty-fset-of-nats "emptyset[nat]" "strings"
                                 :expected "finite_set[nat]")

(def-pvs-term add-to-fset "add[nat]" "strings"
                          :expected "[nat, finite_set[nat] -> finite_set[nat]]")


(def-pvs-term the1      "the[nat]" "naturalnumbers")
(def-pvs-term the2      "the[finite_set[nat]]" "strings")
(def-pvs-term minus1 "-" "naturalnumbers" :expected "[nat, nat -> nat]")
(def-pvs-term plus1  "+" "naturalnumbers" :expected "[nat, nat -> nat]")

(def-pvs-term add-operator "add" "strings"
                           :expected "[nat, set[nat] -> set[nat]]")
(def-pvs-term remove-operator "remove" "strings"
                           :expected "[nat, set[nat] -> set[nat]]")
(def-pvs-term singleton-operator "singleton" "strings"
                           :expected "[nat -> set[nat]]")
(def-pvs-term union-operator    "union" "strings"
                                    :expected "[set[nat], set[nat] -> set[nat]]")
(def-pvs-term intersection-operator "intersection" "strings"
                                    :expected "[set[nat], set[nat] -> set[nat]]")
(def-pvs-term set-difference-operator "difference" "strings"
                                    :expected "[set[nat], set[nat] -> set[nat]]")
(def-pvs-term emptyset-operator "emptyset" "strings"
                                    :expected "set[nat]")


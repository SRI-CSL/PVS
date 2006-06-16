(in-package :clos)
#+allegro
(preload-forms)

#-(and allegro (version>= 6))
(clos::preload-constructors (:cl-user :lisp :pvs))
#+(and allegro (version>= 6))
(excl::preload-constructors (:cl-user :lisp :pvs))

#-(and allegro (version>= 6))
(precache-generic-functions (:cl-user :lisp :pvs))
#+(and allegro (version>= 6))
(excl::precache-generic-functions (:cl-user :lisp :pvs))

#+lucid
(compile-all-dispatch-code)

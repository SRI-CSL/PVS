(in-package :clos)
#+allegro
(preload-forms)

#-(and allegro (version>= 6))
(clos::preload-constructors (:user :lisp :pvs))
#+(and allegro (version>= 6))
(excl::preload-constructors (:user :lisp :pvs))

#-(and allegro (version>= 6))
(precache-generic-functions (:user :lisp :pvs))
#+(and allegro (version>= 6))
(excl::precache-generic-functions (:user :lisp :pvs))

#+lucid
(compile-all-dispatch-code)

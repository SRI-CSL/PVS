(in-package :clos)
#+allegro
(preload-forms)

#+(and allegro (not allegro-v6.0))
(clos::preload-constructors (:user :lisp :pvs))
#+allegro-v6.0
(excl::preload-constructors (:user :lisp :pvs))

#+(and allegro (not allegro-v6.0))
(precache-generic-functions (:user :lisp :pvs))
#+allegro-v6.0
(excl::precache-generic-functions (:user :lisp :pvs))

#+lucid
(compile-all-dispatch-code)

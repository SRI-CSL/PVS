(in-package :clos)
#+allegro
(preload-forms)
#+allegro
(clos::preload-constructors (:user :lisp :pvs))
#+allegro
(precache-generic-functions (:user :lisp :pvs))

#+lucid
(compile-all-dispatch-code)

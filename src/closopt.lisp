(in-package :clos)
#+allegro
(preload-forms)

#+(and allegro (not (or allegro-v6.0 allegro-v6.2)))
(clos::preload-constructors (:user :lisp :pvs))
#+(or allegro-v6.0 allegro-v6.2)
(excl::preload-constructors (:user :lisp :pvs))

#+(and allegro (not (or allegro-v6.0 allegro-v6.2)))
(precache-generic-functions (:user :lisp :pvs))
#+(or allegro-v6.0 allegro-v6.2)
(excl::precache-generic-functions (:user :lisp :pvs))

#+lucid
(compile-all-dispatch-code)

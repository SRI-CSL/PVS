(in-package :clos)
#+allegro
(preload-forms)
#+allegro
(preload-constructors)
#+allegro
(precache-generic-functions)

#+lucid
(compile-all-dispatch-code)

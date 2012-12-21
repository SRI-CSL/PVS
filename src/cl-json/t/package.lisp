(defpackage :json-test
  (:use :json :json-rpc :common-lisp :5am )
  (:import-from #+(or mcl openmcl) #:ccl
                #+cmu #:clos-mop
                #+sbcl #:sb-mop
                #+(or clisp ecl lispworks) #:clos
                #+allegro #:mop
    #:finalize-inheritance))

(in-package :json-test)
(def-suite json)
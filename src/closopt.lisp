;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

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

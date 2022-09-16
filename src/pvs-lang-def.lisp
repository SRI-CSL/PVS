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

(in-package :pvs)

(use-package '(:ergolisp :oper :occ :term :sort :sb-runtime :lang :newattr))

(lang:lang-define 
:name "pvs"
:code-package (string :pvs) ; This works in both Allegro and SBCL
:abs-syn-package (string :pvs)
;; :conc-name "pvs7.2"
:use-packages '(:ergolisp :oper :occ :term :sort :sb-runtime :lang :newattr)
:working-dir (format nil "~asrc/" *pvs-path*)
;; :mergeable-default
;; :grammar-file (format nil "~a/src/pvs-gr.txt" *pvs-path*)
;; :lexer-file (format nil "~a/src/pvs-lexer.lisp" *pvs-path*)
;; :parser-file (format nil "~a/src/pvs-parser.lisp" *pvs-path*)
;; :unparser-file (format nil "~a/src/new-pvs-unparser.lisp" *pvs-path*)
:unparse-nts 't
;; :info-file (format nil "~a/src/new-pvs-info.lisp" *pvs-path*)
:package (string :pvs)
;; :sorts-file (format nil "~a/src/new-pvs-sorts.lisp" *pvs-path*)
;; :parse-routine-name 'pvs-parse
;; :unparse-routine-name 'pvs-unparse
;; :sort-table-name '*pvs-sort-table*	;
;; :opsig-table-name '*pvs-opsig-table*
;; :win-unparse-routine-name 'pvs-win-unparse
:sub-languages '(:lexical-terminals)
)

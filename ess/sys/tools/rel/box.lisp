;;;-*- Mode: lisp; Package: tools; -*-
;;;
;;; Author: fp
;;;
;;; Defines everything necessary for the BOX facility.  The main internal
;;; data structure is a hash-table of efiles.  An efile
;;; contains enough information to tell the Box tools how to regenerate it, in
;;; case one of the files it depends on has been edited.
;;;
;;; Sccs Id "@(#)box.lisp	1.38 10/31/89
;;; ******************************************************************* ;;;
;;;         (c) Copyright 1989 by Carnegie Mellon University.           ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;
;;;
;;; Nomenclature:
;;; crate - collection of boxes.
;;; box - ordered collection of files, possibly with the ability to process
;;;     other files.
;;; generator - box which can generate other files, like lisp-compiler or
;;;	syntax box.
;;; efile - a file including type, with other information.
;;; initial file - a file which is not generated, but a true ``source'' file.
;;;
;;; Revisions:
;;;
;;; srd  	Fri Jan 15 14:58:30 1988
;;;   Added patch for case bug in lisp version 2.1.0.  This bug requires that 
;;;   *print-case* be bound to :upcase during all compilations.  It may be
;;;   removed after that bug is fixed. 
;;;
;;; fp		Fri Dec 30 18:09:57 1988
;;;   Took out the patch (previous note) and moved to box-system.lisp, into
;;;   the lisp-compile function.

(in-package "TOOLS") (use-package :ergolisp)

(export '(defbox eload egen eforce eload-force eflag))
(export '(boxload boxgen boxforce boxload-force boxflag))
(export '(crategen crateload crateload-force))
(export '(locate-efile))
(export '(regenerate-with))
(export '(boxclear boxinit))  ; To reset the state of the BOXes.
(export '(box-needs box-path box-maintainers box-local-opts)) ; For use with SETF
(export '(print-all-boxes print-all-crates print-all-suffixes print-all-efiles
	  all-boxes all-crates all-suffixes all-efiles)) ; For checking
(export '(*box-debug* *box-messages* *box-warn*)) ; For controlling messages
(export '(*system-administration-mode*)) ; For generating a system.
(export '(*null-output* *box-edit-query*)) ; Misc. stuff

(eexport '(\#>))
(eexport '(push2 pop2))

;;;
;;; Structure and variable definitions.
;;;
	 
(defstruct (crate (:print-function print-crate))
  (name nil :type symbol)
  (boxes '() :type list)		; boxes, in the order specified.
  (sorted-boxes '() :type list)		; boxes, in the load-order.
  (doc-string nil :type (or null simple-string))
  )

(defstruct (box (:print-function print-box))
  (name nil :type symbol)
  (needs '() :type list)		; boxes this depends on when running
  (path '() :type list)			; path were to look for files in box
  (generates '() :type list)		; what box generates.  See longer doc.
  (local-opts '() :type list)		; The local options applying to every
					; file in the box.
  (files '() :type list)		; File-groups in this box, without
					;  local options.
  (loadable-efiles '() :type list)	; bare list of loadable files in the
					; box.  Thus is updated as time goes
					; on, and such files are discovered.
  (load-time 0 :type integer)		; Time the box was loaded last.
  (write-time 0 :type integer)		; Time the box was edited last
					; Max time over all files in the box.
  (maintainers () :type list)		; List of maintainers for this box:
					; They may regenerate, no one else.
  (doc-string nil :type (or null simple-string))
  (load-hook nil)			; A form to be evaluated after the box
					; has been loaded for the first time.
  (readme () :type list)		; List of readme files for this box.
					; Used for generating file lists.
  )

(defstruct (suffix (:print-function print-suffix))
  (string "" :type string)
  (key nil :type (or null keyword))	; keyword for the outputfile with this
					; suffix.
  (governing-suffixes '() :type list)	; A list of other `suffix'
					; structures governing this one.
					; eg. #>S".lbin" -> #>S".lisp"
  (governing-keys '() :type list)	; A list corresponding to suffixes
					; with the name of the keys for the
					; filename.
  ;; Do extending-suffixes at runtime
  ;;(extending-suffixes '() :type list); A list of other `suffix'es extending
					; this one.  E.g. #>S".lisp" ->
					; '(#>S"-lexer.lisp" #>S"-parser.lisp" .)
  (box nil :type (or null box))
  (loader-key nil :type (or null keyword))
					; the key for the call to loader-box
  (loader-box nil :type (or null box))	; The box to load a file with this
					; suffix.  NIL means: not loadable.
  )

(defstruct (efile (:print-function print-efile)) ; Ergo file object
  (name "" :type string)		; namestring of the file.
  (directory "" :type string)		; directory of the file when last loaded.
  (box nil :type (or null box))		; The box the file belongs to
  (local-opts '() :type list)		; Local options for processing this file.
  (load-time 0 :type integer)		; last load time in universal format.
  (loadable-suffix nil :type (or null suffix))
					; Show whether file may be loaded.
  ;; Next two are local options which we treat specially.
  (load '() :type list)			; Runtime needed files.
  (compile '() :type list)		; Compile-time needed files.
  ;; Last few properties are not persistent.  This helps us
  ;; because we don't need to keep track if any of that information
  ;; changed.  Eventually, we'd like to reuse some info.
  (flag-time 0 :type integer)		; When has the file been flagged 
					;  for regeneration?
  (write-time 0 :type integer)		; time of last write.
  (dir-file nil :type (or null string))	; where to load next time.
  (governing-efiles '() :type list)	; governing nodes in graph
  (eload-opts '() :type list)		; the options for a the current re-
					; generation of the file.
  (suffix nil :type (or null suffix))	; relevant suffix of file
  (compile-efiles '() :type list)	; special files needed for compilation.
  )

(defun efile-full-filename (efile)
  "Returns the full filename of `efile'."
  ;; this is not very safe
  (concatenate 'string (efile-directory efile) (efile-name efile)))

(defun set-efile-full-filename (efile fullname)
  "Update the :name and :directory components of `efile' from `fullname'."
  (setf (efile-name efile) (file-namestring fullname))
  (setf (efile-directory efile) (directory-namestring fullname))
  fullname)

(defsetf efile-full-filename set-efile-full-filename
  "Setf efile :name and efile :directory from the argument.")

(defun set-efile-dependencies (efile governing-efiles compile-efiles)
  "Changes the transient slots of `efile' and returns `efile'."
  (setf (efile-governing-efiles efile) governing-efiles)
  (setf (efile-compile-efiles efile) compile-efiles)
  efile)

(defvar *suffixtable* (make-hash-table :test #'equal)
  "Hash table of all known suffixes.  Key is suffix-string.")

(defvar *cratetable* (make-hash-table :test #'equal)
  "Hash table of all known crates.  Key is crate-name.")

(defvar *boxtable* (make-hash-table :test #'equal)
  "Hash table of all boxes.  Key is box-name.")

(defvar *efiletable* (make-hash-table :test #'equal)
  "Hash table of files.  Key if file-name (as string).")

(defun print-crate (crate stream depth)
  (declare (ignore depth))
  (format stream "#>C~S" (crate-name crate)))

(defun print-box (box stream depth)
  (declare (ignore depth))
  (format stream "#>B~S" (box-name box)))

(defun print-suffix (suffix stream depth)
  (declare (ignore depth))
  (format stream "#>S~S" (suffix-string suffix)))

(defun print-efile (efile stream depth)
  (declare (ignore depth))
  (format stream "#>E~S" (efile-name efile)))


(defun print-hashtable (hash-table)
  (format t "~&~D entries." (hash-table-count hash-table))
  (maphash #'(lambda (key value) (format t "~&==> ~S : ~S" key value))
	   hash-table))

(defun list-hash (hash-table)
  (let ((l nil))
    (maphash #'(lambda (key value) (declare (ignore key))
		 (push value l))
	     hash-table)
    l))

(defun all-crate ()
  (sort (list-hash *cratetable*) #'string< :key #'crate-name))

(defun print-all-crates ()
  (print-hashtable *cratetable*))

(defun all-boxes ()
  (sort (list-hash *boxtable*) #'string< :key #'box-name))

(defun print-all-boxes ()
  (print-hashtable *boxtable*))

(defun all-suffixes ()
  (sort (list-hash *suffixtable*) #'<
	:key #'(lambda (suffix) (length (suffix-string suffix)))))

(defun print-all-suffixes ()
  (print-hashtable *suffixtable*))

(defun all-efiles ()
  (sort (list-hash *efiletable*) #'string< :key #'efile-name))

(defun print-all-efiles ()
  (print-hashtable *efiletable*))

;;; Parse #>C for crate, #>B for box, #>S for suffix, #>E for efile.

(defun get-efile (file)
  (gethash file *efiletable*))

(defun get-existent-efile (file)
  (let ((efile (get-efile file)))
    (if (not efile)
	(progn
	  (box-cerror "Provide new file name."
		  "File ~S is unknown to the box tool."
		  file)
	  (format *query-io* "~%Give new filename (in doublequotes) : ")
	  (get-existent-efile (read *query-io*)))
	efile)))

(defun get-box (boxspec)
  (gethash (string-upcase (string boxspec)) *boxtable*))

(defun get-existent-box (boxspec)
  (let ((box (get-box boxspec)))
    (if (not box)
	(progn
	  (box-cerror "Provide new box name."
		  "Box ~S is undefined."
		  boxspec)
	  (format *query-io* "~%Give new boxname : ")
	  (get-existent-box (read *query-io*)))
	box)))

(defun box-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((type-char (read-char stream))
	(name-string (string (read stream))))
    (values				; Necessary, because of akcl only.
     (ccase type-char
       ((#\b #\B)
	(get-existent-box name-string))
       ((#\c #\C)
	(get-existent-crate name-string))
       ((#\s #\S)
	(gethash name-string *suffixtable*))
       ((#\e #\E)
	(gethash name-string *efiletable*))))))

(defun \#> ()
  (set-dispatch-macro-character #\# #\> #'box-reader))

(eval-when (load eval)
  (\#>))

(defun box-cerror (continue-string format-string &rest args)
  (apply #'cerror continue-string
	 (format nil "[BOX Error: ~A]" format-string) args))

(defun box-error (format-string &rest args)
  (apply #'error (format nil "[BOX Error: ~A]" format-string) args))

(defvar *box-debug* nil
  "Whether to show debugging messages generated by the box tool.
T means messages go to *DEBUG-IO*.")

(defun boxdebug (format-string &rest args)
  (when *box-debug*
    (format (if (eq *box-debug* t) *debug-io* *box-debug*)
	    "~&[[~?]]~%" format-string args)))

(defvar *box-messages* t
  "Whether to show messages generated by the box tool.  If NIL, nothing
will be shown.  If T, messages go to *standard-output*.  Otherwise
*box-message* should be an open stream to which to write messages.")

(defun boxmsg (format-string &rest args)
  (when *box-messages*
    (format *box-messages* "~&[~?]~%" format-string args))) 

(defvar *box-warn* t
  "Stream of warnings from the box tool.  NIL means not to show
warning.  T means the current value of *error-output*.")

(defun boxwarn (format-string &rest args)
  (when *box-warn*
    (if *break-on-warnings*
	(break "~&[WARNING: ~?]~%" format-string args)
	(format (if (eq *box-warn* t) *error-output* *box-warn*)
		"~&[WARNING: ~?]~%" format-string args))))


(defmacro setf-warn (place value test format-string &rest args)
  `(let ((value ,value))
     (when (not (funcall ,test value ,place))
       (boxwarn "~@? have changed." ,format-string ,@args))
     (setf ,place value)))

;;;
;;; Output-related stuff.
;;;

(defvar *null-output* nil
  "A data sink to reroute output to.")

(def-disksave-hook (setq *null-output* (make-broadcast-stream)))

(eval-when (load eval)
  (if (streamp *null-output*)
      *null-output*
      (setq *null-output* (make-broadcast-stream))))

(defmacro with-warnings-to (stream &rest forms)
  `(let* ((stream ,stream)
	  (*error-output* (if (eq stream t)
			      *error-output*
			      (if (null stream) *null-output* stream))))
     ,@forms))

(defmacro with-messages-to (stream &rest forms)
  `(let* ((stream ,stream)
	  (*standard-output* (if (eq stream t)
				 *standard-output*
				 (if (null stream) *null-output* stream))))
     ,@forms))


;; Alternative to (make-broadcast-stream) would be
;;  (open "/dev/null" :direction :output)

;;; To decide what to do when a processing box has been edited.

(defun box-edit-query (box)
  (let ((y-or-n
	 (y-or-n-p "Box ~S has been edited since it was last loaded.
Would you like to reload it? " box)) )
    (when y-or-n
      (boxload-fun (list (box-name box))))))
	

(defvar *box-edit-action* #'box-edit-query
  "Function called with the edited box as sole argument.")

;;; Utility functions into the Lisp package

(defmacro push2 (item1 item2 place)
  "Pushes first `item2' then `item1' onto the list in `place'."
  `(progn (push ,item2 ,place) (push ,item1 ,place)))

(defmacro pop2 (place)
  "Pops two things of `place', returning the last thing popped."
  `(progn (pop ,place) (pop ,place)))

;;;
;;; Timing stuff moved to box-system.lisp.
;;;

(defun user-equal (name1 name2)
  "Disregards case."
  (string-equal (string name1) (string name2)))

(defvar *system-administration-mode* nil
  "If T, the current user will be allowed to generate any box.
Dangerous if used unjudiciously, but a necessary backdoor.")

(defun generate-legal-p (box)
  "Determines if it is legal for the current user to generate a file
in the given box."
  (or *system-administration-mode*
      (member (user-name) (box-maintainers box) :test #'user-equal)
      (member :all (box-maintainers box))))

;;;
;;; *efiletable* maintenance.
;;;

(defun default-type-file (file)
  "Returns two values: first is defaulted filename, second is a suffix
which may be considered loadable, or NIL."
  (let ((pathname-type (pathname-type file)))
    (cond ((not pathname-type)
	   (values (namestring
		    (merge-pathnames file *lisp-compiled-extension-pathname*))
		   (gethash *lisp-compiled-suffix-string* *suffixtable*)))
	  (t (values file (find-first-loadable-suffix file))))))

(defun equiv-p (bool1 bool2)
  (or (and bool1 bool2)
      (and (not bool1) (not bool2))))

(defun file-enter (file local-opts box load-files compile-files)
  "Enters `file' into *efiletable* and returns `efile' object."
  ;; Fill in default extension.
  (multiple-value-bind 
      (file loadable-suffix)
      (default-type-file file)
    (let ((efile (gethash file *efiletable*)))
      (if (not efile)
	  ;; No previous entry: put it in there, return new efile.
	  (setf (gethash file *efiletable*)
		(make-efile :name file
			    :load-time 0
			    :loadable-suffix loadable-suffix
			    :box box
			    :local-opts local-opts
			    :load load-files
			    :compile compile-files))
	  ;; Previous entry.  Compare.
	  ;; Perhaps we should set a flag to reprocess if the options
	  ;; are changed?
	  (progn
	    (setf-warn (efile-box efile) box #'equal
		       "Box membership of file ~S" efile)
	    (setf-warn (efile-local-opts efile) local-opts #'equal
		       "Local options of file ~S" efile)
	    (setf-warn (efile-compile efile) compile-files #'equal
		       "Compile-time needed files for ~S" efile)
	    (setf-warn (efile-loadable-suffix efile) loadable-suffix #'equal
		       "Loader box of file ~S" efile)
	    (setf (efile-load efile) load-files)
	    efile)))))

(defun update-file-table (boxname)
  (let ((box (if (box-p boxname) boxname (gethash (string boxname) *boxtable*))))
    (when (not box)
      (box-error "No box with name ~S." boxname))
    (do ((filespecs (box-files box) (cdr filespecs))
	 (loadable-efiles
	  nil
	  (let ((new-efile (update-filespec (car filespecs) box)))
	    (if (loadable-efile-p new-efile)
		(cons new-efile loadable-efiles)
		loadable-efiles))))
	((null filespecs)
	 (setf (box-loadable-efiles box)
	       loadable-efiles)))))

(defun ensure-file-list (obj)
  (ctypecase obj
    (string (list obj))
    (list obj)))

(defun update-filespec (filespec box)
  "Enters filespec and its needed files into the *efiletable*.
It reverses the local-opts, :load and :compile specs.  Shouldn't matter, though.
Returns the entered efile."
  (setq filespec (ensure-file-list filespec))
  (do ((opts (rest filespec) (nthcdr 2 opts))
       (local-opts nil)
       (load-files nil)
       (compile-files nil)
       (key) (keyvalue))
      ((null opts)
       (file-enter (first filespec) local-opts box
		   load-files compile-files))
    (setq key (first opts))
    (setq keyvalue (second opts))
    (check-type key keyword "a keyword, either :load, :compile, or a local option.")
    (case key
      (:load (setq load-files (append (ensure-file-list keyvalue)
				      load-files)))
      (:compile (setq compile-files
		      (append (mapcar #'default-type-file
				      (ensure-file-list keyvalue))
			      compile-files)))
      (t (push keyvalue local-opts)
	 (push key local-opts)))))

;;;
;;; *suffixtable* maintenance.
;;;

(defun find-process-files (process-spec)
  "Returns four lists, inputkeys, inputfiles, outputkeys, outputfiles.
Makes some destructive changes to the processs-spec!"
  (check-type (first process-spec)
	      (or symbol string))
  (check-type (second process-spec) (member &key))
  (let ((inputkeys '()) (inputfiles '())
	(outputkeys '()) (outputfiles '()))
    (dolist (process-arg (rest (rest process-spec)))
      (check-type process-arg list)
      (check-type (first process-arg) symbol)
      ;; Replace symbol by corresponding keyword once and forall.
      (setf (first process-arg)
	    (intern (symbol-name (first process-arg))
		    (find-package "KEYWORD")))
      (let ((keyname (first process-arg)))
	(when (rest process-arg)
	  (let ((default (second process-arg)))
	    (when (rest (rest process-arg))
	      (let ((argtype (third process-arg)))
		(ecase argtype
		  (:input (push keyname inputkeys)
			  (push default inputfiles))
		  (:output (push keyname outputkeys)
			   (push default outputfiles))
		  (:lisp)
		  (:relevant-directory))))))))
    (values inputkeys inputfiles outputkeys outputfiles)))

(defun check-wild-string (string)
  (when (not (char= (char string 0) #\*))
    (box-cerror "Put `*' at the beginning of string."
	    "First character of filename string ~S is not a `*'."
	    string)
    (setq string (concatenate 'string "*" string)))
  string)

(defun enter-suffix-table (inputfile)
  "Enters a suffix into the suffix table, if it isn't there already.
Returns the suffix."
  (setq inputfile (check-wild-string inputfile))
  (let* ((suffix-string (subseq inputfile 1))
	 (suffix (gethash suffix-string *suffixtable*)))
    (when (not suffix)
      (setq suffix (make-suffix :string suffix-string))
      (setf (gethash suffix-string *suffixtable*)
	    suffix))
    suffix))

(defun update-loader-suffix (suffix &key (loader-key nil) (loader-box nil))
  "Updates the loader-key and loader-box slot of a suffix."
  (if (suffix-loader-key suffix)
      (progn
	(setf-warn (suffix-loader-key suffix) loader-key #'eq
		   "The keyword argument name for loading files with suffix ~S"
		   suffix)
	(setf-warn (suffix-loader-box suffix) loader-box #'equal
		   "The box loading files with suffix ~S" suffix))
      (progn
	(setf (suffix-loader-key suffix) loader-key)
	(setf (suffix-loader-box suffix) loader-box)))
  suffix)

(defun update-suffix (suffix &key (key nil) (governing-suffixes nil)
			     (governing-keys nil) (box nil))
  "Updates a suffix, perhaps issues warnings."
  (if (suffix-key suffix)
      (progn
	(setf-warn (suffix-key suffix) key #'eq
		   "The keyword argument name for the outputfile with suffix ~S"
		   suffix)
	(setf-warn (suffix-governing-suffixes suffix) governing-suffixes #'equal
		   "The governing suffixes for suffix ~S" suffix)
	(setf-warn (suffix-governing-keys suffix) governing-keys #'equal
		   "The keyword argument names for the inputfils to produce a file
with suffix ~S"
		   suffix)
	(setf-warn (suffix-box suffix) box #'equal
		   "The box producing files with suffix ~S" suffix))
      (progn
	(setf (suffix-key suffix) key)
	(setf (suffix-governing-suffixes suffix) governing-suffixes)
	(setf (suffix-governing-keys suffix) governing-keys)
	(setf (suffix-box suffix) box)))
  suffix)

(defun set-governing-suffixes (outputkey outputfile inputkeys inputfiles box)
  "Updates the suffix-table by entering the given dependency."
  (check-type outputfile string)
  (setq outputfile (check-wild-string outputfile))
  (let* ((output-suffix-string (subseq outputfile 1))
	 (output-suffix (gethash output-suffix-string *suffixtable*)))
    (if (not output-suffix)
	(setf (gethash output-suffix-string *suffixtable*)
	      (make-suffix :string output-suffix-string
			   :key outputkey
			   :governing-suffixes (mapcar #'enter-suffix-table
						       inputfiles)
			   :governing-keys inputkeys
			   :box box))
	(update-suffix output-suffix
		       :key outputkey
		       :governing-suffixes (mapcar #'enter-suffix-table
						   inputfiles)
		       :governing-keys inputkeys
		       :box box))))

(defun set-loader-box (inputkey inputfile box)
  (check-type inputfile string)
  (setq inputfile (check-wild-string inputfile))
  (let* ((input-suffix-string (subseq inputfile 1))
	 (input-suffix (gethash input-suffix-string *suffixtable*)))
    (if (not input-suffix)
	(setf (gethash input-suffix-string *suffixtable*)
	      (make-suffix :string input-suffix-string
			   :loader-key inputkey
			   :loader-box box))
	(update-loader-suffix input-suffix
			      :loader-key inputkey
			      :loader-box box))))

(defun update-suffix-table (boxname)
  (let ((box (if (box-p boxname) boxname (gethash (string boxname) *boxtable*))))
    (when (not box)
      (box-error "No box with name ~S." boxname))
    (let ((box-generates (box-generates box)))
      (when (not (null box-generates))
	(multiple-value-bind (inputkeys inputfiles outputkeys outputfiles)
	    (find-process-files box-generates)
	  ;; for now a box just has one process-function.
	  ;; Now we have to decide whether the box is a loader-box or
	  ;; a truly generating box.
	  (if (null outputfiles)
	      ;; No outputfiles: is a loader-box.
	      (progn
		(when (> (length inputfiles) 1)
		  (box-error "A loading box can only take one input file right now,
but box ~S wants to take files ~S."
			     box inputfiles))
		(mapcar #'(lambda (inputkey inputfile)
			    (set-loader-box inputkey inputfile box))
			inputkeys inputfiles))
	      (mapcar #'(lambda (outputkey outputfile)
			  (set-governing-suffixes outputkey outputfile
						  inputkeys inputfiles
						  box))
		      outputkeys outputfiles)))))))

;;;
;;; Processing (defbox ...)
;;;

(defun switch-arg-format (arglist)
  "Switches argument specs of the form (:key value ...) to :key '(value ...)"
  (mapcan #'(lambda (argpair) `(,(car argpair) ',(cdr argpair)))
	  arglist))
       
(defun update-box (box &key (needs nil) (path nil) (generates nil)
		       (files nil) (maintainers nil) (doc-string nil)
		       (load-hook nil) (local-opts nil)
		       (readme nil))
  "Updates a box, perhaps warns about changes."
  (setf-warn (box-needs box) needs #'equal "Needed boxes of box ~S" box)
  (setf-warn (box-path box) path #'equal "Path of box ~S" box)
  (setf (box-generates box) generates)
  (setf-warn (box-files box) files #'equal "Files of box ~S" box)
  (setf-warn (box-local-opts box) local-opts #'equal
	     "Local options of box ~S" box)
  (setf-warn (box-maintainers box) maintainers #'equal
	     "Maintainers of box ~S" box)
  (setf-warn (box-load-hook box) load-hook #'equal "Load hook of box ~S" box)
  (setf-warn (box-readme box) readme #'equal
	     "Readme files of box ~S" box)
  (setf (box-doc-string box) doc-string)
  box)


(defun update-dependencies (box)
  "This builds a dependency-graph for every file in `box' at least once.
Since we cannot be sure whether the information is correct at
generation time, this is done again at generation time, but this
pass makes the governing files known and available for flagging."
  (dolist (loadable-efile (box-loadable-efiles box))
    (dependency-graph loadable-efile nil)))

(defmacro defbox (boxname &rest args)
  (let ((make-box-args args)
	(doc-string
	 (if (stringp (car args))
	     (car args)
	     nil)))
    (when doc-string (setq make-box-args (rest make-box-args)))
    (setq make-box-args (switch-arg-format make-box-args))
    (push `,doc-string make-box-args)
    (push `:doc-string make-box-args)
    (push `(string ',boxname) make-box-args)
    (push `:name make-box-args)
    `(let ((box (get-box ',boxname)))
       (if (not box)
	   (progn
	     (setf (gethash (string ',boxname) *boxtable*)
		   (make-box ,@make-box-args))
	     (setq box (get-existent-box ',boxname)))
	   (update-box box ,@(rest (rest make-box-args))))
       (update-suffix-table box)
       (update-file-table box)
       (update-dependencies box)
       ',boxname)))

;;;
;;; Processing (defcrate ...)
;;;

(defun get-crate (cratespec)
  (gethash (string-upcase (string cratespec)) *cratetable*))

(defun get-existent-crate (cratespec)
  (let ((crate (get-crate cratespec)))
    (if (not crate)
	(progn
	  (box-cerror "Provide new crate name."
		  "Crate ~S is undefined."
		  cratespec)
	  (format *query-io* "~%Give new cratename : ")
	  (get-existent-crate (read *query-io*)))
	crate)))

(defun update-crate (crate &key (boxes nil) (doc-string nil))
  "Updates a crate, perhaps warns about changes."
  (setf-warn (crate-boxes crate) boxes #'equal "Boxes of crate ~S" crate)
  (setf (crate-doc-string crate) doc-string)
  crate)

(defmacro defcrate (cratename &rest args)
  (let ((make-crate-args args)
	(doc-string
	 (if (stringp (car args))
	     (car args)
	     nil)))
    (when doc-string (setq make-crate-args (rest make-crate-args)))
    (setq make-crate-args (switch-arg-format make-crate-args))
    (push `,doc-string make-crate-args)
    (push `:doc-string make-crate-args)
    (push `(string ',cratename) make-crate-args)
    (push `:name make-crate-args)
    `(let ((crate (get-crate ',cratename)))
       (if (not crate)
	   (progn
	     (setf (gethash (string ',cratename) *cratetable*)
		   (make-crate ,@make-crate-args))
	     (setq crate (get-existent-crate ',cratename)))
	   (update-crate crate ,@(rest (rest make-crate-args))))
       ',cratename)))

;;;
;;; Building the dependency graph.
;;;

(defun member2 (item list)
  "Like member, but checks only the even-numbered elements of list."
  (cond ((null list) nil)
	((null (rest list)) (box-error "List ~S is of odd length." list))
	((eql item (first list)) list)
	(t (member2 item (rest (rest list))))))

(defun merge-opts-conflict (conflict-fun opts-list)
  "Merge option lists in `opts-list'.  If there is a conflict, `conflict-fun'
will be called with three arguments: key, value, and all previously collected
options.  It is expected to return a new list of options."
  (do ((opts-list opts-list (rest opts-list))
       (final-opts nil))
      ((null opts-list) final-opts)
    (do ((opts (first opts-list) (rest (rest opts)))
	 (key) (value))
	((null opts) nil)
      ;; Legality check
      (when (not (and (consp opts) (consp (rest opts))))
	(box-error "Invalid list of options: ~S." opts))
      (setq key (first opts))
      (setq value (second opts))
      (check-type key keyword "a keyword in a list of options.")
      (if (not (member2 key final-opts))
	  (progn (push value final-opts)
		 (push key final-opts))
	  (setq final-opts (funcall conflict-fun key value final-opts))))))

(defun merge-opts (&rest opts-list)
  "Merge options in `opts-list'.  The farther to the left an option is,
the higher its precedence."
  (merge-opts-conflict
   #'(lambda (key value final-opts)
       (boxdebug "Local option ~S with value ~S is ignored
since explicitly overriden."
		 key value)
       final-opts)
   opts-list))

(defun delete-conflicting-opts (key value final-opts)
  "Deletes all options with `key' from `final-opts' if `value' disagrees
with the previously given value."
  (let* ((previous-key-tail (member2 key final-opts))
	 (previous-value (second previous-key-tail)))
    (if (equal value previous-value)
	final-opts
	(progn (boxwarn "Conflicting values ~S and ~S for local option ~S.
Ignoring both."
			previous-value value key)
	       (append (ldiff final-opts previous-key-tail)
		       (rest (rest previous-key-tail)))))))

(defun merge-input-opts (&rest opts-list)
  "Merge options in `opts-list', but gives a warning if an option is
defined twice and ignores it.  This is used when several input files
to a generating function have local options."
  (merge-opts-conflict #'delete-conflicting-opts opts-list))


(defun file-written (pathname &key (dir nil))
  "The date a file was last written.  Uses truename to resolve symbolic
links for reliable information.  `:dir' given directory to look in."
  (when dir (setq pathname (merge-pathnames pathname dir)))
  (if (probe-file pathname) (file-write-date pathname) 0))

(defun loadable-file-check (file)
  "If `file' ends in .lisp or .lbin, return file, otherwise append
.lbin to the name of `file'.  Returns NIL if file has wrong type."
  (let ((pathname-type (pathname-type file)))
    (cond ((not pathname-type)
	   (namestring
	    (merge-pathnames file *lisp-compiled-extension-pathname*)))
	  (t file))))

#|
(defun loadable-file (file)
  "Coerces `file' into a loadable file and issues an error if that's not
possible."
  (let ((loadable-file (loadable-file-check file)))
    (if loadable-file
	loadable-file
	(progn
	  (box-cerror "Supply a new filename."
		  "Illegal extension ~S for file ~S."
		  (pathname-type file) file)
	  (loadable-file (read-line *query-io*))))))

(defun loadable-efile (file)
  (let ((extended-file (loadable-file file)))
    (get-default-efile extended-file)))
|#

(defun loadable-efile-p (efile)
  (let ((loadable-suffix (efile-loadable-suffix efile)))
    (if (not loadable-suffix)
	nil
	(suffix-loader-box loadable-suffix))))

(defun loadable-efile-check (file)
  (let ((extended-file (loadable-file-check file)))
    (get-default-efile (if extended-file extended-file file))))

(defun get-default-efile-box (file box warn-p)
  (let* ((file-name (file-namestring file))
	 (file-directory (directory-namestring file))
	 (efile (gethash file-name *efiletable*)))
    (when (not efile)
      (when warn-p (boxwarn "File ~S not known to ELOAD.  ~
Assuming it's in box ~S." file box))
      (setq efile (make-efile :name file-name
			      :directory file-directory
			      :box box))
      (setf (gethash file-name *efiletable*) efile))
    efile))


(defun get-default-efile (file)
  "Returns the efile associated with `file' in the *efiletable*.  Default
extension is *lisp-compiled-extension*.  If `file' is not found, it is
assumed to be in *default-box*, which may lead to warning messages later."
  (get-default-efile-box file (get-existent-box "*DEFAULT-BOX*") t))

(defun get-default-efile-in-box (file box)
  "Like `get-default-efile', but assumes `file' to be in box, rather than
to be unknown, if it is not in the *efiletable*."
  (get-default-efile-box file box nil))


(defun matching-suffix-p (string suffix-string)
  "Checks whether `string' ends in `suffix-string'.  `suffix-string'
may not be empty.  Uses STRING=, ie case matters."
  (and (> (length string) (length suffix-string))
       (string= string suffix-string
		:start1 (- (length string) (length suffix-string)))))

(defun longer-matching-suffix-p (string suffix-string minlength)
  "Checks whether `string' ends in `suffix-string'.  `suffix-string'
must be at least minlength characters long.  Uses STRING=, ie case matters."
  (and (> (length suffix-string) minlength)
       (matching-suffix-p string suffix-string)))

(defun longer-matching-suffixes (efile previous-suffix)
  "Looks for all matching suffixes above `previous-suffix' in
the suffix graph.  Eg
 (find-matching-suffix \"a-parser.lisp\" #>S\".lisp\") => (#>S\"-parser.lisp\").
Hack this for now by looking at all suffixes, but discarding those which
are too long."
  (let ((previous-length (length (suffix-string previous-suffix)))
	(file (efile-name efile))
	(matching-suffixes nil))
    (maphash #'(lambda (suffix-key suffix)
		 ;; use fact that key = string.
		 (when (longer-matching-suffix-p file suffix-key
						 previous-length)
		   (push suffix matching-suffixes)))
	     *suffixtable*)
    matching-suffixes))

(defun find-first-suffix-name (file filter)
  "Returns the first generated suffix matching file.  Usually #>S\".lbin\".
Filter should be either #'suffix-loader-box (for loadable files) or
#'suffix-key (for files which are supposed to be generated)."
  (let ((matching-suffixes nil))
    (maphash #'(lambda (suffix-key suffix)
		 ;;  use that key = string.
		 (when (and (funcall filter suffix)
			    ;; must be a generating or loadable suffix.
			    (longer-matching-suffix-p file suffix-key 0))
		   (push suffix matching-suffixes)))
	     *suffixtable*)
    (cond ((not matching-suffixes) nil)	; initial file
	  ((not (rest matching-suffixes)) ; one match
	   (first matching-suffixes))
	  (t (box-error "More than one matching suffix for ~S:
Any of ~S matches."
		    file matching-suffixes)))))

(defun find-first-loadable-suffix (file)
  (find-first-suffix-name file #'suffix-loader-box))

(defun find-first-suffix (efile)
  (find-first-suffix-name (efile-name efile) #'suffix-key))

(defun strip-suffix (file suffix)
  (subseq file 0 (- (length file) (length (suffix-string suffix)))))

(defun append-suffix (file-prefix suffix)
  (concatenate 'string file-prefix (suffix-string suffix)))

(defun governing-efiles (efile suffix eload-opts)
  "Returns a list of efiles where in each one `suffix' of `efile'
has been substituted by its governing suffixes."
  (setf (efile-suffix efile) suffix)
  (let ((merged-opts (merge-opts eload-opts (efile-local-opts efile))))
    (setf (efile-eload-opts efile) merged-opts)
    (let ((file-prefix (strip-suffix (efile-name efile) suffix)))
      (mapcar #'(lambda (new-suffix)
		  (let* ((new-file (append-suffix file-prefix new-suffix))
			 (new-efile (get-default-efile-in-box
				     new-file (efile-box efile))))
		    (set-efile-dependencies
		     new-efile
		     (extending-efiles new-efile new-suffix merged-opts)
		     nil)		; NIL here: no compile-efiles.
		    new-efile))
	      (suffix-governing-suffixes suffix)))))

(defun extending-efiles (efile suffix eload-opts)
  (let ((matching-suffixes (longer-matching-suffixes efile suffix)))
    (cond ((not matching-suffixes) nil)
	  ((not (rest matching-suffixes))
	   (governing-efiles efile (first matching-suffixes) eload-opts))
	  (t (box-error "More than one matching suffix for ~S after ~S:
Any of ~S matches."
		    (efile-name efile) (suffix-string suffix)
		    (mapcar #'suffix-string matching-suffixes))))))

(defun get-compile-efiles (efile)
  "Build the dependency graph for the files needed for compiling.
Eload options are not passed on as local options."
  (mapcar #'(lambda (efile) (dependency-graph efile nil))
	  (mapcar #'get-default-efile (efile-compile efile))))

(defun dependency-graph (efile eload-opts)
  "Builds a dependency graph for `efile' given `eload-opts'."
  ;; Two cases: efile may be generated, or is initial (like file.lisp).
  ;; If initial, nothing is to be done.  If not, build a dependency-graph.
  (let ((suffix (efile-suffix efile)))
    (when (not suffix)
      (setq suffix (find-first-suffix efile)))
    ;; if still NIL, means file is initial
    (if (not suffix)
	(set-efile-dependencies efile nil nil)
	(set-efile-dependencies
	 efile
	 (governing-efiles efile suffix eload-opts)
	 (get-compile-efiles efile))))
  efile)


;;;
;;; Interpreting the dependency graph
;;;

(defun dependency-list (efile)
  "Returns the list of files in `graph' without directory qualifier.
Changed this to exclude macro files: they are searched for separately."
  ;;; There is a problem here with the (unqualified) macro files.
  ;;; For now, also look for macro files only in the relevant directory!
  (let ((governing-efiles (append (efile-governing-efiles efile)
				  ;;(efile-compile-efiles efile)
				  )))
    (cons (efile-name efile)	; Want a string here!
	  (mapcan #'dependency-list governing-efiles))))

(defun list-merge (keylist valuelist)
  (mapcan #'list keylist valuelist))

(defun generator-apply (gen-funspec args box)
  "Applies, with appropriate error checks, `gen-function' to `args'."
  (retry-catch :apply
    (let ((gen-function (etypecase gen-funspec
			  (symbol gen-funspec)
			  (string (read-from-string gen-funspec)))))
      (when (not (fboundp gen-function))
	(box-cerror "Retry :apply"
		"Function ~S generated from function specification ~S is undefined."
		gen-function gen-funspec)
	(retry :apply))
      (let ((gen-result (apply gen-function args)))
	(when gen-result
	  ;; gen-result non-NIL means: error while processing box.
	  (box-cerror "Call (retry)"
		  "The generating box ~S has signalled an error condition.
If you would like to retry (after editing) or ignore the error, use `:c'."
		  box)
	  (retry-catch :ignore (retry)))))))


(defun eprocess-with-box (box input-efiles output-efile relevant-directory)
  "Should check for non-existing input-files!  Returns creation date
for the requested file (argument `output-efile')."
  ;; Make sure dir-file info is correct at this point!
  (let ((input-files (mapcar #'efile-dir-file input-efiles))
	(output-file (efile-dir-file output-efile))
	(output-suffix (efile-suffix output-efile))
	(process-fnspec (first (box-generates box)))
	(relevant-dir-key (find :relevant-directory (cddr (box-generates box))
				:key #'third)))
    ;; Need some kind of catch or unwind-protect here
    ;; `output-file'?  `local-opts'?
    ;; fnspec should be (function ...), evaluate it to get function.
    ;; Perhaps there should be some kind of check here.
    ;; Here we should have the relevant directory!
    (when (null output-file)
      (setq output-file (efile-name output-efile)))
    (if (not (generate-legal-p (efile-box output-efile)))
	(boxwarn "File ~S of box ~S should be regenerated,
but only ~S are maintainers of the box."
		 output-efile (efile-box output-efile)
		 (box-maintainers (efile-box output-efile)))
	(progn
	  (cond ((= 0 (box-load-time box))
		 ;; Box has never been loaded.
		 (boxmsg "Box ~S is needed for generating file ~S, but not loaded."
			 box output-efile)
		 (boxload-fun (list (box-name box))))
		((> (box-write-time box) (box-load-time box))
		 ;; Box has been edited incompatibly.
		 (boxwarn "Box ~S has been edited.")
		 (funcall *box-edit-action* box)))
	  (let* ((input-opts (apply #'merge-input-opts
				    (mapcar #'efile-local-opts input-efiles)))
		 (process-args
		  (merge-opts
		   (list-merge (suffix-governing-keys output-suffix) input-files)
		   (list (suffix-key output-suffix) output-file)
		   ;; Input and output-file cannot be overriden by local options.
		   (if relevant-dir-key
		       (list (first relevant-dir-key) relevant-directory)
		       '())
		   (efile-eload-opts output-efile)
		   input-opts
		   (box-local-opts (efile-box output-efile)))))
	    (boxmsg "Applying function ~A to ~S." process-fnspec process-args)
	    (generator-apply process-fnspec process-args box))))
    ;; The TRUENAME in the next line is necessary so that symbolic
    ;; links are resolved correctly.
    (setf (efile-dir-file output-efile) (namestring (truename output-file)))
    (efile-update-dir output-efile)))

(defun locate-efile (efile-name)
  "Locates a given file (as string) in some box and return its full pathname."
  (let ((efile (get-existent-efile efile-name)))
    (let ((annotated-efile (find-efile efile)))
      (efile-dir-file annotated-efile))))

(defun find-efile (efile)
  "Looks for `efile' along the appropriate search path and sets its
`dir-file' slot if found.  Signals an error if not found."
  (let ((path (box-path (efile-box efile))))
    (retry-catch :search
      (dolist (dir path)
	(let ((existent-file (probe-file (merge-pathnames (efile-name efile)
							  dir))))
	  (when existent-file
	    (setf (efile-dir-file efile) (namestring existent-file))
	    (return-from find-efile efile))))
      (box-cerror "Ignore the fact that file does not seem to exist."
	      "File ~S not found in path ~S of box ~S."
	      efile path (efile-box efile))
      nil)))

(defun exists-efile (efile)
  "Tests if `efile' exists in the relevant directory.  This assume that
efile-date-dir has been called on the file!"
  (not (= (efile-write-time efile) 0)))

(defun eprocess-do (efile why relevant-directory)
  "Returns the file-write-date of the newly generated file after regenerating
`efile'.  Assumes that the `dir-file' slot of `efile' is correct."
  ;; why should be :compile or :govern, or :flagged.
  (let ((efile-suffix (efile-suffix efile)))
    (if (not efile-suffix)
	(progn
	  (ecase why
	    (:compile (if (efile-compile efile)
			  (boxwarn "Initial file ~S supposedly depends on some files
 for compilation.  Ignoring this illegal dependency."
				   efile)
			  (box-error "Box internal error.
Initial file ~S does not depend on any macro files,
but is supposed to be regenerated because macro files have changed."))
		      (efile-date-dir efile))
	    (:govern (when (efile-governing-efiles efile)
		       (box-cerror "Ignore dependency."
				   "Initial file ~S supposedly depends on ~S."
				   efile (efile-governing-efiles efile)))
		     (efile-date-dir efile))
	    (:flagged (boxdebug "Not regeneration flagged initial file ~S" efile)
		      ;; Here we must return the flag-time, since dependent
		      ;; files should be regenerated, if they haven't since
		      ;; the last time the initial file was flagged.
		      (efile-flag-time efile))))
	(let* ((input-efiles (efile-governing-efiles efile))
	       (absent-input-efiles
		(remove-if #'exists-efile input-efiles)))
	    (when (not (null absent-input-efiles))
	      (box-cerror (format nil
			      "Search for the file along path ~S for box ~S."
			      (box-path (efile-box efile)) (efile-box efile))
		      "Cannot regenerate file ~S since governing files ~S
are not found in the relevant directory."
		      efile absent-input-efiles)
	      (dolist (absent-file absent-input-efiles)
		;; The find-efile has the side-effect of setting
		;; (efile-dir-file absent-file) for later.
		(find-efile absent-file)))
	    (let ((suffix-box (suffix-box efile-suffix)))
	      (when (not suffix-box)
		(box-error "The efile ~S (as ~S) does not seem to
be present and it cannot be regenerated (probably initial).
Most likely a file generated FROM ~S was found.
Likely fixes are
   (1) delete the generated file and (retry)
or (2) fix the search path for box ~S which currently is
       ~S and (retry)."
			   efile (efile-dir-file efile) efile (efile-box efile)
			   (box-path (efile-box efile))))
	      (boxmsg "Regenerating efile ~S from ~S using box ~S
 because ~A."
		      efile input-efiles suffix-box
		      (ecase why
			(:compile "a macro file has changed")
			(:govern "a governing file is newer")
			(:flagged "it was explicitly flagged for regeneration")))
	      (eprocess-with-box suffix-box input-efiles efile
				 relevant-directory))))))

(defun efile-date (efile relevant-files relevant-directory)
  "Returns date efile was last written.  It also updates the dir-file
slot of the efile to the full filename.  If current directory and
previous directory do not coincide, the load-time is reset to 0."
  (cond ((member (efile-name efile) relevant-files :test #'string=) ;; !!!
	 ;; File exists in relevant directory.
	 (let ((dir-file (namestring
			  (merge-pathnames (efile-name efile)
					   relevant-directory))))
	   (when (not (string= dir-file (efile-dir-file efile)))
	     (setf (efile-load-time efile) 0))
	   (setf (efile-dir-file efile) dir-file)
	   (efile-date-dir efile)))
	((has-directory-component (efile-name efile))
	 (box-cerror "Strip off directory."
		 "Efile ~S has directory component."
		 efile)
	 (setf (efile-name efile) (file-namestring (efile-name efile)))
	 (efile-date efile relevant-files relevant-directory)
	 #|
	 ;; Has directory component: leave alone (except for truename)
	 (let ((truename (probe-file (efile-name efile))))
	   (if (not truename)
	       (progn (setf (efile-dir-file efile) (efile-name efile))
		      (setf (efile-write-time efile) 0)
		      0)
	       (progn (setf (efile-dir-file efile) (namestring truename))
		      (efile-date-dir efile))))
	|#
	)
	(t
	 ;; File does not exist in relevant directory.	
	 (setf (efile-dir-file efile)
	       (namestring (merge-pathnames (efile-name efile)
					    relevant-directory)))
	 ;; Hopefully will be written later.
	 (setf (efile-write-time efile) 0)
	 0)))

(defun efile-update-dir (efile)
  "Called after a file has supposedly been written and returns the last
write date (also saving that information somewhat redundantly).  Issues
an error, if the file has not been written and is therfore suspect."
  (let ((efile-date (file-written (efile-dir-file efile))))
    (when (= efile-date (efile-write-time efile))
      (box-cerror "Ignore this inconsistency."
	      "File ~S which was supposed to be generated has not been written."
	      efile))
    (setf (efile-write-time efile) efile-date)))

(defun efile-date-dir (efile)
  (let ((efile-date (file-written (efile-dir-file efile))))
    (setf (efile-write-time efile) efile-date)
    efile-date))

(defun eprocess-compile-efilelist (efilelist)
  (do ((efilelist efilelist (cdr efilelist))
       (maxdate 0
		(max maxdate
		     (eload-find-efile (car efilelist) nil t))))
      ((null efilelist) maxdate)))

(defun eprocess-efilelist (efilelist relevant-files relevant-directory)
  "Generates the list of files dependent on `dependent-efile'."
  (do ((efilelist efilelist (cdr efilelist))
       (maxdate 0
		(max maxdate
		     (eprocess-graph (car efilelist)
				     relevant-files relevant-directory))))
      ((null efilelist) maxdate)))

(defun eprocess-graph (efile relevant-files relevant-directory)
  "Process the dependency graph `efile' in `relevant-directory'."
  (let ((compile-efiles (efile-compile-efiles efile)))
    (let ((compile-date
	   (if compile-efiles
	       (eprocess-compile-efilelist compile-efiles)
	       nil))
	  (govern-date
	   (eprocess-efilelist (efile-governing-efiles efile)
			       relevant-files relevant-directory))
	  (flag-date (efile-flag-time efile)) 
	  ;; efile-date has side-effect of filling in full filename!
	  (efile-date (efile-date efile
				  relevant-files relevant-directory)))
      (if compile-date
	  ;; This means: we are compiling, and there are macro files.
	  ;; Load macro files, if necessary
	  (progn 
	    ;;(eload-regenerated-efiles compile-efiles relevant-directory)
	    ;; Theses are now loaded by `eprocess-compile-efilelist'.
	    (if (> compile-date efile-date)
		;; Here: must recompile, since macro-file has changed.
		(if (= govern-date 0)
		    (boxwarn "File ~S should be regenerated, because a ~
macro file has changed,
but I cannot find its source file."
			     (efile-name efile))
		    (eprocess-do efile :compile relevant-directory))
		;; otherwise: depends on governing files
		(if (> efile-date govern-date)
		    ;; No need to reprocess.
		    (if (> flag-date efile-date)
			;; Flagged after last generated: generate again
			(eprocess-do efile :flagged relevant-directory)
			;; Otherwise do nothing.
			efile-date)
		    ;; Must reprocess.
		    (eprocess-do efile :govern relevant-directory))))
	  ;; Here: no macro files to consider.
	  (if (> efile-date govern-date)
	      (if (> flag-date efile-date)
		  ;; Flagged after last generated: generate again
		  (eprocess-do efile :flagged relevant-directory)
		  ;; Otherwise do nothing.
		  efile-date)
	      ;; Must reprocess.
	      (eprocess-do efile :govern relevant-directory))))))

#|
;;; This seems inordinately expensive.  Other defn follows.
(defun existing-files (file-list true-dir)
  "Returns a list of files in `true-dir' with names in `file-list'.
Uses function DIR-LIST from file sccs."
  (remove-if-not #'(lambda (file) (member file file-list :test #'string=))
		 ;; ??? for now, since we are in :USER
		 (mapcar #'file-namestring (directory true-dir))))

;;; This second alternative did not work in CMU Common Lisp, but may
;;; be faster than the next version?

(defun existing-files (file-list true-dir)
  "Returns a list of files in `true-dir' with names in `file-list'."
  (let ((*default-pathname-defaults*
	 ;;(make-pathname :directory true-dir)
	 (parse-namestring true-dir)))
    (do ((ex-files () (let ((pf (probe-file (car files))))
			(if pf (cons (file-namestring pf) ex-files) ex-files)))
	 (files file-list (cdr files)))
	((null files) (nreverse ex-files)))))
|#

(defun existing-files (file-list true-dir)
  "Returns a list of files in `true-dir' with names in `file-list'."
  (do ((ex-files
	()
	(let ((pf (probe-file (concatenate 'string true-dir (car files)))))
	  (if pf (cons (file-namestring pf) ex-files) ex-files)))
       (files file-list (cdr files)))
      ((null files) (nreverse ex-files))))


(defun relevant-directory (efile)
  "Returns two values.  1: relevant directory for `efile' in `box'.
2: the relevant files present in that directory."
  (let ((dependency-list (dependency-list efile)))
    (retry-catch :search
      (do ((path (box-path (efile-box efile)) (cdr path))
	   (found-files nil (existing-files dependency-list true-dir))
	   (true-dir))
	  ((not (null found-files))
	   (values true-dir found-files))
	(when (null path)
	  (box-cerror (format nil "Give explicit directory name or
 (push \"dir/\" (box-path ~S)) and (retry :search)." (efile-box efile))
		  "Neither file ~S nor any of its governing files was found
in the search path for box ~S with directories ~S."
		  efile (efile-box efile) (box-path (efile-box efile)))
	  (format *query-io* "~&Directory (with quotes) : ")
	  (setq path (list (read *query-io*))))
	(setq true-dir (directorify (car path)))))))

(defun coerce-loadable-efile-p (efile)
  (if (loadable-efile-p efile)
      t
      (let ((loadable-suffix (find-first-loadable-suffix (efile-name efile))))
	(if loadable-suffix
	    (progn (setf (efile-loadable-suffix efile) loadable-suffix)
		   t)
	    nil))))

(defun eload-do (efile eload-opts)
  (if (not (coerce-loadable-efile-p efile))
      (boxwarn "Ignoring efile ~S since not considered loadable." efile)
      (let* ((loader-box (suffix-loader-box (efile-loadable-suffix efile)))
	     (process-fnspec (first (box-generates loader-box)))
	     (process-args
	      (merge-opts
	       (list (suffix-loader-key (efile-loadable-suffix efile))
		     (efile-dir-file efile))
	       eload-opts
	       (efile-local-opts efile)
	       (box-local-opts (efile-box efile)))))
	(boxmsg "Loading efile ~S as file ~S."
		efile (efile-dir-file efile))
	;;(boxdebug "Applying function ~S to ~S."
	;;process-fun process-args)
	(generator-apply process-fnspec process-args loader-box)
	;;(load (efile-dir-file efile)
	;; :verbose nil :if-does-not-exist :error :print nil)
	(setf (efile-load-time efile) (current-time)))))

(defun eload-regenerated-efile (efile relevant-directory)
  "Finally loads the regenerated file in the relevant directory, if necessary."
  ;; Hack this for now. ???.
  ;; Here we should compare the last load-time with file-write-date
  ;; to see if we need to reload.
  ;; Also reload if flagged or looking in different directory.
  (if (or (> (efile-write-time efile)
	     (efile-load-time efile))
	  (not (string= relevant-directory
			(directory-namestring (efile-dir-file efile))))
	  (> (efile-flag-time efile) (efile-load-time efile)))
      (eload-do efile (efile-eload-opts efile))
      (boxdebug "Not loading efile ~S as file ~S."
		efile (efile-dir-file efile))))

(defun eload-regenerated-efiles (efiles relevant-directory)
  (dolist (efile efiles)
    (eload-regenerated-efile efile relevant-directory)))

(defun source-opt-p (efile eload-opts)
  (cond ((member2 :source eload-opts)
	 (second (member2 :source eload-opts)))
	((member2 :source (efile-local-opts efile))
	 (second (member2 :source (efile-local-opts efile))))
	((member2 :source (box-local-opts (efile-box efile)))
	 (second (member2 :source (box-local-opts (efile-box efile)))))
	(t nil)))

(defun final-efile (efile eload-opts)
  "Determines if the .lbin or .lisp file is ultimately wanted."
  (if (and (equal *lisp-compiled-extension*
		  (pathname-type (efile-name efile)))
	   (source-opt-p efile eload-opts))
      (let ((source-efile (get-default-efile-in-box
			   (namestring
			    (merge-pathnames
			     *lisp-source-extension-pathname*
			     (efile-name efile)))
			  (efile-box efile))))
	(boxdebug "Compilation of file ~S explicitly disabled." source-efile)
	(setf (efile-local-opts source-efile)
	      (merge-opts (efile-local-opts source-efile)
			  (efile-local-opts efile)))
	source-efile)
      efile))

(defun eload-find-efile (efile eload-opts loadp)
  (declare (ignore eload-opts))
  ;; Eload-opts are really already attached to the efile during the building
  ;; of the dependency-graph.  Therefore we can ignore them here.
  (multiple-value-bind
      (relevant-directory relevant-files)
      (relevant-directory efile)
    ;; This should no longer be necessary.Thu Dec 29 22:54:45 1988, fp.
    ;;(while-connected-to relevant-directory
    ;;(boxdebug "Temporarily connecting to relevant directory ~S."
    ;;relevant-directory)
      (retry-catch :generate
	(prog1
	    ;; eprocess-graph returns the file-write-date of efile.
	    (eprocess-graph efile relevant-files relevant-directory)
	  (when loadp
	    (retry-catch :load
	      (eload-regenerated-efile efile relevant-directory)))))
      ;;)
      ))

(defun eload-efile (efile eload-opts loadp)
  ;; Next call, as a side-effect, sets up dependency-graph.
  ;; First we have to see whether we want the .lisp or the .lbin file.
  (setq efile (final-efile efile eload-opts))
  (dependency-graph efile eload-opts)
  (eload-find-efile efile eload-opts loadp))

(defun eload-efiles (efilelist)
  "Eloads a list of files.  This should be improved to build just
one dependency-graph, then do all the processing."
  (dolist (efile efilelist)
    (eload-efile efile nil t)))

(defun eload-file (file eload-opts loadp)
  "Eloads a file, given its local options `eload-opts'.  `loadp' indicates
whether to load the generated files."
  (eload-efile (loadable-efile-check file) eload-opts loadp))

(defun eload-opts (file-opts-list loadp)
  "Generates a list of files and options.  `loadp' tells whether
to load the generated files."
  (dolist (arg file-opts-list)
    (etypecase arg
      (string (eload-file arg nil loadp))
      (efile (eload-efile arg nil loadp))
      (symbol;; some keyword.  Ignore for now
       (box-error "Top-level keyword to ELOAD is illegal."))
      (list (eload-file (car arg) (cdr arg) loadp)))))

(defun eload-nogen (file-opts-list)
  (if (member-if-not #'stringp file-opts-list)
      (progn
	(box-cerror "Call EGEN instead of ELOAD."
		    "No options allowed to ELOAD.  Perhaps you meant EGEN?")
	(retry-catch :egen
	  (eload-opts file-opts-list t)))
      (load-search (mapcar #'loadable-efile-check file-opts-list))))
  

(defmacro eload (&rest eload-list)
  "Loads, but does not generate files in the arguments list."
  `(retry-catch :eload
     (eload-nogen ',eload-list)))

(defmacro egen (&rest eload-list)
  "Generates if necessary and then loads files in the arguments list."
  `(retry-catch :egen
     (eload-opts ',eload-list t)))

(defun eflag-one (efile)
  (setf (efile-flag-time efile)
	(current-time)))

(defun eflag (&rest flag-list)
  "Flags the mentioned files for regeneration."
  (dolist (file flag-list)
    (eflag-one (loadable-efile-check file))))

(defun eflag-list (filelist)
  (dolist (arg filelist)
    (etypecase arg
      (string (eflag-one (loadable-efile-check arg)))
      (efile (eflag-one arg))
      (symbol (box-error "Top-level keyword to EFORCE is illegal."))
      (list (eflag-one (loadable-efile-check (first arg)))))))

(defmacro eforce (&rest force-list)
  "Flags the files for regeneration, then regenerates and loads."
  `(retry-catch :eforce
     ;; It's important to flag all files before eloading any of them.
       (eflag-list ',force-list)
       (eload-opts ',force-list t)))

(defmacro eload-force (&rest force-list)
  "Flags all loadable files to be explicitly loaded, then loads them."
  `(retry-catch :eload-force
     (eflag-list ',force-list)
     (eload-nogen ',force-list)))

;;;
;;; Loading boxes.
;;;

(defun load-regen (efilelist)
  "This is like eload."
  (eload-efiles efilelist))

(defun load-search-final (efile)
  "Searches for and load `efile'.  No dabbling in extensions here.
Return NIL if the efile was not found."
  (let ((path (box-path (efile-box efile))))
    (retry-catch :search
      (dolist (dir path)
	(let ((existent-file (probe-file
			      (merge-pathnames (efile-name efile)
					       (directorify dir)))))
	  (when existent-file
	    (if (or (> (file-write-date existent-file)
		       (efile-load-time efile))
		    ;; ??? Next test presents a problem because of
		    ;; /usr and /usrea0!
		    (not (string= (namestring existent-file)
				  (efile-dir-file efile)))
		    (> (efile-flag-time efile) (efile-load-time efile)))
		;; if written, flagged, or looking in different directory: load
		(retry-catch :load
		  (setf (efile-dir-file efile) (namestring existent-file))
		  ;; If the ELOAD-DO fails, there is some inconsistency
		  ;; introduced here.
		  (eload-do efile nil)
			 ;; As a side-effect
		  (when (> (efile-flag-time efile) (efile-load-time efile))
		    ;; if the file was flagged now unflag, so it will not be
		    ;; accidentally regenerated after it is loaded!
		    (setf (efile-flag-time efile) 0))
		  (return-from load-search-final existent-file))
		(progn
		  (boxdebug "Not loading file ~S." existent-file)
		  (return-from load-search-final existent-file))))))
      (boxwarn "File ~S from box ~S not found in search path ~S."
	       efile (efile-box efile) path)
      nil)))

(defun load-search-one (efile)
  "Searches for and loads `efile'."
  (let ((found (load-search-final (final-efile efile nil))))
    (if (not found)
	(load-search-final (final-efile efile '(:source t)))
	found)))

(defun load-search-rec (efilelist waiting-efiles done-efiles)
  (let ((done-efiles done-efiles))
    (dolist (efile efilelist done-efiles)
      (unless (member efile done-efiles)
	(if (member efile waiting-efiles)
	    (progn (boxmsg "Circularity in load-time dependency of files.  ~
File ~S depends on itself." efile))
	    (progn
	      (setq done-efiles
		    (load-search-rec (mapcar #'loadable-efile-check (efile-compile efile))
				     (cons efile waiting-efiles)
				     done-efiles))
	      (setq done-efiles
		    (load-search-rec (mapcar #'loadable-efile-check (efile-load efile))
				     (cons efile waiting-efiles)
				     done-efiles))
	      (load-search-one efile)
	      (setq done-efiles (cons efile done-efiles))))))))

(defun load-search (efilelist)
  "Search for and load files in efilelist without regenerating.  Circularities
are checked."
  (load-search-rec efilelist nil nil))

(defun run-load-hook (box)
  "Runs the load hook for box, if box has never been loaded before."
  (when (= (box-load-time box) 0)
    (eval `(progn ,@(box-load-hook box)))))

(defun boxload-one-direct (box regenerate-p forced)
  "Directly load `box' without checking if it is forced."
  (if (or (>= (box-write-time box) (box-load-time box))
	  ;; >= because both may be 0.
	  regenerate-p forced)
      (retry-catch :box
	(if regenerate-p
	    (if (generate-legal-p box)
		(progn (boxmsg "Regenerating and Loading box ~S." box)
		       (load-regen (box-loadable-efiles box))
		       (run-load-hook box)
		       (setf (box-load-time box) (current-time)))
		(progn (boxwarn "Not regenerating box ~S because you, ~A, are not in the
list of maintainers ~A for it."
				box (user-name) (box-maintainers box))
		       (boxmsg "Loading box ~S." box)
		       (load-search (box-loadable-efiles box))
		       (run-load-hook box)
		       (setf (box-load-time box) (current-time))))
	    (progn
	      (boxmsg "Loading box ~S." box)
	      (load-search (box-loadable-efiles box))
	      (run-load-hook box)
	      (setf (box-load-time box) (current-time)))))
      (boxdebug "Box ~S already loaded." box)))

(defun boxload-one (box regenerate-p)
  "Directly load `box' without loading its needed boxes."
  (declare (special *force-boxnames*))
  (let ((forced 
	 (if (member (box-name box) *force-boxnames* :test #'string-equal)
	     (progn
	       (setq *force-boxnames*
		     (remove (box-name box) *force-boxnames*
			     :test #'string-equal))
	       t)
	     nil)))
    (boxload-one-direct box regenerate-p forced)))

(defun boxload-rec (boxnamelist waiting-boxes)
  "Loads boxes in `boxnamelist', given that `waiting-boxes' are in the queue.
`*force-boxes*' are always loaded, even if it seems like they shouldn't need
to be loaded."
  (let ((regenerate-p nil))
    (dolist (boxname boxnamelist)
      (typecase boxname
	(keyword (ctypecase boxname
		   ((member :gen :generate :regenerate)
		    (setq regenerate-p t)))) 
	(list (ctypecase (first boxname)
		((member :boot)
		 ;; Load source of box, without regeneration, and without
		 ;; looking for dependency.  Use with care!
		 (let ((box (get-existent-box (second boxname))))
		   (when (= 0 (box-load-time box))
		     (unwind-protect
			 (progn
			   (push2 :source t (box-local-opts box))
			   (boxload-one-direct box nil t))
		       (pop2 (box-local-opts box))
		       ;; To make sure, it will be regenerated later!
		       ))))))
	(t (let ((box (get-existent-box boxname)))
	     (boxdebug "~V@{.~}~S -- Needed boxes." (length waiting-boxes) box)
	     ;; Be careful!  Look up boxes when needed, because loading some
	     ;; boxes may possibly define others later in the list.
	     (if (member box waiting-boxes)
		 ;; Do not load, but issue message.
		 (boxwarn "Box circularity:  Somehow box ~S needs itself."
			  (box-name box))
		 (progn (boxload-rec (box-needs box) (cons box waiting-boxes))
			(boxdebug "~V@{.~}~S -- Loading."
				  (length waiting-boxes) box)
			(boxload-one box regenerate-p)))))))))

(defun boxload-fun (boxnamelist)
  (retry-catch :boxload
    (let ((*force-boxnames* boxnamelist))
      (declare (special *force-boxnames*))
      (boxload-rec boxnamelist nil))))

(defmacro boxload (&rest boxnamelist)
  "Loads the given boxes from left-to-right, if possible.  Circularities
are checked."
  `(boxload-fun ',boxnamelist))

(defmacro boxgen (&rest boxnamelist)
  `(boxload :gen ,@boxnamelist))

(defun boxflag-rec (boxnamelist)
  (dolist (boxname boxnamelist)
    (typecase boxname
      (keyword)				; Ignore keywords here.
      (t (let ((box (get-existent-box boxname)))
	   ;; The following is a bad hack.
	   (maphash #'(lambda (key efile)
			(declare (ignore key))
			(when (equal box (efile-box efile))
			  (boxdebug "Flagging file ~S." efile)
			  (eflag-one efile)))
		    *efiletable*))))))

(defun boxloadflag-rec (boxnamelist)
  "Flag all the loadable files in the listed boxes."
  (dolist (boxname boxnamelist)
    (typecase boxname
      (keyword)				; Ignore keywords here.
      (t (let ((box (get-existent-box boxname)))
	   ;; The following is a bad hack.
	   (mapc #'eflag-one (box-loadable-efiles box)))))))

(defmacro boxforce (&rest boxnamelist)
  `(retry-catch :boxforce
     (boxflag-rec ',boxnamelist)
     (let ((*force-boxnames* ',boxnamelist))
       (declare (special *force-boxnames*))
       (boxload-rec '(:gen ,@boxnamelist) nil))))

(defun boxload-force-fun (boxnamelist)
  (retry-catch :boxload-force
    (boxloadflag-rec boxnamelist)
    (let ((*force-boxnames* boxnamelist))
      (declare (special *force-boxnames*))
      (boxload-rec boxnamelist nil))))  

(defmacro boxload-force (&rest boxnamelist)
  `(boxload-force-fun ',boxnamelist))

(defun boxflag (&rest boxnamelist)
  (boxflag-rec boxnamelist))

;;;
;;; Loading and generating crates.
;;;

(defun crateforce-fun (cratenamelist)
  (dolist (cratename cratenamelist)
    (boxflag-rec (crate-boxes (get-existent-crate cratename))))
  (crategen-fun cratenamelist))

(defmacro crateforce (&rest cratenamelist)
  `(crateforce-fun ',cratenamelist))

(defun crategen-fun (cratenamelist)
  (dolist (cratename cratenamelist)
    (boxload-fun (cons :gen (crate-boxes (get-existent-crate cratename))))))

(defmacro crategen (&rest cratenamelist)
  `(crategen-fun ',cratenamelist))

(defun crateload-fun (cratenamelist)
  (dolist (cratename cratenamelist)
    (boxload-fun (crate-boxes (get-existent-crate cratename)))))

(defmacro crateload (&rest cratenamelist)
  `(crateload-fun ',cratenamelist))

(defun crateload-force-fun (cratenamelist)
  (dolist (cratename cratenamelist)
    (boxload-force-fun (crate-boxes (get-existent-crate cratename)))))

(defmacro crateload-force (&rest cratenamelist)
  `(crateload-force-fun ',cratenamelist))

;;;
;;; For new, incompatible versions of generating boxes,
;;; or for forcing recompilation.
;;;

(defun regenerate-with-fun (gen-boxname)
  (let ((gen-box (get-existent-box gen-boxname)))
    (maphash #'(lambda (key efile)
		 (declare (ignore key))
		 (let* ((suffix (efile-suffix efile))
			(suffix-box (if suffix (suffix-box suffix) nil)))
		   (when (equal gen-box suffix-box)
		     (eflag-one efile))))
	     *efiletable*)))

(defmacro regenerate-with (gen-boxname)
  "Flag files for regeneration which were created with box GEN-BOXNAME."
  `(regenerate-with-fun ',gen-boxname))

;;;
;;; Other useful functions.
;;;

(defun boxclear ()
  (clrhash *suffixtable*)
  (clrhash *cratetable*)
  (clrhash *boxtable*)
  (clrhash *efiletable*)
  t)
  
(defun boxinit ()
  (let ((box-dir (first (box-path (get-existent-box "TOOLS")))))
    ;; The loading will not always work - in that case you have to `load'
    ;; it yourself.  You may also have to reload any private box-definitions
    ;; you have.
    (boxclear)
    (load (merge-pathnames (directorify box-dir) "box"))
    (load (merge-pathnames (directorify box-dir) "box-defs"))))

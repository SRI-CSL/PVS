;;; -*- Mode: LISP; Syntax: Common-Lisp -*-
;;; Wed May 22 19:33:59 1991 by Mark Kantrowitz <mkant@GLINDA.OZ.CS.CMU.EDU>
;;; defsystem.lisp
 
;;; ********************************************************************
;;; Portable Mini-DefSystem ********************************************
;;; ********************************************************************

;;; This is a portable system definition facility for Common Lisp. 
;;; Though home-grown, the syntax was inspired by fond memories of the
;;; defsystem facility on Symbolics 3600's. The exhaustive lists of
;;; filename extensions for various lisps and the idea to have one
;;; "operate-on-system" function instead of separate "compile-system"
;;; and "load-system" functions were taken from Xerox Corp.'s PCL 
;;; system.

;;; This system improves on both PCL and Symbolics defsystem utilities
;;; by performing a topological sort of the graph of file-dependency 
;;; constraints. Thus, the components of the system need not be listed
;;; in any special order, because the defsystem command reorganizes them
;;; based on their constraints. It includes all the standard bells and
;;; whistles, such as not recompiling a binary file that is up to date
;;; (unless the user specifies that all files should be recompiled).

;;; Written by Mark Kantrowitz, School of Computer Science, 
;;; Carnegie Mellon University, October 1989.

;;; Copyright (c) 1989, 1990 by Mark Kantrowitz. All rights reserved.

;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      o no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      o this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 

;;; Please send bug reports, comments and suggestions to mkant@cs.cmu.edu. 
;
;;; ********************************
;;; Change Log *********************
;;; ********************************
;;;
;;; Note: Several of the fixes from 30-JAN-91 and 31-JAN-91 were done in
;;; September and October 1990, but not documented until January 1991. 
;;;
;;; hkt = Rick Taube <hkt@cm-next-8.stanford.edu>
;;; brad = Brad Miller <miller@cs.rochester.edu>
;;; toni = Anton Beschta <toni%l4@ztivax.siemens.com>
;;; bw   = Robert Wilhelm <wilhelm@rpal.rockwell.com>
;;; rs   = Ralph P. Sobek <ralph@vega.laas.fr>
;;; gi   = Gabriel Inaebnit <inaebnit@research.abb.ch>
;;; djc  = Daniel J. Clancy <clancy@cs.utexas.edu>
;;; mc   = Matthew Cornell <cornell@unix1.cs.umass.edu>
;;; ik   = Ik Su Yoo <ik@ctt.bellcore.com>
;;; gc   = Guillaume Cartier <cartier@math.uqam.ca>
;;; Thanks to Steve Strassmann <straz@media-lab.media.mit.edu> and
;;; Sean Boisen <sboisen@BBN.COM> for detailed bug reports and 
;;; miscellaneous assistance. Thanks also to Gabriel Inaebnit
;;; <inaebnit@research.abb.ch> for help with VAXLisp bugs.
;;;
;;; 05-NOV-90  hkt  Changed canonicalize-system-name to make system
;;;                 names package independent. Interns them in the
;;;                 keyword package. Thus either strings or symbols may
;;;                 be used to name systems from the user's point of view.
;;; 05-NOV-90  hkt  Added definition FIND-SYSTEM to allow OOS to
;;;                 work on systems whose definition hasn't been loaded yet.
;;; 05-NOV-90  hkt  Added definitions COMPILE-SYSTEM and LOAD-SYSTEM
;;;                 as alternates to OOS for naive users.
;;; 05-NOV-90  hkt  Shadowing-import of 'defsystem in Allegro CL 3.1 [NeXT]
;;;                 into USER package instead of import.
;;; 15-NOV-90  mk   Changed package name to "MAKE", eliminating "DEFSYSTEM"
;;;                 to avoid conflicts with allegro, symbolics packages
;;;                 named "DEFSYSTEM".
;;; 30-JAN-91  mk   Modified append-directories to work with the 
;;;                 logical-pathnames system.
;;; 30-JAN-91  mk   Append-directories now works with Sun CL4.0. Also, fixed
;;;                 bug wrt Lucid 4.0's pathnames (which changed from lcl3.0
;;;                 -- 4.0 uses a list for the directory slot, whereas
;;;                 3.0 required a string). Possible fix to symbolics bug.
;;; 30-JAN-91  mk   Defined NEW-REQUIRE to make redefinition of REQUIRE
;;;                 cleaner. Replaced all calls to REQUIRE in this file with
;;;                 calls to NEW-REQUIRE, which should avoid compiler warnings.
;;; 30-JAN-91  mk   In VAXLisp, when we redefine lisp:require, the compiler
;;;                 no longer automatically executes require forms when it
;;;                 encounters them in a file. The user can always wrap an
;;;                 (eval-when (compile load eval) ...) around the require
;;;                 form. Alternately, see commented out code near the
;;;                 redefinition of lisp:require which redefines it as a
;;;                 macro instead.
;;; 30-JAN-91  mk   Added parameter :version to operate-on-system. If it is
;;;                 a number, that number is used as part of the binary
;;;                 directory name as the place to store and load files.
;;;                 If NIL (the default), uses regular binary directory.
;;;                 If T, tries to find the most recent version of the
;;;                 binary directory.
;;; 30-JAN-91  mk   Added global variable *use-timeouts* (default: t), which
;;;                 specifies whether timeouts should be used in
;;;                 Y-OR-N-P-WAIT. This is provided for users whose lisps
;;;                 don't handle read-char-no-hang properly, so that they
;;;                 can set it to NIL to disable the timeouts. Usually the
;;;                 reason for this is the lisp is run on top of UNIX,
;;;                 which buffers input LINES (and provides input editing).
;;;                 To get around this we could always turn CBREAK mode
;;;                 on and off, but there's no way to do this in a portable
;;;                 manner.
;;; 30-JAN-91  mk   Fixed bug where in :test t mode it was actually providing
;;;                 the system, instead of faking it.
;;; 30-JAN-91  mk   Changed storage of system definitions to a hash table.
;;;                 Changed canonicalize-system-name to coerce the system
;;;                 names to uppercase strings. Since we're no longer using
;;;                 get, there's no need to intern the names as symbols,
;;;                 and strings don't have packages to cause problems.
;;;                 Added UNDEFSYSTEM, DEFINED-SYSTEMS, and DESCRIBE-SYSTEM.
;;;                 Added :delete-binaries command. 
;;; 31-JAN-91  mk   Franz Allegro CL has a defsystem in the USER package,
;;;                 so we need to do a shadowing import to avoid name
;;;                 conflicts.
;;; 31-JAN-91  mk   Fixed bug in compile-and-load-operation where it was
;;;                 only loading newly compiled files.
;;; 31-JAN-91  mk   Added :load-time slot to components to record the
;;;                 file-write-date of the binary/source file that was loaded.
;;;                 Now knows "when" (which date version) the file was loaded.
;;;                 Added keyword :minimal-load and global *minimal-load*
;;;                 to enable defsystem to avoid reloading unmodified files.
;;;                 Note that if B depends on A, but A is up to date and
;;;                 loaded and the user specified :minimal-load T, then A
;;;                 will not be loaded even if B needs to be compiled. So
;;;                 if A is an initializations file, say, then the user should
;;;                 not specify :minimal-load T.
;;; 31-JAN-91  mk   Added :load-only slot to components. If this slot is
;;;                 specified as non-NIL, skips over any attempts to compile
;;;                 the files in the component. (Loading the file satisfies
;;;                 the need to recompile.)
;;; 31-JAN-91  mk   Eliminated use of set-alist-lookup and alist-lookup,
;;;                 replacing it with hash tables. It was too much bother,
;;;                 and rather brittle too.
;;; 31-JAN-91  mk   Defined #@ macro character for use with AFS @sys
;;;                 feature simulator. #@"directory" is then synonymous
;;;                 with (afs-binary-directory "directory").
;;; 31-JAN-91  mk   Added :private-file type of module. It is similar to
;;;                 :file, but has an absolute pathname. This allows you
;;;                 to specify a different version of a file in a system
;;;                 (e.g., if you're working on the file in your home
;;;                 directory) without completely rewriting the system
;;;                 definition.
;;; 31-JAN-91  mk   Operations on systems, such as :compile and :load,
;;;                 now propagate to subsystems the system depends on
;;;                 if *operations-propagate-to-subsystems* is T (the default)
;;;                 and the systems were defined using either defsystem
;;;                 or as a :system component of another system. Thus if
;;;                 a system depends on another, it can now recompile the 
;;;                 other.
;;; 01-FEB-91  mk   Added default definitions of PROVIDE/REQUIRE/*MODULES*
;;;                 for lisps that have thrown away these definitions in
;;;                 accordance with CLtL2.
;;; 01-FEB-91  mk   Added :compile-only slot to components. Analogous to
;;;                 :load-only. If :compile-only is T, will not load the
;;;                 file on operation :compile. Either compiles or loads
;;;                 the file, but not both. In other words, compiling the
;;;                 file satisfies the demand to load it. This is useful
;;;                 for PCL defmethod and defclass definitions, which wrap  
;;;                 an (eval-when (compile load eval) ...) around the body
;;;                 of the definition -- we save time by not loading the
;;;                 compiled code, since the eval-when forces it to be
;;;                 loaded. Note that this may not be entirely safe, since
;;;                 CLtL2 has added a :load keyword to compile-file, and
;;;                 some lisps may maintain a separate environment for
;;;                 the compiler. This feature is for the person who asked
;;;                 that a :COMPILE-SATISFIES-LOAD keyword be added to
;;;                 modules. It's named :COMPILE-ONLY instead to match 
;;;                 :LOAD-ONLY.
;;; 11-FEB-91  mk   Now adds :mk-defsystem to features list, to allow
;;;                 special cased loading of defsystem if not already
;;;                 present.
;;; 19-FEB-91  duff Added filename extension for hp9000/300's running Lucid.
;;; 26-FEB-91  mk   Distinguish between toplevel systems (defined with
;;;                 defsystem) and systems defined as a :system module
;;;                 of a defsystem. The former can depend only on systems,
;;;                 while the latter can depend on anything at the same
;;;                 level.
;;; 12-MAR-91  mk   Added :subsystem component type to be a system with
;;;                 pathnames relative to its parent component.
;;; 12-MAR-91  mk   Uncommented :device :absolute for CMU pathnames, so
;;;                 that the leading slash is included.
;;; 12-MAR-91  brad Patches for Allegro 4.0.1 on Sparc. 
;;; 12-MAR-91  mk   Changed definition of format-justified-string so that
;;;                 it no longer depends on the ~<~> format directives,
;;;                 because Allegro 4.0.1 has a bug which doesn't support
;;;                 them. Anyway, the new definition is twice as fast
;;;                 and conses half as much as FORMAT.
;;; 12-MAR-91 toni  Remove nils from list in expand-component-components.
;;; 12-MAR-91 bw    If the default-package and system have the same name,
;;;                 and the package is not loaded, this could lead to
;;;                 infinite loops, so we bomb out with an error.
;;;                 Fixed bug in default packages.
;;; 13-MAR-91 mk    Added global *providing-blocks-load-propagation* to
;;;                 control whether system dependencies are loaded if they
;;;                 have already been provided.
;;; 13-MAR-91 brad  In-package is a macro in CLtL2 lisps, so we change
;;;                 the package manually in operate-on-component.
;;; 15-MAR-91 mk    Modified *central-registry* to be either a single
;;;                 directory pathname, or a list of directory pathnames
;;;                 to be checked in order.
;;; 15-MAR-91 rs    Added afs-source-directory to handle versions when
;;;                 compiling C code under lisp. Other minor changes to
;;;                 translate-version and operate-on-system.
;;; 21-MAR-91 gi    Fixed bug in defined-systems. 
;;; 22-MAR-91 mk    Replaced append-directories with new version that works
;;;                 by actually appending the directories, after massaging
;;;                 them into the proper format. This should work for all
;;;                 CLtL2-compliant lisps.
;;; 09-APR-91 djc   Missing package prefix for lp:pathname-host-type.
;;;                 Modified component-full-pathname to work for logical
;;;                 pathnames.
;;; 09-APR-91 mk    Added *dont-redefine-require* to control whether
;;;                 REQUIRE is redefined. Fixed minor bugs in redefinition
;;;                 of require.
;;; 12-APR-91 mk    (pathname-host nil) causes an error in MCL 2.0b1
;;; 12-APR-91 mc    Ported to MCL2.0b1.
;;; 16-APR-91 mk    Fixed bug in needs-loading where load-time and
;;;                 file-write-date got swapped.
;;; 16-APR-91 mk    If the component is load-only, defsystem shouldn't
;;;                 tell you that there is no binary and ask you if you
;;;                 want to load the source.  
;;; 17-APR-91 mc    Two additional operations for MCL.
;;; 21-APR-91 mk    Added feature requested by ik. *files-missing-is-an-error*
;;;                 new global variable which controls whether files (source
;;;                 and binary) missing cause a continuable error or just a
;;;                 warning.
;;; 21-APR-91 mk    Modified load-file-operation to allow compilation of source
;;;                 files during load if the binary files are old or
;;;                 non-existent. This adds a :compile-during-load keyword to
;;;                 oos, and load-system. Global *compile-during-load* sets
;;;                 the default (currently :query).
;;; 21-APR-91 mk    Modified find-system so that there is a preference for
;;;                 loading system files from disk, even if the system is
;;;                 already defined in the environment.
;;; 25-APR-91 mk    Removed load-time slot from component defstruct and added
;;;                 function COMPONENT-LOAD-TIME to store the load times in a
;;;                 hash table. This is safer than the old definition because
;;;                 it doesn't wipe out load times every time the system is
;;;                 redefined.
;;; 25-APR-91 mk    Completely rewrote load-file-operation. Fixed some bugs
;;;                 in :compile-during-load and in the behavior of defsystem
;;;                 when multiple users are compiling and loading a system
;;;                 instead of just a single user.
;;; 16-MAY-91 mk    Modified FIND-SYSTEM to do the right thing if the system
;;;                 definition file cannot be found.
;;; 16-MAY-91 mk    Added globals *source-pathname-default* and
;;;                 *binary-pathname-default* to contain default values for
;;;                 :source-pathname and :binary-pathname. For example, set
;;;                 *source-pathname-default* to "" to avoid having to type
;;;                 :source-pathname "" all the time.
;;; 27-MAY-91 mk    Fixed bug in new-append-directories where directory
;;;                 components of the form "foo4.0" would appear as "foo4",
;;;                 since pathname-name truncates the type. Changed
;;;                 pathname-name to file-namestring.
;;;  3-JUN-91 gc    Small bug in new-append-directories; replace (when
;;;                 abs-name) with (when (not (null-string abs-name)))
;;;  4-JUN-91 mk    Additional small change to new-append-directories for
;;;                 getting the device from the relative pname if the abs
;;;                 pname is "". This is to fix a small behavior in CMU CL old
;;;                 compiler. Also changed (when (not (null-string abs-name)))
;;;                 to have an (and abs-name) in there.


;
;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;; Need way to load old binaries even if source is newer.
;;;
;;; Load a system (while not loading anything already loaded)
;;; and inform the user of out of date fasls with the choice
;;; to load the old fasl or recompile and then load the new
;;; fasl?
;;; 
;;; modify compile-file-operation to handle a query keyword....
;;;
;;; Perhaps systems should keep around the file-write-date of the system
;;; definition file, to prevent excessive reloading of the system definition?
;;;
;;; load-file-operation needs to be completely reworked to simplify the
;;; logic of when files get loaded or not.
;;;
;;; Need to revamp output: Nesting and indenting verbose output doesn't
;;; seem cool, especially when output overflows the 80-column margins.
;;;
;;; Document various ways of writing a system. simple (short) form
;;; (where :components is just a list of filenames) in addition to verbose.
;;; Put documentation strings in code.
;;;
;;; :load-time for modules and systems -- maybe record the time the system
;;; was loaded/compiled here and print it in describe-system?
;;;
;;; Make it easy to define new functions that operate on a system. For 
;;; example, a function that prints out a list of files that have changed, 
;;; hardcopy-system, edit-system, etc.
;;;
;;; If a user wants to have identical systems for different lisps, do we 
;;; force the user to use logical pathnames? Or maybe we should write a 
;;; generic-pathnames package that parses any pathname format into a 
;;; uniform underlying format (i.e., pull the relevant code out of
;;; logical-pathnames.lisp and clean it up a bit).
;;;
;;;    Verify that Mac pathnames now work with append-directories.
;;;
;;; A common human error is to violate the modularization by making a file
;;; in one module depend on a file in another module, instead of making
;;; one module depend on the other. This is caught because the dependency
;;; isn't found. However, is there any way to provide a more informative
;;; error message? Probably not, especially if the system has multiple
;;; files of the same name.
;;; 
;;; For a module none of whose files needed to be compiled, have it print out
;;; "no files need recompilation".
;;; 
;;; Write a system date/time to a file? (version information) I.e., if the
;;; filesystem supports file version numbers, write an auxiliary file to
;;; the system definition file that specifies versions of the system and
;;; the version numbers of the associated files. 
;;; 
;;; Add idea of a patch directory.
;;; 
;;; In verbose printout, have it log a date/time at start and end of
;;; compilation: 
;;;     Compiling system "test" on 31-Jan-91 21:46:47 
;;;     by Defsystem version v2.0 01-FEB-91.
;;; 
;;; Define other :force options:
;;;    :query    allows user to specify that a file not normally compiled
;;;              should be. OR
;;;    :confirm  allows user to specify that a file normally compiled
;;;              shouldn't be. AND
;;; 
;;; We currently assume that compilation-load dependencies and if-changed
;;; dependencies are identical. However, in some cases this might not be
;;; true. For example, if we change a macro we have to recompile functions
;;; that depend on it (except in lisps that automatically do this, such
;;; as the new CMU Common Lisp), but not if we change a function. Splitting
;;; these apart (with appropriate defaulting) would be nice, but not worth
;;; doing immediately since it may save only a couple of file recompilations,
;;; while making defsystem much more complex than it already is. 
;;; 
;
;;; ********************************
;;; Notes **************************
;;; ********************************
;;;
;;;    DEFSYSTEM has been tested (successfully) in the following lisps:
;;;       CMU Common Lisp (M2.9 15-Aug-90, Compiler M1.8 15-Aug-90)
;;;       CMU Common Lisp (14-Dec-90 beta, Python Compiler 0.0 PMAX/Mach)
;;;       ExCL (Franz Allegro CL 3.1.12 [DEC 3100] 3/30/90)
;;;       ExCL (Franz Allegro CL 4.0.1 [SPARC])
;;;       Lucid CL (Version 2.1 6-DEC-87)
;;;       Lucid Common Lisp (3.0 [SPARC,SUN3]) 
;;;       Lucid Common Lisp (4.0 [SPARC,SUN3])
;;;       VAXLisp (v2.2) [VAX/VMS]
;;;       VAXLisp (v3.1)
;;;
;;;    DEFSYSTEM needs to be tested in the following lisps:
;;;       Symbolics Common Lisp (8.0)
;;;       Macintosh Allegro Common Lisp (1.3.2)
;;;       KCL (June 3, 1987 or later)
;;;       AKCL (1.86, June 30, 1987 or later)
;;;       TI (Release 4.1 or later)
;;;       Ibuki Common Lisp (01/01, October 15, 1987)
;;;       Golden Common Lisp (3.1 IBM-PC)
;;;       HP Common Lisp (same as Lucid?)
;;;       Procyon Common Lisp

;
;;; ********************************************************************
;;; How to Use this System *********************************************
;;; ********************************************************************

;;; To use this system,
;;; 1. If you want to have a central registry of system definitions, 
;;;    modify the value of the variable *central-registry* below.
;;; 2. Load this file (defsystem.lisp) in either source or compiled form,
;;; 3. Load the file containing the "defsystem" definition of your system,
;;; 4. Use the function "operate-on-system" to do things to your system.

;;; For more information, see the documentation and examples in defsystem.text.

;;; ****************************************************************
;;; Lisp Code ******************************************************
;;; ****************************************************************

;;; ********************************
;;; Massage CLtL2 onto *features* **
;;; ********************************
;;; Let's be smart about CLtL2 compatible Lisps:
#+(or (and :excl :allegro-v4.0) :mcl)
(eval-when (compile load eval)
  (pushnew :cltl2 *features*))

;;; ********************************
;;; Provide/Require/*modules* ******
;;; ********************************

;;; Since CLtL2 has dropped require and provide from the language, some
;;; lisps may not have the functions PROVIDE and REQUIRE and the
;;; global *MODULES*. So if lisp::provide and user::provide are not
;;; defined, we define our own.

;;; Hmmm. CMU CL old compiler gives bogus warnings here about functions
;;; and variables not being declared or bound, apparently because it
;;; sees that (or (fboundp 'lisp::require) (fboundp 'user::require)) returns
;;; T, so it doesn't really bother when compiling the body of the unless.
;;; The new compiler does this properly, so I'm not going to bother
;;; working around this.

#-(or gcl (and :CMU (not :new-compiler)) :vms :mcl :harlequin-common-lisp)
(eval-when (compile load eval)
  (unless (or (fboundp 'lisp::require) (fboundp 'user::require)
	      #+(and :excl :allegro-v4.0) (fboundp 'cltl1::require))
    (in-package "LISP")
    (export '(*modules* provide require))

    ;; Documentation strings taken almost literally from CLtL1.
  
    (defvar *MODULES* ()
      "List of names of the modules that have been loaded into Lisp so far.
     It is used by PROVIDE and REQUIRE.")

    ;; We provide two different ways to define modules. The default way
    ;; is to put either a source or binary file with the same name
    ;; as the module in the library directory. The other way is to define
    ;; the list of files in the module with defmodule.

    ;; The directory listed in *library* is implementation dependent,
    ;; and is intended to be used by Lisp manufacturers as a place to
    ;; store their implementation dependent packages. 
    ;; Lisp users should use systems and *central-registry* to store
    ;; their packages -- it is intended that *central-registry* is
    ;; set by the user, while *library* is set by the lisp.

    (defvar *library* nil		; "/usr/local/lisp/Modules/"
      "Directory within the file system containing files, where the name
     of a file is the same as the name of the module it contains.")

    (defun canonicalize-module-name (name)
      ;; if symbol, string-downcase the printrep to make nicer filenames.
      (if (stringp name) name (string-downcase (string name))))

    (defvar *module-files* (make-hash-table :test #'equal)
      "Hash table mapping from module names to list of files for the
     module. REQUIRE loads these files in order.")
    (defmacro defmodule (name &rest files)
      "Defines a module NAME to load the specified FILES in order."
      `(setf (gethash (canonicalize-module-name ,name) *module-files*)
	     ',files))
    (defun module-files (name)
      (gethash name *module-files*))

    (defun PROVIDE (name)
      "Adds a new module name to the list of modules maintained in the
     variable *modules*, thereby indicating that the module has been 
     loaded. Name may be a string or symbol -- strings are case-senstive,
     while symbols are treated like lowercase strings. Returns T if
     NAME was not already present, NIL otherwise."
      (let ((module (canonicalize-module-name name)))
	(unless (find module *modules* :test #'string=)
	  ;; Module not present. Add it and return T to signify that it 
	  ;; was added.
	  (push module *modules*)
	  t)))

    (defun REQUIRE (name &optional pathname)
      "Tests whether a module is already present. If the module is not
     present, loads the appropriate file or set of files. The pathname
     argument, if present, is a single pathname or list of pathnames
     whose files are to be loaded in order, left to right. If the
     pathname is nil, the system first checks if a module was defined
     using defmodule and uses the pathnames so defined. If that fails,
     it looks in the library directory for a file with name the same
     as that of the module. Returns T if it loads the module."
      (let ((module (canonicalize-module-name name)))
	(unless (find module *modules* :test #'string=)
	  ;; Module is not already present.
	  (when (and pathname (not (listp pathname)))
	    ;; If there's a pathname or pathnames, ensure that it's a list.
	    (setf pathname (list pathname)))
	  (unless pathname 
	    ;; If there's no pathname, try for a defmodule definition.
	    (setf pathname (module-files module)))
	  (unless pathname
	    ;; If there's still no pathname, try the library directory.
	    (when *library*
	      (setf pathname (concatenate 'string *library* module))
	      ;; Test if the file exists.
	      ;; We assume that the lisp will default the file type 
	      ;; appropriately. If it doesn't, use #+".fasl" or some
	      ;; such in the concatenate form above.
	      (if (probe-file pathname)
		  ;; If it exists, ensure we've got a list
		  (setf pathname (list pathname))
		  ;; If the library file doesn't exist, we don't want
		  ;; a load error.
		  (setf pathname nil))))
	  ;; Now that we've got the list of pathnames, let's load them.
	  (dolist (pname pathname T)
	    (load pname :verbose nil)))))))

;;; ********************************
;;; Set up Package *****************
;;; ********************************


;;; Unfortunately, lots of lisps have their own defsystems, some more
;;; primitive than others, all uncompatible, and all in the DEFSYSTEM
;;; package. To avoid name conflicts, we've decided to name this the
;;; MAKE package. A nice side-effect is that the short nickname
;;; MK is my initials.

#-(or cltl2 gcl)
(in-package "MAKE" :nicknames '("MK"))

#+gcl
(in-package "MAKE")

#+gcl
(import '(operate-on-system oos afs-binary-directory afs-source-directory
			 files-in-system) "USER")
#+gcl
(import '(defsystem compile-system load-system) "USER")

#+gcl
(export '(operate-on-system oos afs-binary-directory afs-source-directory
			    files-in-system))
#+gcl
(export '(defsystem compile-system load-system))
#+gcl
(export '(*central-registry* *bin-subdir* 
			     machine-type-translation software-type-translation
					;require
			     allegro-make-system-fasl 
			     files-which-need-compilation  
			     undefsystem
			     defined-systems
			     describe-system
			     *defsystem-version*
			     *compile-during-load*
			     *minimal-load*
			     *dont-redefine-require*
			     *files-missing-is-an-error*
			     *reload-systems-from-disk*
			     *source-pathname-default*
			     *binary-pathname-default*
			     ))

;;; For CLtL2 compatible lisps...
#+(and :excl :allegro-v4.0 :cltl2)
(defpackage "MAKE" (:nicknames "MK") (:use "COMMON-LISP") 
  (:import-from cltl1 *modules* provide require))
#+:mcl                                  
(defpackage "MAKE" (:nicknames "MK") (:use "COMMON-LISP") 
  (:import-from ccl *modules* provide require))
#+(and :cltl2 (not (or (and :excl :allegro-v4.0) :mcl)))   
(unless (find-package "MAKE") 
  (make-package "MAKE" :nicknames '("MK") :use '("COMMON-LISP")))

#+:cltl2
(in-package "MAKE")

#+(and :excl :allegro-v4.0 :cltl2)
(cltl1:provide 'make)
#+:mcl
(ccl:provide 'make)
#+(and :cltl2 (not (or (and :excl :allegro-v4.0) :mcl)))
(provide 'make)
#-:cltl2
(provide 'make)

(pushnew :mk-defsystem *features*)

#-gcl
(eval-when (compile load eval)
  (defvar *special-exports*
    '(defsystem compile-system load-system))
   (defvar *exports*
     '(operate-on-system oos afs-binary-directory afs-source-directory
			 files-in-system))

   (defvar *other-exports* 
     '(*central-registry* *bin-subdir* 
			  machine-type-translation software-type-translation
					;require
			  allegro-make-system-fasl 
			  files-which-need-compilation  
			  undefsystem
			  defined-systems
			  describe-system
			  *defsystem-version*
			  *compile-during-load*
			  *minimal-load*
			  *dont-redefine-require*
			  *files-missing-is-an-error*
			  *reload-systems-from-disk*
			  *source-pathname-default*
			  *binary-pathname-default*
			  )))

;;; The external interface consists of *exports* and *other-exports*.
#-gcl
(eval-when (compile load eval)
   (export *exports*)
   (export *special-exports*)
   (export *other-exports*))

;;; We import these symbols into the USER package to make them
;;; easier to use. Since some lisps have already defined defsystem
;;; in the user package, we may have to shadowing-import it.
#-(OR :CMU :CCL :ALLEGRO :EXCL :GCL)
(eval-when (compile load eval)
  (import *exports* #-:cltl2 "USER" #+:cltl2 "COMMON-LISP-USER")
  (import *special-exports* #-:cltl2 "USER" #+:cltl2 "COMMON-LISP-USER"))
#+(OR :CMU :CCL :ALLEGRO :EXCL)
(eval-when (compile load eval)
  (import *exports* #-:cltl2 "USER" #+:cltl2 "COMMON-LISP-USER")
  (shadowing-import *special-exports* 
		    #-:cltl2 "USER" 
		    #+:cltl2 "COMMON-LISP-USER"))

#-PCL(when (find-package "PCL") 
       (pushnew :pcl *modules*)
       (pushnew :pcl *features*))

;;; ********************************
;;; Defsystem Version **************
;;; ********************************
(defvar *defsystem-version* "v2.4 22-MAY-91"
  "Current version number/date for Defsystem.")

;;; ********************************
;;; Customizable System Parameters *
;;; ********************************

(defvar *dont-redefine-require* nil
  "If T, prevents the redefinition of REQUIRE. This is useful for
   lisps that treat REQUIRE specially in the compiler.")

;;; Change this variable to set up the location of a central
;;; repository for system definitions if you want one.
(defvar *central-registry* '() 
  "Central directory of system definitions. May be either a single
   directory pathname, or a list of directory pathnames to be checked
   after the local directory.")
;;; (setq *central-registry* "/usr/local/lisp/Registry/")

(defvar *bin-subdir* ".bin/"
  "The subdirectory of an AFS directory where the binaries are really kept.")

;;; These variables set up defaults for operate-on-system, and are used 
;;; for communication in lieu of parameter passing. Yes, this is bad,
;;; but it keeps the interface small. Also, in the case of the -if-no-binary
;;; variables, parameter passing would require multiple value returns
;;; from some functions. Why make life complicated?
(defvar *tell-user-when-done* nil
  "If T, system will print ...DONE at the end of an operation")
(defvar *oos-verbose* nil 
  "Operate on System Verbose Mode")
(defvar *oos-test* nil 
  "Operate on System Test Mode")
(defvar *load-source-if-no-binary* nil
  "If T, system will try loading the source if the binary is missing")
(defvar *bother-user-if-no-binary* t
  "If T, the system will ask the user whether to load the source if 
   the binary is missing")
(defvar *load-source-instead-of-binary* nil
  "If T, the system will load the source file instead of the binary.")
(defvar *compile-during-load* :query
  "If T, the system will compile source files during load if the
   binary file is missing. If :query, it will ask the user for
   permission first.")
(defvar *minimal-load* nil
  "If T, the system tries to avoid reloading files that were already loaded
   and up to date.")

(defvar *files-missing-is-an-error* t
  "If both the source and binary files are missing, signal a continuable 
   error instead of just a warning.")

(defvar *operations-propagate-to-subsystems* t
  "If T, operations like :COMPILE and :LOAD propagate to subsystems
   of a system that are defined either using a component-type of :system
   or by another defsystem form.")

;;; Particular to CMULisp
(defvar *compile-error-file-type* "err"
  "File type of compilation error file in cmulisp")
(defvar *cmu-errors-to-terminal* t
  "Argument to :errors-to-terminal in compile-file in cmulisp")
(defvar *cmu-errors-to-file* t
  "If T, cmulisp will write an error file during compilation")

;;; ********************************
;;; Global Variables ***************
;;; ********************************

;;; Massage people's *features* into better shape.
(eval-when (compile load eval)  
  (dolist (feature *features*)
    (when (and (symbolp feature)   ; 3600
               (equal (symbol-name feature) "CMU"))
      (pushnew :CMU *features*)))
  
  #+Lucid
  (when (search "IBM RT PC" (machine-type))
    (pushnew :ibm-rt-pc *features*))
  )

;;; *filename-extensions* is a cons of the source and binary extensions.
(defvar *filename-extensions*
  (car '(#+(and Symbolics Lispm)              ("lisp" . "bin")
         #+(and dec common vax (not ultrix))  ("LSP"  . "FAS")
         #+(and dec common vax ultrix)        ("lsp"  . "fas")
         #+KCL                                ("lsp"  . "o")
         #+IBCL                               ("lsp"  . "o")
         #+Xerox                              ("lisp" . "dfasl")
	 ;; the entry for (and lucid hp300) must precede
	 ;; that of (and lucid mc68000) for hp9000/300's running lucid,
	 ;; since *features* on hp9000/300's also include the :mc68000
	 ;; feature.
	 #+(and lucid hp300)                  ("lisp" . "6bin")
         #+(and Lucid MC68000)                ("lisp" . "lbin")
         #+(and Lucid Vax)                    ("lisp" . "vbin")   
         #+(and Lucid Prime)                  ("lisp" . "pbin")
         #+(and Lucid SUNRise)                ("lisp" . "sbin")
         #+(and Lucid SPARC)                  ("lisp" . "sbin")
         #+(and Lucid :IBM-RT-PC)              ("lisp" . "bbin")
	 ;; PA is Precision Architecture, HP's 9000/800 RISC cpu
	 #+(and Lucid PA)                    ("lisp" . "hbin")   
         #+excl                               ("cl"   . "fasl")
         #+:CMU                               ("lisp" . "fasl")
	 #+PRIME                              ("lisp" . "pbin")
         #+HP                                 ("l"    . "b")
         #+TI ("lisp" . #.(string (si::local-binary-file-type)))
         #+:gclisp                            ("LSP"  . "F2S")
         #+pyramid                            ("clisp" . "o")
         #+:coral                             ("lisp" . "fasl")
	 ;; Harlequin LispWorks on Mips M2000
	 #+(and :mips :lispworks) 	      ("lisp" . "mfasl")
         
         ;; Otherwise,
         ("lisp" . "lbin")))
  "Filename extensions for Common Lisp. A cons of the form
   (Source-Extension . Binary-Extension). If the system is 
   unknown (as in *features* not known), defaults to lisp and lbin.")

;;; There is no real support for this variable being nil, so don't change it.
;;; Note that in any event, the toplevel system (defined with defsystem)
;;; will have its dependencies delayed. Not having dependencies delayed
;;; might be useful if we define several systems within one defsystem.
(defvar *system-dependencies-delayed* t 
  "If T, system dependencies are expanded at run time")

;;; Replace this with consp, dammit!
(defun non-empty-listp (list)
  (and list (listp list)))

;;; ********************************
;;; Component Operation Definition *
;;; ********************************
(defvar *component-operations* (make-hash-table :test #'equal)
  "Hash table of (operation-name function) pairs.")
(defun component-operation (name &optional operation)
  (if operation
      (setf (gethash name *component-operations*) operation)
      (gethash name *component-operations*)))

;;; ********************************
;;; AFS @sys immitator *************
;;; ********************************

;;; mc 11-Apr-91: Bashes MCL's point reader, so commented out.
#-:mcl 
(eval-when (compile load eval)
  ;; Define #@"foo" as a shorthand for (afs-binary-directory "foo").
  ;; For example,
  ;;    <cl> #@"foo"
  ;;    "foo/.bin/rt_mach/"
  (set-dispatch-macro-character 
   #\# #\@ 
   #'(lambda (stream char arg)
       (declare (ignore char arg))
       `(afs-binary-directory ',(read stream t nil t)))))

(defun afs-binary-directory (root-directory)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :binary-pathname in defsystem. For example,
  ;; :binary-pathname (afs-binary-directory "scanner/")
  (let ((machine (machine-type-translation (machine-type)))
	(software (software-type-translation (software-type))))
    ;; pmax_mach rt_mach sun3_35 sun3_mach vax_mach
    (setq root-directory (namestring root-directory))
    (setq root-directory (ensure-trailing-slash root-directory))
    (format nil "~A~@[~A~]~@[~A/~]" 
	    root-directory
	    *bin-subdir*
	    (afs-component machine software))))

(defun afs-source-directory (root-directory &optional version-flag)
  ;; Function for obtaining the directory AFS's @sys feature would have
  ;; chosen when we're not in AFS. This function is useful as the argument
  ;; to :source-pathname in defsystem.
  (setq root-directory (namestring root-directory))
  (setq root-directory (ensure-trailing-slash root-directory))
  (format nil "~A~@[~A/~]" 
          root-directory
          (and version-flag (translate-version *version*))))

(defun null-string (s)
  (string-equal s ""))

(defun ensure-trailing-slash (dir)
  (if (and dir 
	   (not (null-string dir))
	   (not (char= (char dir
			     (1- (length dir)))
		       #\/)))
      (concatenate 'string dir "/")
      dir))

(defun afs-component (machine software)
  (format nil "~@[~A~]~@[_~A~]" 
	    machine 
	    (or software "mach")))

(defvar *machine-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the machine-type")
(defun machine-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *machine-type-alist*) operation)
      (gethash (string-upcase name) *machine-type-alist*)))

(machine-type-translation "IBM RT PC"   "rt")
(machine-type-translation "DEC 3100"    "pmax")
(machine-type-translation "DEC VAX-11"  "vax")
(machine-type-translation "Sun3"        "sun3")
(machine-type-translation "Sun-4"       "sun4")
#+(and :lucid :sun :mc68000)
(machine-type-translation "unknown"     "sun3")
 

(defvar *software-type-alist* (make-hash-table :test #'equal)
  "Hash table for retrieving the software-type")
(defun software-type-translation (name &optional operation)
  (if operation
      (setf (gethash (string-upcase name) *software-type-alist*) operation)
      (gethash (string-upcase name) *software-type-alist*)))

(software-type-translation "BSD UNIX" "mach") ; "unix"
(software-type-translation "Ultrix" "mach") ; "ultrix"
(software-type-translation "SunOS" "SunOS")
(software-type-translation "MACH/4.3BSD" "mach")
#+:lucid
(software-type-translation "Unix" 
			   #+:lcl4.0 "4.0"
			   #+(and :lcl3.0 (not :lcl4.0)) "3.0")

;;; ********************************
;;; System Names *******************
;;; ********************************
(defun canonicalize-system-name (name)
  ;; Originally we were storing systems using GET. This meant that the
  ;; name of a system had to be a symbol, so we interned the symbols
  ;; in the keyword package to avoid package dependencies. Now that we're
  ;; storing the systems in a hash table, we've switched to using strings.
  ;; Since the hash table is case sensitive, we use uppercase strings.
  ;; (Names of modules and files may be symbols or strings.)
  #|(if (keywordp name)
      name
      (intern (string-upcase (string name)) "KEYWORD"))|#
  (if (stringp name) name (string-upcase (string name))))

(defvar *defined-systems* (make-hash-table :test #'equal)
  "Hash table containing the definitions of all known systems.")

(defun get-system (name)
  "Returns the definition of the system named NAME."
  (gethash (canonicalize-system-name name) *defined-systems*))

(defsetf get-system (name) (value)
  `(setf (gethash (canonicalize-system-name ,name) *defined-systems*) ,value))

(defun undefsystem (name)
  "Removes the definition of the system named NAME."
  (setf (get-system name) nil))

(defun defined-systems ()
  "Returns a list of defined systems."
  (let ((result nil))
    (maphash #'(lambda (key value)
		 (declare (ignore key))
		 (push value result))
	     *defined-systems*)
    result))

;;; ********************************
;;; Directory Pathname Hacking *****
;;; ********************************

;;; Unix example: An absolute directory starts with / while a 
;;; relative directory doesn't. A directory ends with /, while
;;; a file's pathname doesn't. This is important 'cause
;;; (pathname-directory "foo/bar") will return "foo" and not "foo/".

;;; I haven't been able to test the fix to the problem with symbolics
;;; hosts. Essentially, append-directories seems to have been tacking
;;; the default host onto the front of the pathname (e.g., mk::source-pathname
;;; gets a "B:" on front) and this overrides the :host specified in the
;;; component. The value of :host should override that specified in
;;; the :source-pathname and the default file server. If this doesn't
;;; fix things, specifying the host in the root pathname "F:>root-dir>"
;;; may be a good workaround.

;;; Need to verify that merging of pathnames where modules are located
;;; on different devices (in VMS-based VAXLisp) now works.

;;; Merge-pathnames works for VMS systems. In VMS systems, the directory
;;; part is enclosed in square brackets, e.g.,
;;; 	"[root.child.child_child]" or "[root.][child.][child_child]"
;;; To concatenate directories merge-pathnames works as follows:
;;; 	(merge-pathnames "" "[root]")               ==> "[root]"
;;; 	(merge-pathnames "[root.]" "[son]file.ext") ==> "[root.son]file.ext"
;;; 	(merge-pathnames "[root.]file.ext" "[son]") ==> "[root.son]file.ext"
;;; 	(merge-pathnames "[root]file.ext" "[son]")  ==> "[root]file.ext"
;;; Thus the problem with the #-VMS code was that it was merging x y into
;;; [[x]][y] instead of [x][y] or [x]y. 

;;; Miscellaneous notes:
;;;   On GCLisp, the following are equivalent:
;;;       "\\root\\subdir\\BAZ"
;;;       "/root/subdir/BAZ"
;;;   On VAXLisp, the following are equivalent:
;;;       "[root.subdir]BAZ"
;;;       "[root.][subdir]BAZ"
;;; Use #+:vaxlisp for VAXLisp 3.0, #+(and vms dec common vax) for v2.2

(defun new-append-directories (absolute-dir relative-dir)
  ;; Version of append-directories for CLtL2-compliant lisps. In particular,
  ;; they must conform to section 23.1.3 "Structured Directories". We are
  ;; willing to fix minor aberations in this function, but not major ones.
  ;; Tested in Allegro CL 4.0 [SPARC], Allegro CL 3.1.12 [DEC 3100], 
  ;; CMU CL old and new compilers, Lucid 3.0, Lucid 4.0.
  (setf absolute-dir (or absolute-dir "")
	relative-dir (or relative-dir ""))
  (let* ((abs-dir (pathname absolute-dir))
	 (rel-dir (pathname relative-dir))
	 (host (pathname-host abs-dir))
	 (device (if (null-string absolute-dir) ; fix for CMU CL old compiler
		     (pathname-device rel-dir)
		   (pathname-device abs-dir)))
	 (abs-directory (coerce (pathname-directory abs-dir) 'list))
	 (abs-keyword (when (keywordp (car abs-directory))
			(pop abs-directory)))
	 (abs-name (file-namestring abs-dir)) ; was pathname-name
	 (rel-directory (coerce (pathname-directory rel-dir) 'list))
	 (rel-keyword (when (keywordp (car rel-directory))
			(pop rel-directory)))
	 (rel-file (file-namestring rel-dir))
	 (directory nil))
    ;; Allegro v4.0 parses "/foo" into :directory '(:absolute :root)
    ;; and filename "foo". The namestring of a pathname with 
    ;; directory '(:absolute :root "foo") ignores everything after the
    ;; :root.
    #+:allegro-v4.0(when (eq (car abs-directory) :root) (pop abs-directory))
    #+:allegro-v4.0(when (eq (car rel-directory) :root) (pop rel-directory))
    (when (and abs-name (not (null-string abs-name))) ; was abs-name
      (cond ((and (null abs-directory) (null abs-keyword))
	     #-:lucid (setf abs-keyword :relative)
	     (setf abs-directory (list abs-name)))
	    (t
	     (setf abs-directory (append abs-directory (list abs-name))))))
    (when (and (null abs-directory) (null abs-keyword) rel-keyword)
      (setf abs-keyword rel-keyword))
    (setf directory (append abs-directory rel-directory))
    (when abs-keyword (setf directory (cons abs-keyword directory)))
    (namestring 
     (make-pathname :host host
		    :device device
		    :directory #-:cmu directory
		               #+:cmu (coerce directory 'simple-vector)
		    :name rel-file))))

(defvar *append-dirs-tests* 
  '("~/foo/" "baz/bar.lisp"
     "~/foo" "baz/bar.lisp"
     "/foo/bar/" "baz/barf.lisp"
     "/foo/bar/" "/baz/barf.lisp"
     "foo/bar/" "baz/barf.lisp"
     "foo/bar" "baz/barf.lisp"
     "foo/bar" "/baz/barf.lisp"
     "foo/bar/" "/baz/barf.lisp"
     "/foo/bar/" nil
     "foo/bar/" nil
     "foo/bar" nil
     nil "baz/barf.lisp"
     nil nil))

(defun test-new-append-directories (&optional (test-dirs *append-dirs-tests*))
  (do* ((dir-list test-dirs (cddr dir-list))
	(abs-dir (car dir-list) (car dir-list))
	(rel-dir (cadr dir-list) (cadr dir-list)))
      ((null dir-list) (values))
    (format t "~&ABS: ~S ~18TREL: ~S ~41TResult: ~S"
	    abs-dir rel-dir (new-append-directories abs-dir rel-dir))))

#|
<cl> (test-new-append-directories) 

ABS: "~/foo/"     REL: "baz/bar.lisp"    Result: "/usr0/mkant/foo/baz/bar.lisp"
ABS: "~/foo"      REL: "baz/bar.lisp"    Result: "/usr0/mkant/foo/baz/bar.lisp"
ABS: "/foo/bar/"  REL: "baz/barf.lisp"   Result: "/foo/bar/baz/barf.lisp"
ABS: "/foo/bar/"  REL: "/baz/barf.lisp"  Result: "/foo/bar/baz/barf.lisp"
ABS: "foo/bar/"   REL: "baz/barf.lisp"   Result: "foo/bar/baz/barf.lisp"
ABS: "foo/bar"    REL: "baz/barf.lisp"   Result: "foo/bar/baz/barf.lisp"
ABS: "foo/bar"    REL: "/baz/barf.lisp"  Result: "foo/bar/baz/barf.lisp"
ABS: "foo/bar/"   REL: "/baz/barf.lisp"  Result: "foo/bar/baz/barf.lisp"
ABS: "/foo/bar/"  REL: NIL               Result: "/foo/bar/"
ABS: "foo/bar/"   REL: NIL               Result: "foo/bar/"
ABS: "foo/bar"    REL: NIL               Result: "foo/bar/"
ABS: NIL          REL: "baz/barf.lisp"   Result: "baz/barf.lisp"
ABS: NIL          REL: NIL               Result: ""

|#

(defun append-directories (absolute-directory relative-directory)
  "There is no CL primitive for tacking a subdirectory onto a directory.
   We need such a function because defsystem has both absolute and
   relative pathnames in the modules. This is a somewhat ugly hack which
   seems to work most of the time. We assume that ABSOLUTE-DIRECTORY
   is a directory, with no filename stuck on the end. Relative-directory,
   however, may have a filename stuck on the end."
  (when (or absolute-directory relative-directory)
    (cond 
     #+:logical-pathnames-mk
     ((eq (lp:pathname-host-type absolute-directory) :logical)
      ;; For use with logical pathnames package.
      (lp:append-logical-directories absolute-directory relative-directory))
     (t
      ;; In VMS, merge-pathnames actually does what we want!!!
      #+:VMS(namestring (merge-pathnames (or absolute-directory "")
					 (or relative-directory "")))
      ;; Cross your fingers and pray.
      #-:VMS
      (new-append-directories absolute-directory relative-directory)))))

#|
(defun append-directories (absolute-directory relative-directory)
  "There is no CL primitive for tacking a subdirectory onto a directory.
   We need such a function because defsystem has both absolute and
   relative pathnames in the modules. This is a very gross hack which
   seems to work most of the time. We assume that ABSOLUTE-DIRECTORY
   is a directory, with no filename stuck on the end. Relative-directory,
   however, may have a filename stuck on the end. We assume that 
   if we do a (make-pathname :directory abs-directory :name rel-directory)
   it will do what we want. The #+ and #-'s that appear before this
   form are used to massage abs-directory and rel-directory into a
   format acceptable to make-pathname in the particular lisp."
  (when (or absolute-directory relative-directory)
    (cond 
     #+:logical-pathnames-mk
     ((eq (pathname-host-type absolute-directory) :logical)
      ;; For use with logical pathnames package.
      (lp::append-logical-directories absolute-directory relative-directory))
     (t
      ;; Allegro CL barfs if abs-dir is "", so we replace it with NIL.
      #+:ExCL(when (and (stringp absolute-directory)
			(null-string absolute-directory))
	       (setq absolute-directory nil))
      ;; CMU CL needs a / at the end of absolute directory, so we
      ;; coerce abs-dir to a namestring and then check the last character
      ;; of the namestring. An alternate method of doing this might
      ;; be to break the directory into components, cons :absolute or
      ;; :relative on the front, and hand that off to make-pathname.
      #+:CMU(when (pathnamep absolute-directory) 
	      (setq absolute-directory (namestring absolute-directory)))
      #+:CMU(when (and absolute-directory 
		       (not (null-string absolute-directory))
		       (not (char= (char absolute-directory
					 (1- (length absolute-directory)))
				   #\/)))
	      (setq absolute-directory 
		    (concatenate 'string absolute-directory "/")))
      #+:CMU(when (pathnamep relative-directory) 
	      (setq relative-directory (namestring relative-directory)))
      ;; In VMS, merge-pathnames actually does what we want!!!
      #+:VMS(namestring (merge-pathnames (or absolute-directory "")
					 (or relative-directory "")))
      ;; For Sun Common Lisp 4.0, which is the same as Lucid 4.0.
      ;; For this one, we need to break the string up into components,
      ;; and tack a :ROOT on the front.
      ;; The :lucid probably should be removed below for it to work properly
      ;; in Lucid 3.0.
      #+(or (and (not :excl) :SUN) :lcl4.0 :lucid)
      (namestring (make-pathname 
		   :directory (cons :ROOT
				    (parse-slashed-pathname
				     (merge-pathnames absolute-directory)))
				 :name relative-directory))
      #|(namestring (make-pathname
		   :directory (list :ROOT (or absolute-directory ""))
		   :name relative-directory))|#
      ;; Cross your fingers and pray.
      #-(or :VMS (and (not :excl) :SUN) :lcl4.0 :lucid)
      (namestring (make-pathname :directory absolute-directory
				 #+:cmu :device #+:cmu :absolute
				 #+:symbolics :host #+:symbolics nil
				 :name relative-directory))))))

(defun parse-with-delimiter (line &optional (delim #\/))
  (let ((pos (position delim line)))
    (cond (pos
           (cons (subseq line 0 pos)
                 (parse-with-delimiter (subseq line (1+ pos)) delim)))
          (t
           (list line)))))

(defun parse-slashed-pathname (directory)
  "Parses strings like \"/usr/mkant/Public/\" into a list of
   the directory components: '(\"usr\" \"mkant\" \"Public\"),
   with null components (\"\") removed."
  (if directory
      (remove-if #'(lambda (string) (string-equal string ""))
		 (parse-with-delimiter directory #\/))
      (list "")))
|#

#|
;;; This was a try at appending a subdirectory onto a directory.
;;; It failed. We're keeping this around to prevent future mistakes
;;; of a similar sort.
(defun merge-directories (absolute-directory relative-directory)
  ;; replace concatenate with something more intelligent
  ;; i.e., concatenation won't work with some directories.
  ;; it should also behave well if the parent directory 
  ;; has a filename at the end, or if the relative-directory ain't relative
  (when absolute-directory 
    (setq absolute-directory (pathname-directory absolute-directory)))
  (concatenate 'string 
	       (or absolute-directory "")
	       (or relative-directory "")))
|#



#|
<cl> (defun d (d n) (namestring (make-pathname :directory d :name n)))

D
<cl> (d "~/foo/" "baz/bar.lisp")
"/usr0/mkant/foo/baz/bar.lisp" 

<cl> (d "~/foo" "baz/bar.lisp")
"/usr0/mkant/foo/baz/bar.lisp" 

<cl> (d "/foo/bar/" "baz/barf.lisp")
"/foo/bar/baz/barf.lisp"

<cl> (d "foo/bar/" "baz/barf.lisp")
"foo/bar/baz/barf.lisp"

<cl> (d "foo/bar" "baz/barf.lisp")
"foo/bar/baz/barf.lisp"

<cl> (d "foo/bar" "/baz/barf.lisp")
"foo/bar//baz/barf.lisp"

<cl> (d "foo/bar" nil)
"foo/bar/"

<cl> (d nil "baz/barf.lisp")
"baz/barf.lisp"

<cl> (d nil nil)
""

|#


(defun namestring-or-nil (pathname)
  (when pathname
    (namestring pathname)))

(defun new-file-type (pathname type)
  (make-pathname
   :host (pathname-host pathname)
   :device (pathname-device pathname)
   :directory (pathname-directory pathname)
   :name (pathname-name pathname)
   :type type
   :version (pathname-version pathname)))



;;; ********************************
;;; Component Defstruct ************
;;; ********************************
(defvar *source-pathname-default* nil
  "Default value of :source-pathname keyword in DEFSYSTEM. Set this to
   \"\" to avoid having to type :source-pathname \"\" all the time.")
(defvar *binary-pathname-default* nil
  "Default value of :binary-pathname keyword in DEFSYSTEM.")

(defstruct (topological-sort-node (:conc-name topsort-))
  color
  time)

(defstruct (component (:include topological-sort-node)
                      (:print-function print-component))
  type                ; :defsystem, :system, :subsystem, :module, :file, or :private-file
  name                ; a symbol or string
  indent              ; number of characters of indent in verbose output to the user.
  host                ; the pathname host (i.e., "/../a")
  device              ; the pathname device
  source-root-dir
  ;; relative or absolute (starts with "/"), directory or file (ends with "/")
  (source-pathname *source-pathname-default*)
  source-extension    ; a string, e.g., "lisp". If nil, uses default for machine-type
  (binary-pathname *binary-pathname-default*)
  binary-root-dir
  binary-extension    ; a string, e.g., "fasl". If nil, uses default for machine-type
  package             ; package for use-package
  components          ; a list of components comprising this component's definition
  depends-on          ; a list of the components this one depends on. may refer only
                      ; to the components at the same level as this one.
  initially-do        ; form to evaluate before the operation
  finally-do          ; form to evaluate after the operation
  compile-form        ; for foreign libraries
  load-form           ; for foreign libraries
;  load-time           ; The file-write-date of the binary/source file loaded.
  ;; If load-only is T, will not compile the file on operation :compile.
  ;; In other words, for files which are :load-only T, loading the file
  ;; satisfies any demand to recompile.
  load-only           ; If T, will not compile this file on operation :compile.
  ;; If compile-only is T, will not load the file on operation :compile.
  ;; Either compiles or loads the file, but not both. In other words,
  ;; compiling the file satisfies the demand to load it. This is useful
  ;; for PCL defmethod and defclass definitions, which wrap a 
  ;; (eval-when (compile load eval) ...) around the body of the definition.
  ;; This saves time in some lisps.
  compile-only        ; If T, will not load this file on operation :compile.
)

(defvar *file-load-time-table* (make-hash-table :test #'equal)
  "Hash table of file-write-dates for the system definitions and 
   files in the system definitions.")
(defun component-load-time (component)
  (when component
    (etypecase component
      (string    (gethash component *file-load-time-table*))
      (pathname (gethash (namestring component) *file-load-time-table*))
      (component 
       (ecase (component-type component)
	 (:defsystem
	  (let ((name (component-name component))
		(path nil))
	    (when (and name
		       (setf path (compute-system-path name nil)))
	      (gethash (namestring path) *file-load-time-table*))))
	 ((:file :private-file)
	  ;; Use only :source pathname to identify component's
	  ;; load time.
	  (let ((path (component-full-pathname component :source)))
	    (when path
	      (gethash (namestring path) *file-load-time-table*)))))))))
(defsetf component-load-time (component) (value)
  `(when ,component
    (etypecase ,component
      (string   (setf (gethash ,component *file-load-time-table*) ,value))
      (pathname (setf (gethash (namestring ,component) *file-load-time-table*)
		      ,value))
      (component 
       (ecase (component-type ,component)
	 (:defsystem
	  (let ((name (component-name ,component))
		(path nil))
	    (when (and name
		       (setf path (compute-system-path name nil)))
	      (setf (gethash (namestring path) *file-load-time-table*)
		    ,value))))
	 ((:file :private-file)
	  ;; Use only :source pathname to identify file.
	  (let ((path (component-full-pathname ,component :source)))
	    (when path
	      (setf (gethash (namestring path) *file-load-time-table*)
		    ,value)))))))))

(defun compute-system-path (module-name definition-pname)
  (let* ((filename (format nil "~A.system" 
			   (if (symbolp module-name)
			       (string-downcase (string module-name))
			       module-name))))
    (or (when definition-pname		; given pathname for system def
	  (probe-file definition-pname))
	(probe-file filename)		; try current dir
	(when *central-registry*	; central registry
	  (if (listp *central-registry*)
	      (dolist (registry *central-registry*)
		(let ((file (probe-file (append-directories registry
							    filename))))
		  (when file (return file))))
	      (probe-file (append-directories *central-registry* filename)))) 
	)))

(defvar *reload-systems-from-disk* t
  "If T, always tries to reload newer system definitions from disk.
   Otherwise first tries to find the system definition in the current
   environment.")

(defun find-system (system-name &optional (mode :ask) definition-pname)
  "Returns the system named SYSTEM-NAME. If not already loaded, loads it.
   This allows operate-on-system to work on non-loaded as well as
   loaded system definitions. DEFINITION-PNAME is the pathname for
   the system definition, if provided."
  (ecase mode
    (:ask
     (or (get-system system-name)
	 (when (y-or-n-p-wait 
		#\y 20
		"System ~A not loaded. Shall I try loading it? "
		system-name)
	   (find-system system-name :load definition-pname))))
    (:error
     (or (get-system system-name)
	 (error "Can't find system named ~s." system-name)))
    (:load-or-nil
     (let ((system (get-system system-name)))
       (or (unless *reload-systems-from-disk* system)
	   (let ((path (compute-system-path system-name definition-pname)))
	     (when (and path
			(or (null system)
			    (null (component-load-time path))
			    (< (component-load-time path)
			       (file-write-date path))))
		   (load path)
		   (setf system (get-system system-name))
		   (when system
			 (setf (component-load-time path)
			       (file-write-date path))))
	     system)
	   system)))
    (:load
     (or (unless *reload-systems-from-disk* (get-system system-name))
	 (or (find-system system-name :load-or-nil definition-pname)
	     (error "Can't find system named ~s." system-name))))))

(defun print-component (component stream depth)
  (declare (ignore depth))
  (format stream "#<~:@(~A~): ~A>"
          (component-type component)
          (component-name component)))

(defun describe-system (name &optional (stream *standard-output*))
  "Prints a description of the system to the stream. If NAME is the
   name of a system, gets it and prints a description of the system.
   If NAME is a component, prints a description of the component."
  (let ((system (if (typep name 'component) name (find-system name :load))))
    (format stream "~&~A ~A: ~
                    ~@[~&   Host: ~A~]~
                    ~@[~&   Device: ~A~]~
                    ~@[~&   Package: ~A~]~
                    ~&   Source: ~@[~A~] ~@[~A~] ~@[~A~]~
                    ~&   Binary: ~@[~A~] ~@[~A~] ~@[~A~]~
                    ~@[~&   Depends On: ~A ~]~&   Components: ~{~15T~A~&~}"
	    (component-type system)
	    (component-name system)
	    (component-host system)
	    (component-device system)
	    (component-package system)
	    (component-root-dir system :source)
	    (component-pathname system :source)
	    (component-extension system :source)
	    (component-root-dir system :binary)
	    (component-pathname system :binary)
	    (component-extension system :binary)
	    (component-depends-on system)
	    (component-components system))
    #|(when recursive
      (dolist (component (component-components system))
	(describe-system component stream recursive)))|#      
    system))

(defun canonicalize-component-name (component)
  ;; Within the component, the name is a string.
  (if (typep (component-name component) 'string)
      ;; Unnecessary to change it, so just return it, same case
      (component-name component)
    ;; Otherwise, make it a downcase string
    (setf (component-name component) 
	  (string-downcase (string (component-name component))))))

(defun component-pathname (component type)
  (when component
    (case type
      (:source (component-source-pathname component))
      (:binary (component-binary-pathname component))
      (:error  (component-error-pathname component)))))
(defun component-error-pathname (component)
  (let ((binary (component-pathname component :binary)))
    (new-file-type binary *compile-error-file-type*)))
(defsetf component-pathname (component type) (value)
  `(when ,component
     (case ,type
       (:source (setf (component-source-pathname ,component) ,value))
       (:binary (setf (component-binary-pathname ,component) ,value)))))

(defun component-root-dir (component type)
  (when component
    (case type
      (:source (component-source-root-dir component))
      ((:binary :error) (component-binary-root-dir component))
      )))
(defsetf component-root-dir (component type) (value)
  `(when ,component
     (case ,type
       (:source (setf (component-source-root-dir ,component) ,value))
       (:binary (setf (component-binary-root-dir ,component) ,value)))))

(defvar *version-dir* nil
  "The version subdir. bound in oos.")
(defvar *version-replace* nil
  "The version replace. bound in oos.")
(defvar *version* nil
  "Default version")
(defun component-full-pathname (component type &optional (version *version*)
					  &aux version-dir replace)
  (when component
    ;; If the pathname-type is :binary and the root pathname is null,
    ;; distribute the binaries among the sources (= use :source pathname).
    ;; This assumes that the component's :source pathname has been set
    ;; before the :binary one.
    (if version
	(multiple-value-setq (version-dir replace) (translate-version version))
      (setq version-dir *version-dir* replace *version-replace*))
    (let ((pathname
	   (append-directories 
	    (if replace
		version-dir
	      (append-directories (component-root-dir component type)
				  version-dir))
	    (component-pathname component type))))
      ;; When a logical pathname is used, it must first be translated to
      ;; a physical pathname. This isn't strictly correct. What should happen
      ;; is we fill in the appropriate slots of the logical pathname, and
      ;; then return the logical pathname for use by compile-file & friends.
      ;; But calling translate-logical-pathname to return the actual pathname
      ;; should do for now.
      #+:logical-pathnames-mk
      (when (eq (lp:pathname-host-type pathname) :logical)
	;;(setf (lp::%logical-pathname-type pathname)
	;;      (component-extension component type))
	(setf pathname (lp:translate-logical-pathname pathname)))

      (make-pathname :name (pathname-name pathname)
		     :type (component-extension component type)
		     :host (when (component-host component)
			     ;; MCL2.0b1 causes an error on
			     ;; (pathname-host nil)
			     (pathname-host (component-host component)))
		     :device #+CMU :absolute
		     #-CMU (pathname-device (component-device component))
		     ;; :version :newest
		     ;; Use :directory instead of :defaults
		     :directory (pathname-directory pathname)))))

(defun translate-version (version)
  ;; Value returns the version directory and whether it replaces 
  ;; the entire root (t) or is a subdirectory.
  ;; Version may be nil to signify no subdirectory,
  ;; a symbol, such as alpha, beta, omega, :alpha, mark, which
  ;; specifies a subdirectory of the root, or
  ;; a string, which replaces the root.
  (cond ((null version) 
	 (values "" nil))
	((symbolp version)
	 (values (let ((sversion (string version)))
		   (if (find-if #'lower-case-p sversion)
		       sversion
		       (string-downcase sversion))) 
		 nil))
	((stringp version)
	 (values version t))
	(t (error "~&; Illegal version ~S" version))))

(defun component-extension (component type)
  (case type
    (:source (component-source-extension component))
    (:binary (component-binary-extension component))
    (:error  *compile-error-file-type*)))
(defsetf component-extension (component type) (value)
  `(case ,type
     (:source (setf (component-source-extension ,component) ,value))
     (:binary (setf (component-binary-extension ,component) ,value))
     (:error  (setf *compile-error-file-type* ,value))))

;;; ********************************
;;; System Definition **************
;;; ********************************
(defmacro defsystem (name &rest definition-body)    
  `(create-component :defsystem ',name ',definition-body nil 0))

(defun create-component (type name definition-body &optional parent (indent 0))
  (let ((component (apply #'make-component :type type :name name :indent indent definition-body)))
    ;; Set up :load-only attribute
    (unless (find :load-only definition-body)
      ;; If the :load-only attribute wasn't specified, 
      ;; inherit it from the parent. If no parent, default it to nil.
      (setf (component-load-only component) 
	    (when parent
	      (component-load-only parent))))
    ;; Set up :compile-only attribute
    (unless (find :compile-only definition-body)
      ;; If the :compile-only attribute wasn't specified, 
      ;; inherit it from the parent. If no parent, default it to nil.
      (setf (component-compile-only component) 
	    (when parent
	      (component-compile-only parent))))

    ;; Initializations/after makes
    (canonicalize-component-name component)

    ;; Inherit package from parent if not specified.
    (setf (component-package component)
	  (or (component-package component)
	      (when parent (component-package parent))))

    ;; Type specific setup:
    (when (or (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (setf (get-system name) component))

    ;; Set up the component's pathname
    (create-component-pathnames component parent)

    ;; If there are any components of the component, expand them too.
    (expand-component-components component (+ indent 2))

    ;; Make depends-on refer to structs instead of names.
    (link-component-depends-on (component-components component))

    ;; Design Decision: Topologically sort the dependency graph at
    ;; time of definition instead of at time of use. Probably saves a
    ;; little bit of time for the user.

    ;; Topological Sort the components at this level.
    (setf (component-components component)
          (topological-sort (component-components component)))

    ;; Return the component.
    component))

(defun create-component-pathnames (component parent)
  ;; Evaluate the root dir arg
  (setf (component-root-dir component :source)
	(eval (component-root-dir component :source)))
  (setf (component-root-dir component :binary)
	(eval (component-root-dir component :binary)))
  ;; Evaluate the pathname arg
  (setf (component-pathname component :source)
	(eval (component-pathname component :source)))
  (setf (component-pathname component :binary)
	(eval (component-pathname component :binary)))
  ;; Pass along the host and devices
  (setf (component-host component)
	(or (component-host component)
	    (when parent (component-host parent))))
  (setf (component-device component)
	(or (component-device component)
	    (when parent (component-device parent))
	    ""))
  ;; Set up extension defaults
  (setf (component-extension component :source)
	(or (component-extension component :source) ; for local defaulting
	    (when parent		; parent's default
	      (component-extension parent :source))
	    (car *filename-extensions*))) ; system default
  (setf (component-extension component :binary)
	(or (component-extension component :binary) ; for local defaulting
	    (when parent		; parent's default
	      (component-extension parent :binary))
	    (cdr *filename-extensions*))) ; system default
  ;; Set up pathname defaults -- expand with parent
  ;; We must set up the source pathname before the binary pathname
  ;; to allow distribution of binaries among the sources to work.
  (generate-component-pathname component parent :source)
  (generate-component-pathname component parent :binary))

;; maybe file's inheriting of pathnames should be moved elsewhere?
(defun generate-component-pathname (component parent pathname-type)
  ;; Pieces together a pathname for the component based on its component-type.
  ;; Assumes source defined first.
  ;; Null binary pathnames inherit from source instead of the component's
  ;; name. This allows binaries to be distributed among the source if
  ;; binary pathnames are not specified. Or if the root directory is
  ;; specified for binaries, but no module directories, it inherits
  ;; parallel directory structure.
  (case (component-type component)
    ((:defsystem :system)		; Absolute Pathname
     ;; Set the root-dir to be the absolute pathname
     (setf (component-root-dir component pathname-type)
	   (or (component-pathname component pathname-type)
	       (when (eq pathname-type :binary)
		 ;; When the binary root is nil, use source.
		 (component-root-dir component :source))) )
     ;; Set the relative pathname to be nil
     (setf (component-pathname component pathname-type) 
	   nil));; should this be "" instead?
    ;; If the name of the component-pathname is nil, it
    ;; defaults to the name of the component. Use "" to
    ;; avoid this defaulting.
    (:private-file                      ; Absolute Pathname
     ;; Root-dir is the directory part of the pathname
     (setf (component-root-dir component pathname-type)
	   ""
	   #+ignore(or (when (component-pathname component pathname-type)
			 (pathname-directory 
			  (component-pathname component pathname-type)))
		       (when (eq pathname-type :binary)
			 ;; When the binary root is nil, use source.
			 (component-root-dir component :source)))
	   )
     ;; The relative pathname is the name part
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (or (when (component-pathname component pathname-type)
;		     (pathname-name )
		     (component-pathname component pathname-type))
		   (component-name component)))))
    ((:module :subsystem)			; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (setf (component-root-dir component pathname-type)
	   (component-root-dir parent pathname-type))
     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (when (and (eq pathname-type :binary)
			  (null (component-pathname component :binary)))
		 ;; When the binary-pathname is nil use source.
		 (component-pathname component :source))
	       (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component))))))
    (:file				; Pathname relative to parent.
     ;; Inherit root-dir from parent
     (setf (component-root-dir component pathname-type)
	   (component-root-dir parent pathname-type))
     ;; Tack the relative-dir onto the pathname
     (setf (component-pathname component pathname-type)
	   (or (append-directories
		(component-pathname parent pathname-type)
		(or (component-pathname component pathname-type)
		    (component-name component)
		    (when (eq pathname-type :binary)
		      ;; When the binary-pathname is nil use source.
		      (component-pathname component :source)))))))
    ))	   

(defun expand-component-components (component &optional (indent 0)) 
  (setf (component-components component)
	(remove-if #'null
	   (mapcar #'(lambda (definition)
		       (expand-component-definition definition
						    component indent))
		   (component-components component)))))

(defun expand-component-definition (definition parent &optional (indent 0))
  ;; Should do some checking for malformed definitions here.
  (cond ((null definition) nil)
        ((stringp definition) 
         ;; Strings are assumed to be of type :file
         (create-component :file definition nil parent indent))
        ((and (listp definition)
              (not (member (car definition) 
			   '(:defsystem :system :subsystem
			     :module :file :private-file))))
         ;; Lists whose first element is not a component type
         ;; are assumed to be of type :file
         (create-component :file (car definition) (cdr definition) parent indent))
        ((listp definition)
         ;; Otherwise, it is (we hope) a normal form definition
         (create-component (car definition)   ; type
                           (cadr definition)  ; name
                           (cddr definition)  ; definition body
                           parent             ; parent
			   indent)            ; indent
         )))

(defun link-component-depends-on (components)
  (dolist (component components)
    (unless (and *system-dependencies-delayed*
                 (eq (component-type component) :defsystem))
      (setf (component-depends-on component)
            (mapcar #'(lambda (dependency)
			(let ((parent (find (string dependency) components
					    :key #'component-name 
					    :test #'string-equal)))
			  (cond (parent parent)
				;; make it more intelligent about the following
				(t (warn "Dependency ~S of component ~S not found."
					 dependency component)))))
			      
                    (component-depends-on component))))))

;;; ********************************
;;; Topological Sort the Graph *****
;;; ********************************
(defun topological-sort (list &aux (time 0))
  ;; The algorithm works by calling depth-first-search to compute the
  ;; blackening times for each vertex, and then sorts the vertices into
  ;; reverse order by blackening time.
  (labels ((dfs-visit (node)
	      (setf (topsort-color node) 'gray)
	      (unless (and *system-dependencies-delayed*
			   (eq (component-type node) :defsystem))
		(dolist (child (component-depends-on node))
		  (cond ((eq (topsort-color child) 'white)
			 (dfs-visit child))
			((eq (topsort-color child) 'gray)
			 (format t "~&Detected cycle containing ~A" child)))))
		      (setf (topsort-color node) 'black)
		      (setf (topsort-time node) time)
		      (incf time)))
    (dolist (node list)
      (setf (topsort-color node) 'white))
    (dolist (node list)
      (when (eq (topsort-color node) 'white)
        (dfs-visit node)))
    (sort list #'< :key #'topsort-time)))

;;; ********************************
;;; Output to User *****************
;;; ********************************
;;; All output to the user is via the tell-user functions.

(defun split-string (string &key (item #\space) (test #'char=))
  ;; Splits the string into substrings at spaces.
  (let ((len (length string))
	(index 0) result)
    (dotimes (i len
		(progn (unless (= index len)
			 (push (subseq string index) result))
		       (reverse result)))
      (when (funcall test (char string i) item)
	(unless (= index i);; two spaces in a row
	  (push (subseq string index i) result))
	(setf index (1+ i))))))

#-gcl
(defun prompt-string (component)
  (format nil "; ~:[~;TEST:~]~V,@T "
	  *oos-test*
	  (component-indent component)))

#+gcl
(defun prompt-string (component)
  (format nil "; ~:[~;TEST:~]~V@T "
	  *oos-test*
	  (component-indent component)))

#|
(defun format-justified-string (prompt contents)
  (format t (concatenate 'string "~%" prompt "-~{~<~%" prompt " ~1,80:; ~A~>~^~}")
	  (split-string contents))
  (finish-output *standard-output*))
|#

(defun format-justified-string (prompt contents &optional (width 80)
				       (stream *standard-output*))
  (let ((prompt-length (+ 2 (length prompt))))
    (cond ((< (+ prompt-length (length contents)) width)
	   (format stream "~%~A- ~A" prompt contents))
	  (t
	   (format stream "~%~A-" prompt)
	   (do* ((cursor prompt-length)
		 (contents (split-string contents) (cdr contents))
		 (content (car contents) (car contents))
		 (content-length (1+ (length content)) (1+ (length content))))
	       ((null contents))
	     (cond ((< (+ cursor content-length) width)
		    (incf cursor content-length)
		    (format stream " ~A" content))
		   (t
		    (setf cursor (+ prompt-length content-length))
		    (format stream "~%~A  ~A" prompt content)))))))
  (finish-output stream))

(defun tell-user (what component &optional type no-dots force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
     (format nil "~A ~(~A~) ~@[\"~A\"~] ~:[~;...~]"
	     ;; To have better messages, wrap the following around the
	     ;; case statement:
	     ;;(if (find (component-type component) 
	     ;;    '(:defsystem :system :subsystem :module))
	     ;;  "Checking"
	     ;;  (case ...))
	     ;; This gets around the problem of DEFSYSTEM reporting
	     ;; that it's loading a module, when it eventually never
	     ;; loads any of the files of the module.
	     (case what 
	       ((compile :compile) 
		(if (component-load-only component)
		    ;; If it is :load-only t, we're loading.
		    "Loading"
		    ;; Otherwise we're compiling.
		    "Compiling"))
	       ((load :load) "Loading")
	       (otherwise what))
	     (component-type component)
	     (or (when type
		   (namestring-or-nil (component-full-pathname
				       component type)))
		 (component-name component))
	     (and *tell-user-when-done*
		  (not no-dots))))))

(defun tell-user-done (component &optional force no-dots)
  ;; test is no longer really used, but we're leaving it in.
  (when (and *tell-user-when-done*
	     (or *oos-verbose* force))
    (format t "~&~A~:[~;...~] Done."
	    (prompt-string component) (not no-dots))
    (finish-output *standard-output*)))

(defmacro with-tell-user ((what component &optional type no-dots force) &body body)
  `(progn
     (tell-user ,what ,component ,type ,no-dots ,force)
     ,@body
     (tell-user-done ,component ,force ,no-dots)))

(defun tell-user-no-files (component &optional force)
  (when (or *oos-verbose* force)
    (format-justified-string (prompt-string component)
      (format nil "Source file ~A ~
             ~:[and binary file ~A ~;~]not found, not loading."
	      (namestring (component-full-pathname component :source))
	      (or *load-source-if-no-binary* *load-source-instead-of-binary*)
	      (namestring (component-full-pathname component :binary))))))

(defun tell-user-require-system (name parent)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - System ~A requires ~S"
	    *oos-test* (component-name parent) name)
    (finish-output *standard-output*)))

(defun tell-user-generic (string)
  (when *oos-verbose*
    (format t "~&; ~:[~;TEST:~] - ~A"
	    *oos-test* string)
    (finish-output *standard-output*)))

;;; ********************************
;;; Y-OR-N-P-WAIT ******************
;;; ********************************
;;; y-or-n-p-wait is like y-or-n-p, but will timeout
;;; after a specified number of seconds
(defun internal-real-time-in-seconds ()
  (float (/ (get-internal-real-time) 
	    internal-time-units-per-second)))

(defun read-char-wait (&optional (timeout 20) input-stream &aux char)
  (do ((start (internal-real-time-in-seconds)))
      ((or (setq char (read-char-no-hang input-stream)) ;(listen *query-io*)
	   (< (+ start timeout) (internal-real-time-in-seconds)))
       char)))

;;; Lots of lisps, especially those that run on top of UNIX, do not get
;;; their input one character at a time, but a whole line at a time because
;;; of the buffering done by the UNIX system. This causes y-or-n-p-wait
;;; to not always work as expected. 
;;;
;;; I wish lisp did all its own buffering (turning off UNIX input line
;;; buffering by putting the UNIX into CBREAK mode). Of course, this means
;;; that we lose input editing, but why can't the lisp implement this? 

(defvar *use-timeouts* t
  "If T, timeouts in Y-OR-N-P-WAIT are enabled. Otherwise it behaves
   like Y-OR-N-P. This is provided for users whose lisps don't handle
   read-char-no-hang properly.")

(defvar *clear-input-before-query* t
  "If T, y-or-n-p-wait will clear the input before printing the prompt
   and asking the user for input.")

(defun y-or-n-p-wait (&optional (default #\y) (timeout 20) 
				format-string &rest args)
  "Y-OR-N-P-WAIT prints the message, if any, and reads characters from
   *QUERY-IO* until the user enters y, Y or space as an affirmative, or either
   n or N as a negative answer, or the timeout occurs. It asks again if
   you enter any other characters."
  (when *clear-input-before-query* (clear-input *query-io*))
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    ;; FINISH-OUTPUT needed for CMU and other places which don't handle
    ;; output streams nicely. This prevents it from continuing and
    ;; reading the query until the prompt has been printed.
    (finish-output *query-io*))
  (loop
   (let* ((read-char (if *use-timeouts*
			 (read-char-wait timeout *query-io*)
			 (read-char *query-io*)))
	  (char (or read-char default)))
     ;; We need to ignore #\newline because otherwise the bugs in 
     ;; clear-input will cause y-or-n-p-wait to print the "Type ..."
     ;; message every time... *sigh*
     ;; Anyway, we might want to use this to ignore whitespace once
     ;; clear-input is fixed.
     (unless (find char '(#\tab #\newline #\return))
       (when (null read-char) 
	 (format *query-io* "~@[~A~]" default)
	 (finish-output *query-io*))
       (cond ((null char) (return t))
	     ((find char '(#\y #\Y #\space) :test #'char=) (return t))
	     ((find char '(#\n #\N) :test #'char=) (return nil))
	     (t 
	      (when *clear-input-before-query* (clear-input *query-io*))
	      (format *query-io* "~&Type \"y\" for yes or \"n\" for no. ")
	      (when format-string
		(fresh-line *query-io*)
		(apply #'format *query-io* format-string args))
	      (finish-output *query-io*)))))))

#|
(y-or-n-p-wait #\y 20 "What? ")
(progn (format t "~&hi") (finish-output)
       (y-or-n-p-wait #\y 10 "1? ")
       (y-or-n-p-wait #\n 10 "2? "))
|#
;;; ********************************
;;; Operate on System **************
;;; ********************************
;;; Operate-on-system
;; Operation is :compile, 'compile, :load or 'load
;; Force is :all or :new-source or :new-source-and-dependents or a list of
;; specific modules.
;;    :all (or T) forces a recompilation of every file in the system
;;    :new-source-and-dependents compiles only those files whose
;;          sources have changed or who depend on recompiled files.
;;    :new-source compiles only those files whose sources have changed
;;    A list of modules means that only those modules and their dependents are recompiled.
;; Test is T to print out what it would do without actually doing it. 
;;      Note: it automatically sets verbose to T if test is T.
;; Verbose is T to print out what it is doing (compiling, loading of
;;      modules and files) as it does it.
;; Dribble should be the pathname of the dribble file if you want to 
;; dribble the compilation.
;; Load-source-instead-of-binary is T to load .lisp instead of binary files.
;; Version may be nil to signify no subdirectory,
;; a symbol, such as alpha, beta, omega, :alpha, mark, which
;; specifies a subdirectory of the root, or
;; a string, which replaces the root.
;;
(defun operate-on-system (name operation &key force
			       (version *version*)
			       (test *oos-test*) (verbose *oos-verbose*)
                               (load-source-instead-of-binary *load-source-instead-of-binary*)
                               (load-source-if-no-binary *load-source-if-no-binary*) 
			       (bother-user-if-no-binary *bother-user-if-no-binary*)
			       (compile-during-load *compile-during-load*)
			       dribble
			       (minimal-load *minimal-load*))
  (unwind-protect
      ;; Protect the undribble.
      (progn
	(when dribble (dribble dribble))
	(when test (setq verbose t))
	(when (null force);; defaults
	  (case operation
	    ((load :load) (setq force :all))
	    ((compile :compile) (setq force :new-source-and-dependents))
	    (t (setq force :all))))
	;; Some CL implementations have a variable called *compile-verbose*
	;; or *compile-file-verbose*.
	(multiple-value-bind (*version-dir* *version-replace*) 
	    (translate-version version)
	  ;; CL implementations may uniformly default this to nil
	  (let ((*load-verbose* t) ; nil
		(*compile-file-verbose* t) ; nil
		(*compile-verbose* t) ; nil
		(*version* version)
		(*oos-verbose* verbose)
		(*oos-test* test)
		(*load-source-if-no-binary* load-source-if-no-binary)
		(*compile-during-load* compile-during-load)
		(*bother-user-if-no-binary* bother-user-if-no-binary)
		(*load-source-instead-of-binary* load-source-instead-of-binary)
		(*minimal-load* minimal-load)
		(system (find-system name :load)))
	    (unless (component-operation operation)
	      (error "Operation ~A undefined." operation))
	    (operate-on-component system operation force))))
    (when dribble (dribble))))

(defun COMPILE-SYSTEM (name &key force
			    (version *version*)
			    (test *oos-test*) (verbose *oos-verbose*)
			    (load-source-instead-of-binary *load-source-instead-of-binary*)
			    (load-source-if-no-binary *load-source-if-no-binary*) 
			    (bother-user-if-no-binary *bother-user-if-no-binary*)
			    (compile-during-load *compile-during-load*)
			    dribble
			    (minimal-load *minimal-load*))
  ;; For users who are confused by OOS.
  (operate-on-system 
   name :compile
   :force force
   :version version
   :test test
   :verbose verbose
   :load-source-instead-of-binary load-source-instead-of-binary
   :load-source-if-no-binary load-source-if-no-binary
   :bother-user-if-no-binary bother-user-if-no-binary
   :compile-during-load compile-during-load
   :dribble dribble
   :minimal-load minimal-load))

(defun LOAD-SYSTEM (name &key force
			 (version *version*)
			 (test *oos-test*) (verbose *oos-verbose*)
			 (load-source-instead-of-binary *load-source-instead-of-binary*)
			 (load-source-if-no-binary *load-source-if-no-binary*) 
			 (bother-user-if-no-binary *bother-user-if-no-binary*)
			 (compile-during-load *compile-during-load*)
			 dribble
			 (minimal-load *minimal-load*))
  ;; For users who are confused by OOS.
  (operate-on-system 
   name :load
   :force force
   :version version
   :test test
   :verbose verbose
   :load-source-instead-of-binary load-source-instead-of-binary
   :load-source-if-no-binary load-source-if-no-binary
   :bother-user-if-no-binary bother-user-if-no-binary
   :compile-during-load compile-during-load
   :dribble dribble
   :minimal-load minimal-load))

(defun operate-on-component (component operation force &aux changed)
  ;; Returns T if something changed and had to be compiled.
  (let ((type (component-type component))
	(old-package (package-name *package*)))

    (unwind-protect
	;; Protect old-package.
	(progn
	  ;; Use the correct package.
	  (when (component-package component)
	    (tell-user-generic (format nil "Using package ~A" 
				       (component-package component)))
	    (unless *oos-test*
	      (unless (find-package (component-package component))
		;; If the package name is the same as the name of the system,
		;; and the package is not defined, this would lead to an
		;; infinite loop, so bomb out with an error.
		(when (string-equal (string (component-package component)) 
				    (component-name component))
		  (format t "~%Component ~A not loaded:~%"
			  (component-name component))
		  (error  "  Package ~A is not defined"
			  (component-package component)))
		;; If package not found, try using REQUIRE to load it.
		(new-require (component-package component)))
	      ;; This was USE-PACKAGE, but should be IN-PACKAGE.
	      ;; Actually, CLtL2 lisps define in-package as a macro,
	      ;; so we'll set the package manually.
	      ;; (in-package (component-package component))
	      (let ((package (find-package (component-package component))))
		(when package
		  (setf *package* package)))))

	  ;; Load any required systems
	  (when (eq type :defsystem)	; maybe :system too?
	    (operate-on-system-dependencies component operation force))

	  ;; Do any initial actions
	  (when (component-initially-do component)
	    (tell-user-generic (format nil "Doing initializations for ~A"
				       (component-name component)))
	    (or *oos-test*
		(eval (component-initially-do component))))

	  ;; If operation is :compile and load-only is T, this would change
	  ;; the operation to load. Only, this would mean that a module would
	  ;; be considered to have changed if it was :load-only and had to be
	  ;; loaded, and then dependents would be recompiled -- this doesn't
	  ;; seem right. So instead, we propagate the :load-only attribute
	  ;; to the components, and modify compile-file-operation so that
	  ;; it won't compile the files (and modify tell-user to say "Loading"
	  ;; instead of "Compiling" for load-only modules). 
	  #|(when (and (find operation '(:compile compile))
	       (component-load-only component))
      (setf operation :load))|#

	  ;; Do operation and set changed flag if necessary.
	  (setq changed 
		(case type
		  ((:file :private-file)
		   (funcall (component-operation operation) component force))
		  ((:module :system :subsystem :defsystem)
		   (operate-on-components component operation force changed))))

	  ;; Do any final actions
	  (when (component-finally-do component)
	    (tell-user-generic (format nil "Doing finalizations for ~A"
				       (component-name component)))
	    (or *oos-test*
		(eval (component-finally-do component)))))

      ;; Reset the package. (Cleanup form of unwind-protect.)
      ;;(in-package old-package)
      (setf *package* (find-package old-package)))

    ;; Provide the loaded system
    (when (or (eq type :defsystem) (eq type :system) (eq type :subsystem))
      (tell-user-generic (format nil "Providing system ~A"
				 (component-name component)))
      (or *oos-test*
	  (provide (canonicalize-system-name (component-name component))))))

  ;; Return t if something changed in this component and hence had to be recompiled.
  changed)

(defvar *force* nil)
(defvar *providing-blocks-load-propagation* t
  "If T, if a system dependency exists on *modules*, it is not loaded.")
(defun operate-on-system-dependencies (component operation &optional force)
  (when *system-dependencies-delayed*
    (let ((*force* force))
      (dolist (system (component-depends-on component))
	;; For each system that this system depends on, if it is a
	;; defined system (either via defsystem or component type :system),
	;; and propagation is turned on, propagates the operation to the
	;; subsystem. Otherwise runs require (my version) on that system
	;; to load it (needed since we may be depending on a lisp
	;; dependent package).
	;; Explores the system tree in a DFS manner.
	(cond ((and *operations-propagate-to-subsystems*
		    (not (listp system))
		    ;; The subsystem is a defined system.
		    (find-system system :load-or-nil))
	       ;; Call OOS on it. Since *system-dependencies-delayed* is
	       ;; T, the :depends-on slot is filled with the names of
	       ;; systems, not defstructs.
	       ;; Aside from system, operation, force, for everything else
	       ;; we rely on the globals.
	       (unless (and *providing-blocks-load-propagation*
			    ;; If *providing-blocks-load-propagation* is T,
			    ;; the system dependency must not exist in the
			    ;; *modules* for it to be loaded. Note that
			    ;; the dependencies are implicitly systems.
			    (find operation '(load :load))    
			    ;; (or (eq force :all) (eq force t))
			    (find (canonicalize-system-name system)
				  *modules* :test #'string=))
		 (operate-on-system system operation :force force)))
	      ((listp system)
	       (tell-user-require-system 
		(cond ((and (null (car system)) (null (cadr system)))
		       (caddr system))
		      (t system))
		component)
	       (or *oos-test* (new-require (car system) nil
					   (eval (cadr system))
					   (caddr system) 
					   (or (car (cdddr system))
					       *version*))))
	      (t
	       (tell-user-require-system system component)
	       (or *oos-test* (new-require system))))))))

(defun operate-on-components (component operation force changed)
  (with-tell-user (operation component)
    (if (component-components component)
	(dolist (module (component-components component))
	  (when (operate-on-component module operation
		  (cond ((and (dolist (dependent (component-depends-on module))
				(when (member dependent changed)
				  (return t)))
			      #|(some #'(lambda (dependent)
					(member dependent changed))
				    (component-depends-on module))|#
			      (or (non-empty-listp force)
				  (eq force :new-source-and-dependents)))
			 ;; The component depends on a changed file 
			 ;; and force agrees.
			 (if (eq force :new-source-and-dependents)
			     :new-source-all
			   :all))
			((and (non-empty-listp force)
			      (member (component-name module) force
				      :test #'string-equal :key #'string))
			 ;; Force is a list of modules 
			 ;; and the component is one of them.
			 :all)
			(t force)))
	    (push module changed)))
	(case operation
	  ((compile :compile)
	   (eval (component-compile-form component)))
	  ((load :load)
	   (eval (component-load-form component))))))
  changed)

;;; ********************************
;;; New Require ********************
;;; ********************************
(defvar *old-require* nil)

;;; All calls to require in this file have been replaced with calls
;;; to new-require to avoid compiler warnings and make this less of
;;; a tangled mess.
(defun new-require (module-name &optional pathname definition-pname
				default-action (version *version*))
  ;; If the pathname is present, this behaves like the old require.
  (unless (and module-name 
	       (find #-CMU (string module-name)
		     #+CMU (string-downcase (string module-name))
		     *modules* :test #'string=)) 
    (cond (pathname
	   (funcall *old-require* module-name pathname))
	  ;; If the system is defined, load it.
	  ((find-system module-name :load-or-nil definition-pname)
	   (operate-on-system module-name :load
	     :force *force*
	     :version version
	     :test *oos-test*
	     :verbose *oos-verbose*
	     :load-source-if-no-binary *load-source-if-no-binary*
	     :bother-user-if-no-binary *bother-user-if-no-binary*
	     :compile-during-load *compile-during-load*
	     :load-source-instead-of-binary *load-source-instead-of-binary*
	     :minimal-load *minimal-load*))
	  ;; If there's a default action, do it. This could be a progn which
	  ;; loads a file that does everything. 
	  ((and default-action
		(eval default-action)))
	  ;; If no system definition file, try regular require.
	  ;; had last arg  PATHNAME, but this wasn't really necessary.
	  ((funcall *old-require* module-name)) 
	  ;; If no default action, print a warning or error message.
	  (t
	   (format t "~&Warning: System ~A doesn't seem to be defined..." 
		   module-name)))))

;;; Note that in some lisps, when the compiler sees a REQUIRE form at
;;; top level it immediately executes it. This is as if an 
;;; (eval-when (compile load eval) ...) were wrapped around the REQUIRE
;;; form. I don't see any easy way to do this without making REQUIRE
;;; a macro. 
;;;
;;; For example, in VAXLisp, if a (require 'streams) form is at the top of
;;; a file in the system, compiling the system doesn't wind up loading the
;;; streams module. If the (require 'streams) form is included within an
;;; (eval-when (compile load eval) ...) then everything is OK.
;;;
;;; So perhaps we should replace the redefinition of lisp:require
;;; with the following macro definition:
#|
(unless *old-require*
  (setf *old-require* 
	(symbol-function #-(and :excl :allegro-v4.0) 'lisp:require
			 #+(and :excl :allegro-v4.0) 'cltl1:require))

  (let (#+:CCL (ccl:*warn-if-redefine-kernel* nil))
    ;; Note that lots of lisps barf if we redefine a function from
    ;; the LISP package. So what we do is define a macro with an
    ;; unused name, and use (setf macro-function) to redefine 
    ;; lisp:require without compiler warnings. If the lisp doesn't
    ;; do the right thing, try just replacing require-as-macro 
    ;; with lisp:require.
    (defmacro require-as-macro (module-name 
				&optional pathname definition-pname
				default-action (version '*version*))
      `(eval-when (compile load eval)
	 (new-require ,module-name ,pathname ,definition-pname 
		      ,default-action ,version)))
    (setf (macro-function #-(and :excl :allegro-v4.0) 'lisp:require
			  #+(and :excl :allegro-v4.0) 'cltl1:require)
	  (macro-function 'require-as-macro))))
|#
;;; This will almost certainly fix the problem, but will cause problems
;;; if anybody does a funcall on #'require.

;;; Redefine old require to call the new require.
(unless *old-require*
  (setf *old-require* 
	(symbol-function #-(or (and :excl :allegro-v4.0) :mcl
			       :harlequin-common-lisp) 'lisp:require
			 #+(and :excl :allegro-v4.0) 'cltl1:require
			 #+:mcl 'ccl:require
			 #+:harlequin-common-lisp 'system::require))

  (unless *dont-redefine-require*
    (let (#+(and :CCL (not :harlequin-common-lisp))
	    (ccl:*warn-if-redefine-kernel* nil))
      (setf (symbol-function 
	     #-(or (and :excl :allegro-v4.0) :mcl :harlequin-common-lisp)
	     'lisp:require
	     #+(and :excl :allegro-v4.0) 'cltl1:require
	     #+:mcl 'ccl:require
	     #+:harlequin-common-lisp 'system::require)
	    (symbol-function 'new-require)))))


;;; ********************************
;;; Component Operations ***********
;;; ********************************
;;; Define :compile/compile and :load/load operations
(component-operation :compile  'compile-and-load-operation)
(component-operation 'compile  'compile-and-load-operation)
(component-operation :load     'load-file-operation)
(component-operation 'load     'load-file-operation)

(defun compile-and-load-operation (component force)
  ;; FORCE was CHANGED. this caused defsystem during compilation to only
  ;; load files that it immediately compiled.
  (let ((changed (compile-file-operation component force)))
    ;; Return T if the file had to be recompiled and reloaded.
    (if (and changed (component-compile-only component))
	;; For files which are :compile-only T, compiling the file
	;; satisfies the need to load. 
	changed
	;; If the file wasn't compiled, or :compile-only is nil,
	;; check to see if it needs to be loaded.
	(and (load-file-operation component force) ; FORCE was CHANGED ???
	     changed))))

(defun compile-file-operation (component force)
  ;; Returns T if the file had to be compiled.
  (let ((must-compile
	 ;; For files which are :load-only T, loading the file
	 ;; satisfies the demand to recompile.
	 (and (null (component-load-only component)) ; not load-only
	      (or (find force '(:all :new-source-all t) :test #'eq) 
		  (and (find force '(:new-source :new-source-and-dependents)
			     :test #'eq)
		       (needs-compilation component))))))

    (cond ((and must-compile
		(probe-file (component-full-pathname component :source)))
	   (with-tell-user ("Compiling source" component :source)
	     (or *oos-test*
		 (compile-file (component-full-pathname component :source)
			       :output-file (component-full-pathname component :binary)
			       #+CMU :error-file #+CMU (and *cmu-errors-to-file* 
							    (component-full-pathname component :error))
			       #+(and CMU (not :new-compiler))
			       :errors-to-terminal
			       #+(and CMU (not :new-compiler))
			       *cmu-errors-to-terminal*
			       )))
	   must-compile)
	  (must-compile
	   (tell-user "Source file not found. Not compiling"
		      component :source :no-dots :force)
	   nil)
	  (t nil))))

(defun needs-compilation (component)
  ;; If there is no binary, or it is older than the source
  ;; file, then the component needs to be compiled.
  ;; Otherwise we only need to recompile if it depends on a file that changed.
  (and 
   ;; source must exist
   (probe-file (component-full-pathname component :source)) 
   (or
    ;; no binary
    (null (probe-file (component-full-pathname component :binary))) 
    ;; old binary
    (< (file-write-date (component-full-pathname component :binary)) 
       (file-write-date (component-full-pathname component :source))))))

(defun needs-loading (component &optional (check-source t) (check-binary t))
  ;; Compares the component's load-time against the file-write-date of
  ;; the files on disk. 
  (let ((load-time (component-load-time component)))
    (or 
     ;; File never loaded.
     (null load-time)
     ;; Binary is newer.
     (when (and check-binary
		(probe-file (component-full-pathname component :binary)))
       (< load-time
	  (file-write-date (component-full-pathname component :binary))))
     ;; Source is newer.
     (when (and check-source
		(probe-file (component-full-pathname component :source)))
       (< load-time
	  (file-write-date (component-full-pathname component :source)))))))

;;; Need to completely rework this function...
(defun load-file-operation (component force)
  ;; Returns T if the file had to be loaded
  (let* ((binary-pname (component-full-pathname component :binary))
	 (source-pname (component-full-pathname component :source))
	 (binary-exists (probe-file binary-pname))
	 (source-exists (probe-file source-pname))
	 (source-needs-loading (needs-loading component t nil))
	 (binary-needs-loading (needs-loading component nil t))
	 ;; needs-compilation has an implicit source-exists in it.
	 (needs-compilation (if (component-load-only component)
				source-needs-loading
				(needs-compilation component)))
	 (check-for-new-source 
	  ;; If force is :new-source*, we're checking for files
	  ;; whose source is newer than the compiled versions.
	  (find force '(:new-source :new-source-and-dependents :new-source-all)
		:test #'eq))
	 (load-binary (or (find force '(:all :new-source-all t) :test #'eq)
			  binary-needs-loading))
	 (load-source
	  (or *load-source-instead-of-binary*
	      (and load-binary (component-load-only component))
	      (and check-for-new-source needs-compilation)))
	 (compile-and-load
	  (and needs-compilation (or load-binary check-for-new-source)
	       (compile-and-load-source-if-no-binary component))))
    ;; When we're trying to minimize the files loaded to only those
    ;; that need be, restrict the values of load-source and load-binary
    ;; so that we only load the component if the files are newer than
    ;; the load-time.
    (when *minimal-load*
      (when load-source (setf load-source source-needs-loading))
      (when load-binary (setf load-binary binary-needs-loading)))

    (when (or load-source load-binary compile-and-load)
      (cond (compile-and-load
	     ;; If we're loading the binary and it is old or nonexistent,
	     ;; and the user says yes, compile and load the source.
	     (compile-file-operation component t)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (load binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     T)
	    ((and source-exists
		  (or (and load-source	; implicit needs-comp...
			   (or *load-source-instead-of-binary*
			       (component-load-only component)
			       (not *compile-during-load*)))
		      (and load-binary (not binary-exists)
			   (load-source-if-no-binary component))))
	     ;; Load the source if the source exists and:
	     ;;   o  we're loading binary and it doesn't exist
	     ;;   o  we're forcing it
	     ;;   o  we're loading new source and user wasn't asked to compile
	     (with-tell-user ("Loading source" component :source)
	       (or *oos-test*
		   (progn 
		     (load source-pname)
		     (setf (component-load-time component)
			   (file-write-date source-pname)))))
	     T)
	    ((and binary-exists load-binary)
	     (with-tell-user ("Loading binary"   component :binary)
	       (or *oos-test*
		   (progn
		     (load binary-pname)
		     (setf (component-load-time component)
			   (file-write-date binary-pname)))))
	     T)
	    ((and (not binary-exists) (not source-exists))
	     (tell-user-no-files component :force)
	     (when *files-missing-is-an-error*
	       (cerror "Continue, ignoring missing files."
		       "~&Source file ~S ~:[and binary file ~S ~;~]do not exist."
		       (namestring source-pname)
		       (or *load-source-if-no-binary* 
			   *load-source-instead-of-binary*)
		       (namestring binary-pname)))
	     nil)
	    (t 
	     nil)))))

(component-operation :delete-binaries     'delete-binaries-operation)
(defun delete-binaries-operation (component force)
  (when (or (eq force :all)
	    (eq force t)
	    (and (find force '(:new-source :new-source-and-dependents
					   :new-source-all)
		       :test #'eq)
		 (needs-compilation component)))
    (when (probe-file (component-full-pathname component :binary))
      (with-tell-user ("Deleting binary"   component :binary)
	(or *oos-test*
	    (delete-file (component-full-pathname component :binary)))))))

	
;; when the operation = :compile, we can assume the binary exists in test mode.
;;	((and *oos-test*
;;	      (eq operation :compile)
;;	      (probe-file (component-full-pathname component :source)))
;;	 (with-tell-user ("Loading binary"   component :binary)))

;;; or old-binary
(defun compile-and-load-source-if-no-binary (component)
  (when (and (not *load-source-instead-of-binary*)
	     (not *load-source-if-no-binary*))
    (cond ((component-load-only component)
	   #|(let ((prompt (prompt-string component)))
	     (format t "~A- File ~A is load-only, ~
                      ~&~A  not compiling."
		     prompt
		     (namestring (component-full-pathname component :source))
		     prompt))|#
	   nil)
	  ((eq *compile-during-load* :query)
	   (let* ((prompt (prompt-string component))
		  (compile-source
		   (y-or-n-p-wait 
		    #\y 30
		    "~A- Binary file ~A is old or does not exist. ~
                   ~&~A  Compile (and load) source file ~A instead? "
		    prompt
		    (namestring (component-full-pathname component :binary))
		    prompt
		    (namestring (component-full-pathname component :source)))))
	     (unless (y-or-n-p-wait 
		      #\y 30
		      "~A- Should I bother you if this happens again? "
		      prompt)
	       (setq *compile-during-load* 
		     (y-or-n-p-wait 
		      #\y 30
		      "~A- Should I compile and load or not? "
		      prompt))) ; was compile-source, then t
	     compile-source))
	  (*compile-during-load*)
	  (t nil))))

(defun load-source-if-no-binary (component)
  (and (not *load-source-instead-of-binary*)
       (or *load-source-if-no-binary*
	   (component-load-only component)
	   (when *bother-user-if-no-binary*
	     (let* ((prompt (prompt-string component))
		    (load-source
		     (y-or-n-p-wait #\y 30
		      "~A- Binary file ~A does not exist. ~
                       ~&~A  Load source file ~A instead? "
		      prompt
		      (namestring (component-full-pathname component :binary))
		      prompt
		      (namestring (component-full-pathname component :source)))))
	       (setq *bother-user-if-no-binary*
		     (y-or-n-p-wait #\n 30
		      "~A- Should I bother you if this happens again? "
		      prompt ))
	       (unless *bother-user-if-no-binary*
		 (setq *load-source-if-no-binary* load-source))
	       load-source)))))

;;; ********************************
;;; Allegro Make System Fasl *******
;;; ********************************
#+:excl
(defun allegro-make-system-fasl (system destination)
  (excl:shell
   (format nil "rm -f ~A; cat~{ ~A~} > ~A" 
	   destination
	   (mapcar #'namestring
		   (files-in-system system :all :binary)))))

(defun files-which-need-compilation (system)
  (mapcar #'(lambda (comp) (namestring (component-full-pathname comp :source)))
	  (remove nil
		  (file-components-in-component
		   (find-system system :load) :new-source))))

(defun files-in-system (name &optional (force :all) (type :source) version)
  ;; Returns a list of the pathnames in system in load order.
  (let ((system (find-system name :load)))
    (multiple-value-bind (*version-dir* *version-replace*) 
	(translate-version version)
      (let ((*version* version))
	(file-pathnames-in-component system type force)))))

(defun file-pathnames-in-component (component type &optional (force :all))
  (mapcar #'(lambda (comp) (component-full-pathname comp type))
	  (file-components-in-component component force)))

(defun file-components-in-component (component &optional (force :all) 
					       &aux result changed)
  (case (component-type component)
    ((:file :private-file)
     (when (setq changed 
		 (or (find force '(:all t) :test #'eq) 
		     (and (not (non-empty-listp force))
			  (needs-compilation component))))
       (setq result
	     (list component))))
    ((:module :system :subsystem :defsystem)
     (dolist (module (component-components component))
       (multiple-value-bind (r c)
	   (file-components-in-component 
	    module 
	    (cond ((and (some #'(lambda (dependent)
				  (member dependent changed))
			      (component-depends-on module))
			(or (non-empty-listp force)
			    (eq force :new-source-and-dependents)))
		   ;; The component depends on a changed file and force agrees.
		   :all)
		  ((and (non-empty-listp force)
			(member (component-name module) force
				:test #'string-equal :key #'string))
		   ;; Force is a list of modules and the component is one of them.
		   :all)
		  (t force)))
	 (when c
	   (push module changed)
	   (setq result (append result r)))))))
  (values result changed))

(setf (symbol-function 'oos) (symbol-function 'operate-on-system))

;;; ********************************
;;; Additional Component Operations 
;;; ********************************

;;; *** Edit Operation ***

#+:ccl
(defun edit-operation (component force)
  "Always returns nil, i.e. component not changed."
  (declare (ignore force))
  ;;
  (let* ((full-pathname (make::component-full-pathname component :source))
         (already-editing\? #+:mcl (dolist (w (windows :class 'fred-window))
                                    (when (equal (window-filename w)
                                                 full-pathname)
                                      (return w)))
                           #-:mcl nil))
    (if already-editing\?
      #+:mcl (window-select already-editing\?) #-:mcl nil
      (ed full-pathname)))
  nil)

#+:ccl
(make::component-operation :edit 'edit-operation)
#+:ccl
(make::component-operation 'edit 'edit-operation)

;;; *** System Source Size ***

(defun system-source-size (system-name)
  "Prints a short report and returns the size in bytes of the source files in
   <system-name>."
  (let* ((file-list (files-in-system system-name :all :source))
         (total-size (file-list-size file-list)))
    (format t "~&~S (~A files) totals ~A bytes (~A K)"
            system-name (length file-list) total-size (round total-size 1024))
    total-size))

(defun file-list-size (file-list)
  "Returns the size in bytes of the files in <file-list>."
  ;;
  (let ((total-size 0))
    (dolist (file file-list)
      (with-open-file (stream file)
        (incf total-size (file-length stream))))
    total-size))


;
;;; ****************************************************************
;;; Dead Code ******************************************************
;;; ****************************************************************

#|
;;; ********************************
;;; Alist Manipulation *************
;;; ********************************
;;; This is really gross. I've replaced it with hash tables.

(defun alist-lookup (name alist &key (test #'eql) (key #'identity))
  (cdr (assoc name alist :test test :key key)))

(defmacro set-alist-lookup ((name alist &key (test '#'eql) (key '#'identity)) 
			    value)
  (let ((pair (gensym)))
    `(let ((,pair (assoc ,name ,alist :test ,test :key ,key)))
       (if ,pair
	   (rplacd ,pair ,value)
	 (push (cons ,name ,value) ,alist)))))

(defun component-operation (name &optional operation)
  (if operation
      (set-alist-lookup (name *component-operations*) operation)
    (alist-lookup name *component-operations*)))

(defun machine-type-translation (name &optional operation)
  (if operation
      (set-alist-lookup (name *machine-type-alist* :test #'string-equal)
			operation)
    (alist-lookup name *machine-type-alist* :test #'string-equal)))

(defun software-type-translation (name &optional operation)
  (if operation
      (set-alist-lookup (name *software-type-alist* :test #'string-equal)
			operation)
    (alist-lookup name *software-type-alist* :test #'string-equal)))

|#

;;; *END OF FILE*





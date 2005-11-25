;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-doc.el --
;;; ILISP mode documentation
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(defconst ilisp-documentation
  "Major mode for interacting with an inferior Lisp process.  Runs a
Lisp interpreter as a subprocess of Emacs, with Lisp I/O through an
Emacs buffer.  If you have problems, use `M-x ilisp-bug' in the buffer
where you are having a problem to send a bug report.

To start a Lisp use `M-x run-ilisp', or a specific dialect like `M-x
allegro'.  If called with a prefix you will be prompted for a buffer
name and a program to run.  The default buffer name is the name of the
dialect.  The default program for a dialect will be the value of
`DIALECT-program' or the value of `ilisp-program' inherited from a
less specific dialect.  If there are multiple Lisp's, use the dialect
name or `select-ilisp' \(\\[select-ilisp]) to select the current ILISP
buffer.

Currently supported Lisp dialects include (some of them may be only
partially supported):
 common-lisp
   allegro
   clisp-hs
   cmulisp
   kcl
     akcl
       gcl
       ecl
     ibcl
   lispworks
   lucid
     liquid
   sbcl
 scheme
   chez
   guile
   oaklisp
   Scheme->C
   scm
   snow
   stk
 xlisp
   xlispstat

Customization: Starting a dialect runs the hooks on `comint-mode-hook'
and `ilisp-mode-hook' and then `DIALECT-hook's specific to dialects in
the nesting order above.  On the very first prompt in the inferior
Lisp, the hooks on `ilisp-init-hook' are run.  For more information on
creating a new dialect or variables to set in hooks, see ilisp.el.

Most of these key bindings work in both Lisp Mode and ILISP mode.
There are a few additional and-go bindings found in Lisp Mode.
\\{ilisp-use-map}
There are also a few bindings found in global-map including:
  \\[ilisp-bury-output] `ilisp-bury-output'
  \\[ilisp-scroll-output] `ilisp-scroll-output'
  \\[previous-buffer-lisp] `previous-buffer-lisp'
  \\[switch-to-lisp] `switch-to-lisp'

ILISP has a very flexible means for displaying output from the
underlying lisp.  All output is funneled through the function bound to
`ilisp-display-output-function'.  That function gets a single
argument, the string to display, and should make it visible to the
user.  The default display function, `ilisp-display-output-default',
displays one-line output in the echo area and longer output in a
shrink-wrapped typeout window.  This typeout window can be manipulated
with \\[ilisp-bury-output] `ilisp-bury-output',
\\[ilisp-scroll-output] `ilisp-scroll-output', and
\\[ilisp-grow-output] `ilisp-grow-output'.

An alternative to typeout windows is to always have the inferior Lisp
buffer visible and have all output go there.  If your are using the
default display function, then setting `lisp-no-popper' to t will
cause all output to go to the inferior Lisp buffer.  Setting
`comint-always-scroll' to t will cause process output to always be
visible.  If a command gets an error, you will be left in the break
loop.

Here are the supplied display functions:
 `ilisp-display-output-default'
 `ilisp-display-output-adaptively'
 `ilisp-display-output-in-echo-area'
 `ilisp-display-output-in-typeout-window'
 `ilisp-display-output-in-lisp-listener'

Each ILISP buffer has a command history associated with it.  Commands
that do not match `ilisp-filter-regexp' and that are longer than
`ilisp-filter-length' and that do not match the immediately prior
command will be added to this history.  `comint-previous-input'
\(\\[comint-previous-input]) and `comint-next-input'
\(\\[comint-next-input]) cycle through the input history.
`comint-previous-similar-input' \(\\[comint-previous-similar-input])
cycles through input that has the string typed so far as a prefix.

See `comint-mode' documentation for more information on comint
commands.

A number of commands refer to \"defun\".  A \"defun\" is a list that
starts at the left margin in a Lisp buffer, or after a prompt in the
ILISP buffer.  So the commands refer to the \"defun\" that contains
point.

There are two keyboard modes for interacting with the inferior Lisp,
\"interactive\" and \"raw\".  Normally you are in interactive mode
where keys are interpreted as commands to Emacs and nothing is sent to
the inferior Lisp unless a specific command does so.  In raw mode, all
characters are passed directly to the inferior Lisp without any
interpretation as Emacs commands.  Keys will not be echoed unless
`ilisp-raw-echo' is t.  Raw mode can be turned on interactively by
`raw-keys-ilisp' \(\\[raw-keys-ilisp]) and will continue until you
type \\[keyboard-quit].  Raw mode can also be turned on/off by
inferior Lisp functions if `io-bridge-ilisp' \(\\[io-bridge-ilisp])
has been executed in the inferior Lisp interactively or on a hook.  To
turn on raw mode, a function should print ^[1^] and to turn it off
should print ^[0^].

When you send something to Lisp, the status light will reflect the
progress of the command.  If you type top-level forms ahead of the
processing, the status may indicate ready when the Lisp is actually
running.  In a lisp mode buffer the light will reflect the status of
the currently selected inferior Lisp unless `lisp-show-status' is nil.
If you want to find out what command is currently running, use the
command `status-lisp' \(\\[status-lisp]).  If you call it with a
prefix, the pending commands will be displayed as well.

If you want to abort the last command you can use \\[keyboard-quit].
If you want to abort all commands, you should use the command
`abort-commands-lisp' \(\\[abort-commands-lisp]).  Commands that are
aborted will be put in the buffer *Aborted Commands* so that you can
see what was aborted.  If you want to abort the currently running
top-level command, use `interrupt-subjob-ilisp'
\(\\[interrupt-subjob-ilisp]).  As a last resort, \\[panic-lisp] will
reset the ILISP state without affecting the inferior Lisp so that you
can see what is happening.  If you become totally frustrated, you can
also try \\[repair-ilisp].

`bol-ilisp' \(\\[bol-ilisp]) will go after the prompt as defined by
`comint-prompt-regexp' or `ilisp-other-prompt' or to the left margin
with a prefix.

`return-ilisp' \(\\[return-ilisp]) knows about prompts and sexps.  If
an sexp is not complete, it will indent properly.  When an entire sexp
is complete, it is sent to the inferior Lisp together with a new line.
If you edit old input, the input will be copied to the end of the
buffer first.

`close-and-send-lisp' \(\\[close-and-send-lisp]) will close the
current sexp, indent it, then send it to the current inferior Lisp.

`indent-line-ilisp' \(\\[indent-line-ilisp]) indents for Lisp.  With
prefix, shifts rest of expression rigidly with the current line.

`newline-and-indent-lisp' \(\\[newline-and-indent-lisp]) will insert a
new line and then indent to the appropriate level.  If you are at the
end of the inferior Lisp buffer and an sexp, the sexp will be sent to
the inferior Lisp without a trailing newline.

`indent-sexp-ilisp' \(\\[indent-sexp-ilisp]) will indent each line in
the next sexp.

`backward-delete-char-untabify' \(\\[backward-delete-char-untabify])
converts tabs to spaces as it moves back.

`delete-char-or-pop-ilisp' \(\\[delete-char-or-pop-ilisp]) will delete
prefix characters unless you are at the end of an ILISP buffer in
which case it will pop one level in the break loop.

`reset-ilisp', \(\\[reset-ilisp]) will reset the current inferior
Lisp's top-level so that it will no longer be in a break loop.

`switch-to-lisp' \(\\[switch-to-lisp]) will pop to the current ILISP
buffer or if already in an ILISP buffer, it will return to the buffer
that last switched to an ILISP buffer.  With a prefix, it will also go
to the end of the buffer.  If you do not want it to pop, set
`pop-up-windows' to nil.

`call-defun-lisp' \(\\[call-defun-lisp]) will put a call to the
current defun in the inferior Lisp and go there.  If it is a \(def*
name form, it looks up reasonable forms of name in the input history
unless called with a prefix. If not found, \(name or *name* will be
inserted.  If it is not a def* form, the whole defun will be put in
the buffer.

`reposition-window-lisp' \(\\[reposition-window-lisp]) will scroll the
current window to show as much of the current defun and its
introductory comments as possible without moving the point.  If called
with a prefix, the point will be moved if necessary to show the start
of the defun.  If called more than once with the first line of the
defun showing, the introductory comments will be shown or suppressed.

`previous-buffer-lisp' \(\\[previous-buffer-lisp]) will switch to the
last visited buffer in the current window or the Nth previous buffer
with a prefix.

`find-unbalanced-lisp' \(\\[find-unbalanced-lisp]) will find
unbalanced parens in the current buffer.  When called with a prefix it
will look in the current region.

`close-all-lisp' \(\\[close-all-lisp]) will close all outstanding
parens back to the containing form, or a previous left bracket which
will be converted to a left parens.  If there are too many parens,
they will be deleted unless there is text between the last paren and
the end of the defun.  If called with a prefix, all open left brackets
will be closed.

`reindent-lisp' \(\\[reindent-lisp]) will reindent the current
paragraph if in a comment or string.  Otherwise it will close the
containing defun and reindent it.

`comment-region-lisp' \(\\[comment-region-lisp]) will put prefix
copies of `comment-start' before and `comment-end's after the lines in
region.  To uncomment a region, use a minus prefix.

The very first inferior Lisp command executed may send some forms to
initialize the inferior Lisp.

Each time an inferior Lisp command is executed, the last form sent can be
seen in the \*ilisp-send* buffer.

The first time an inferior Lisp mode command is executed in a Lisp
Mode buffer, the package will be determined by using the regular
expression `ilisp-hash-form-regexp' to find a package sexp and then
passing that sexp to the inferior Lisp through
`ilisp-package-command'.  For the CLISP dialect, this will find the
first \(in-package PACKAGE) form in the file.  A buffer's package will
be displayed in the mode line.  `set-buffer-package-lisp'
\(\\[set-buffer-package-lisp]) will update the current package from
the buffer.  If it is called with a prefix, the package can be set
manually.  If a buffer has no specification, forms will be evaluated
in the current inferior Lisp package.  `package-lisp'
\(\\[package-lisp]) will show the current package of the inferior
Lisp.  `set-package-lisp' \(\\[set-package-lisp]) will set the
inferior Lisp package to the current buffer's package or to a manually
entered package with a prefix.

`describe-lisp', `inspect-lisp', `arglist-lisp', `documentation-lisp',
`macroexpand-1-lisp', `macroexpand-lisp`, `edit-definitions-lisp',
`who-calls-lisp', `edit-callers-lisp' and `trace-defun-lisp' will
switch whether they prompt for a response or use a default when called
with a negative prefix.  If they are prompting, there is completion
through the inferior Lisp by using TAB or M-TAB.  When you are
entering an expression in the minibuffer, all of the normal ILISP
commands like `arglist-lisp' also work.

Commands that work on a function will use the nearest previous
function symbol.  This is either a symbol after a #' or the symbol at
the start of the current list.

`describe-lisp' \(\\[describe-lisp]) will describe the previous sexp.
`inspect-lisp' \(\\[inpsect-lisp]) will inspect the previous sexp.  If
there is no previous-sexp and you are in an ILISP buffer, the previous
result will be described or inspected.

`arglist-lisp' \(\\[arglist-lisp]) will return the arglist of the
current function.  With a numeric prefix, the leading paren will be
removed and the arglist will be inserted into the buffer.

`documentation-lisp' \(\\[documentation-lisp]) infers whether function
or variable documentation is desired.  With a negative prefix, you can
specify the type of documentation as well.  With a positive prefix the
documentation of the current function call is returned.

If the Franz online Common Lisp manual is available, and
`ilisp-*use-fi-clman-interface-p*' is set to t, `fi:clman'
\(\\[fi:clman]) will get information on a specific symbol.
`fi:clman-apropos' \(\\[fi:clman-apropos]) will get information
apropos a specific string.  Some of the documentation is specific to
the Allegro dialect, but most of it is for standard Common Lisp.

`macroexpand-lisp' \(\\[macroexpand-lisp]) and `macroexpand-1-lisp'
\(\\[macroexpand-1-lisp]) will be applied to the next sexp.  They will
insert their result into the buffer if called with a numeric prefix.

`complete-lisp' \(\\[complete-lisp]) will try to complete the previous
symbol in the current inferior Lisp.  Partial completion is supported
unless `ilisp-*prefix-match*' is set to t.  \(If you set it to t,
inferior Lisp completions will be faster.)  With partial completion,
\"p--n\" would complete to \"position-if-not\" in Common Lisp.  If the
symbol follows a left paren or a #', only symbols with function cells
will be considered.  If the symbol starts with a \* or you call with a
positive prefix all possible completions will be considered.  Only
external symbols are considered if there is a package qualification
with only one colon.  The first time you try to complete a string the
longest common substring will be inserted and the cursor will be left
on the point of ambiguity.  If you try to complete again, you can see
the possible completions.  If you are in a string, then filename
completion will be done instead.  And if you try to complete a
filename twice, you will see a list of possible completions.  Filename
components are completed individually, so /u/mi/ could expand to
/usr/misc/.  If you complete with a negative prefix, the most recent
completion \(symbol or filename) will be undone.

`complete' \(\\[complete]) will complete the current symbol to the
most recently seen symbol in Emacs that matches what you have typed so
far.  Executing it repeatedly will cycle through potential matches.
This is from the TMC completion package and there may be some delay as
it is initially loaded.

`trace-defun-lisp' \(\\[trace-defun-lisp]) traces the current defun.
When called with a numeric prefix the function will be untraced.

`trace-defun-lisp-break' \(\\[trace-defun-lisp-break]) traces the
current defun but sets a breakpoint in the function if possible.  When
called with a numeric prefix the function will be untraced.

`default-directory-lisp' \(\\[default-directory-lisp]\) sets the
default inferior Lisp directory to the directory of the current
buffer.  If called in an inferior Lisp buffer, it sets the Emacs
default-directory the Lisp default directory.

The eval/compile commands evaluate or compile the forms specified.  If
any of the forms contain an interactive command, then the command will
never return.  To get out of this state, you need to use
`abort-commands-lisp' \(\\[abort-commands-lisp]).  The eval/compile
commands verify that their expressions are balanced and then send the
form to the inferior Lisp.  If called with a positive prefix, the
result of the operation will be inserted into the buffer after the
form that was just sent.  If `lisp-wait-p' is t, then Emacs will
display the result of the command in the minibuffer or a pop-up
window.  If `lisp-wait-p' is nil, (the default) the send is done
asynchronously and the results will be brought up only if there is
more than one line or there is an error.  In this case, you will be
given the option of ignoring the error, keeping it in another buffer
or keeping it and aborting all pending sends.  If there is not a
command already running in the inferior Lisp, you can preserve the
break loop.  If called with a negative prefix, the sense of
`lisp-wait-p' will be inverted for the next command.  The and-go
versions will perform the operation and then immediately switch to the
ILISP buffer where you will see the results of executing your form.
If `eval-defun-and-go-lisp' \(\\[eval-defun-and-go-lisp]) or
`compile-defun-and-go-lisp' \(\\[compile-defun-and-go-lisp]) is called
with a prefix, a call for the form will be inserted as well.

When an eval is done of a single form matching `ilisp-defvar-regexp',
the corresponding symbol will be unbound and the value assigned again.

When `compile-defun-lisp' \(\\[compile-defun-lisp]) is called in an
inferior Lisp buffer with no current form, the last form typed to the
top-level will be compiled.

The following commands all deal with finding things in source code.
The first time that one of these commands is used, there may be some
delay while the source module is loaded.  When searching files, the
first applicable rule is used: 1) try the inferior Lisp, 2) try a tags
file if defined, 3) try all buffers in one of `lisp-source-modes' or
all files defined using `lisp-directory'.

`lisp-directory' \(\\[lisp-directory]) defines a set of files to be
searched by the source code commands.  It prompts for a directory and
sets the source files to be those in the directory that match entries
in `auto-mode-alist' for modes in `lisp-source-modes'.  With a
positive prefix, the files are appended.  With a negative prefix, all
current buffers that are in one of `lisp-source-modes' will be
searched.  This is also what happens by default.  Using this command
stops using a tags file.

`edit-definitions-lisp' \(\\[edit-definitions-lisp]) will find a
particular type of definition for a symbol.  It tries to use the rules
described above.  The files to be searched are listed in the buffer
\*Edit-Definitions*.  If `lisp-edit-files' is nil, no search will be
done if not found through the inferior Lisp.  The variable
`ilisp-locator' contains a function that when given the name and type
should be able to find the appropriate definition in the file.  There
is often a flag to cause your Lisp to record source files that you
will need to set in the initialization file for your Lisp.  The
variable is `\*record-source-files*' in both Allegro and Lucid.  Once
a definition has been found, `next-definition-lisp'
\(\\[next-definition-lisp]) will find the next definition.  \(Or the
previous definition with a prefix.)

`edit-callers-lisp' \(\\[edit-callers-lisp]) will generate a list of
all of the callers of a function in the current inferior Lisp and edit
the first caller using `edit-definitions-lisp'.  Each successive call
to `next-caller-lisp' \(\\[next-caller-lisp]) will edit the next
caller.  \(Or the previous caller with a prefix.)  The list is stored
in the buffer \*All-Callers*.  You can also look at the callers by
doing `who-calls-lisp' \(\\[who-calls-lisp]).

`search-lisp' \(\\[search-lisp]) will search the current tags files,
Lisp directory files or buffers in one of `lisp-source-modes' for a
string or a regular expression when called with a prefix.
\\[next-definition-lisp] will find the next definition.  \(Or the
previous definition with a prefix.)

`replace-lisp' \(\\[replace-lisp]) will replace a string (or a regexp
with a prefix) in the current tags files, Lisp directory files or
buffers in one of `lisp-source-modes'.

The following commands all deal with making a number of changes all at
once.  The first time one of these commands is used, there may be some
delay as the module is loaded.  The eval/compile versions of these
commands are always executed asynchronously.

`mark-change-lisp' \(\\[mark-change-lisp]) marks the current defun as
being changed.  A prefix causes it to be unmarked.
`clear-changes-lisp' \(\\[clear-changes-lisp]) will clear all of the
changes.  `list-changes-lisp' \(\\[list-changes-lisp]) will show the
forms currently marked.

`eval-changes-lisp' \(\\[eval-changes-lisp]), or
`compile-changes-lisp' \(\\[compile-changes-lisp]) will evaluate or
compile these changes as appropriate.  If called with a positive
prefix, the changes will be kept.  If there is an error, the process
will stop and show the error and all remaining changes will remain in
the list.  All of the results will be kept in the buffer
*Last-Changes*.

File commands in `lisp-source-mode' buffers keep track of the last
used directory and file.  If the point is on a string, that will be
the default if the file exists.  If the buffer is one of
`lisp-source-modes', the buffer file will be the default.  Otherwise,
the last file used in a `lisp-source-mode' will be used.

`find-file-lisp' \(\\[find-file-lisp]) will find a file.  If it is in
a string, that will be used as the default if it matches an existing
file.  Symbolic links are expanded so that different references to the
same file will end up with the same buffer.

`load-file-lisp' \(\\[load-file-lisp]) will load a file into the inferior
Lisp.  You will be given the opportunity to save the buffer if it has
changed and to compile the file if the compiled version is older than
the current version.

`compile-file-lisp' \(\\[compile-file-lisp]) will compile a file in
the current inferior Lisp.")

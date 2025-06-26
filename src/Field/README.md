# Extrategies

**Extrategies** is a collection of strategies, strategy combinators, and Lisp functions for writting strategies in PVS. It provides improved versions of standard PVS strategies for skolemizing, naming, labeling, replacing, splitting, etc. Notably, it also contains a framework to debug PVS strategies.


## Summary of Provided Strategies

|  | |
| --- | --- |
| Printing and commenting| `printf`, `commentf`, `quietly`, `error-msg`, `warning-msg`|
| Defining proof rules| `deftactic`, `deforacle`|
| Labeling and naming| `unlabel*`, `delabel`, `relabel`, `name-label`, `name-label*`, `name-replace*`, `discriminate`|
| Copying formulas| `copy*`, `protect`, `with-focus-on`, `with-focus-on@`|
| Programming| `mapstep`, `mapstep@`, `with-fresh-labels`, `with-fresh-labels@`, `with-fresh-names`, `with-fresh-names@`, `cond-match`, `if-match`|
| Control flow| `try@`, `try-then`, `try-then@`, `finalize`, `finalize*`, `touch`, `for`, `for@`, `when`, `when@`, `unless`, `unless@`, `when-label`, `when-label@`, `unless-label`, `unless-label@`, `if-label`, `sklisp`|
| Let-in| `skoletin`, `skoletin*`, `redlet`, `redlet*`|
| Quantifiers| `skeep`, `skeep*`, `skodef`, `skodef*`, `insteep`, `insteep*`, `unroll`|
| TCCs| `tccs-expression`, `tccs-formula`, `tccs-formula*`, `tccs-step`, `if-tcc-sequent`, `with-tccs`|
| Ground evaluation (PVSio)| `eval-formula`, `eval-formula*`, `eval-expr`, `eval`|
| Miscellaneous| `splash`, `replaces`, `rewrites`, `rewrite*`, `suffices`|
| Strategy debugging ([see below](#debugging-strategies))| `skip-steps`, `show-debug-mode`, `enable-debug-mode`, `disable-debug-mode`, `set-debug-mode`, `load-files`|

## Debugging Strategies

When debugging strategies (or any
code), it is often challenging to print relevant information only at the
the relevant execution points in the code. This usually involves more than
instrumenting the code with print and break calls since those calls
may be executed multiple times before they become relevant to the issue
we are debugging. This library provides a set of functions and
strategy commands that help on instrumenting Lisp code to print and
break at specified execution points. The actions of these print and
break calls can be dynamically suppressed based on user-defined
conditions such as function scope, strategy cope, and local variables
and expressions. Although this library could be used to with any Lisp
code, its interface is tailored to debugging strategy code
within the PVS theorem prover. Also, this library is not a replacement
for more sophisticated Common Lisp development environments
such as [SLIME](https://slime.common-lisp.dev/) and [SLY](https://joaotavora.github.io/sly/).

### Strategy Commands

The library includes the following strategy commands, which should be
called within the interactive read-eval-loop of the PVS theorem prover.
-`(enable-debug-mode [<files>])` and `(disable-debug-mode [<files>])`:
These commands enable/disable, respectively, debug mode, on a list of
Lisp files or directories containing Lisp files.
-`(set-debug-mode [<files>] :frames [<n>] :verbose [ none | nil | t ] :mode
[ toggle | enable | disable ] :suppress [<suppress-function>])`:
Configure debug mode.
- `(show-debug-mode)`: Print current configuration of debug mode.

### Lisp Functions

The Lisp functions
`(extra-debug-println ...)`, `(extra-debug-print ...)`,
`(extra-debug-break ...)` can be used to instrument Lisp code with
formatted print messages involving Lisp expressions and their
values. The function `(extra-debug-break ...)` behaves exactly like
`(extra-debug-print ...)` but it will break into the Common Lisp
debugger before returning from the call.  The arguments of those
functions have the form `<string> | <expr> | <fmted-string> | <tag>`.
A string `<string>` will be printed as such. A Lisp expression `<expr>` will be printed as
`EXPR = VAL` where `EXPR`
is the string representation of `<expr>` and `VAL` is its Lisp value.
A `<fmted-string>` has the form `(:fmt <format> <e1> .. <en>)`
and will be printed using the call `(format t <format> <e1>
.. <en>)`.  A `<tag>` is a Lisp keyword, i.e., a symbol prefixed with a
colon. Tag keywords appearing before `<expr>` have two
purposes. First, they are used when printing `<expr>` as `TAG = VAL`,
instead of `EXPR = VAL`.  Also, `<tag>` will serve to bind `VAL`
to a local variable `TAG` in suppress functions (see below).

The functions `extra-debug-println`, `extra-debug-print`, and
`extra-debug-break` will fail if debug mode is not enabled. Therefore, the
typical way to use these functions is to add them to the Lisp file
containing the code to be debugged, e.g., `myfile.lisp`, and then
enable debug mode in that file through the strategy
`(enable-debug-mode "myfile.lisp")`. To discourage having debug code
in the PVS binaries, function calls `(extra-debug-println ...)`,
`(extra-debug-print ...)` and `(extra-debug-break ...)`
will **fail** at compilation time. To avoid the compilation error,
a conditional compilation directive
`#+extra-debug` could be use, e.g.,

```
#+extra-debug (extra-debug-println ...)
```

### Suppress Functions

The function `(extra-debug-println ...)` prints one line of
information. In contrast to the other two functions
`extra-debug-print` and `extra-debug-break`, the action of
`extra-debug-println` cannot be dynamically
suppressed.  The functions `(extra-debug-print ...)`
and `(extra-debug-break ...)` print every input
data in one line. Strings, in contarts to Lisp expressions, are
printed without  indentation, so they can
be used as section headers in the output.  The library allows for a
verbose mode, which by default is disabled. When verbose mode is enabled, these functions will also
print the stack of execution frames up to a user-specified number of
frames and the stack of strategies. Furthermore, the action of these
functions could be dynamically disabled via programmatic high-level
functions called suppress functions. A suppress function is a
Lisp  lambda function on an association list of tags and their
respective values. The lambda function is called before the execution
of ` (extra-debug-print ...)` and `(extra-debug-break ...)`
to decide if their actions should be suppressed or no.
The association list has the tags `:frame-stack`, associated to the current stack of frames, and
`:strategy-stack`, associated to the current stack of strategies, in
addition to those tags defined by the user. The following pre-defined
suppress functions are available.

* `suppress-all`: Suppress all printing
* `suppress-none`: Suppress nothing
* `(suppress-from f1 .. fn)`: Suppress printing from `f1`,.., and `fn`
* `(suppress-but f1 .. fn)`: Suppress printing except from `f1`,.., or `fn`
* `(suppress-in-scope f1 .. fn)`: Suppress printing within the scope of `f1`,.., and `fn` up to `*extra-debug-frames*`
* `(suppress-out-scope f1 .. fn)`: Suppress printing except outside the scope of `f1`,.., or `fn` up to `*extra-debug-frames*`
* `(suppress-from-strat s1 .. sn)`: Suppress printing from strategies `s1`,.., and `sn`
* `(suppress-but-strat s1 .. sn)`: Suppress printing except from strategies `s1`,.., or `sn`
* `(suppress-in-scope-strat s1 .. sn)`: Suppress printing within the scope of strategies `s1`,.., and `sn`
* `(suppress-out-scope-strat s1 .. sn)`: Suppress printing outside the scope of strategies `s1`,.., or `sn`
* `(suppress-when expr)`: Suppress when Lisp expression `expr` holds
* `(suppress-unless expr)`: Suppress unless Lisp expression `expr` holds

Furthermore, suppress functions can be combined using `(suppress-and
<supp1> .. <suppn>)`, `(suppress-or <supp1> .. <suppn>)` and
`(suppress-not <supp>)`. The global variable `*extra-debug-suppress*`
has the current suppress function, which can be combined to form a new one.

### Use Case

Assume we are developing the strategy `mystrat` in the file `pvs-strategies`.

```
(defstep mystrat (fnums)
 ...
   (let ((expr (extra-get-formula fnum)))
        ...
   )
 ...
)
```

To print a message "**HERE**", the value of `fnum`, whether `fnum` is positive, `expr`, and whether `expr` is an existential expression, we could write

```
(let ((expr  (extra-get-formula fnum))
      (dummy (extra-debug-println "**HERE**" 
              fnum (> fnum 0) expr (exists-expr? expr))
...)
```

Then, we run the strategy

```
Rule? (enable-debug-mode ".")
```

This strategy will enable debug mode. Furthermore, it will load all Lisp files from directory `"."` (current directory) including the file `pvs-strategies`. After that, running `mystrat` on

```
{-1}  a > 0
  |-------
{1}   EXISTS (b: real | b > 0): b < a
```

results in

```
Rule? (mystrat 1)
[*extra-debug-println*] **HERE**, FNUM = 1, (> FNUM 0) = T, EXPR = EXISTS (b:
                                                                             real
                                                                               | b
                                                                                  >
                                                                                  0):
                                                                     b < a, (EXISTS-EXPR? EXPR) = T
...

Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL, EXPR = a > 0, (EXISTS-EXPR?
                                                                      EXPR) = NIL
...
```

The function `extra-debug-print` is more verbose than
`extra-debug-println` and allows for more control on the information
that is printed. Let's assume that `mystrat` calls strategies `ff` and
`gg` , which, respectively call, functions `f` and `g` . In turn,
`f` and `g` call function `h`.

```
(defstep mystrat (fnum)
  (let ((expr (extra-get-formula fnum)))
    (if (> fnum 0)
		(ff fnum expr)
		(gg fnum expr))
 ...)

(defstep ff (fnum expr)
  (let ((dummy (f fnum expr)))
    (skip-msg "ff" t))
 ...)

(defstep gg (fnum expr)
  (let ((dummy (g fnum expr)))
    (skip-msg "gg" t))
  ...)

(defun f (fnum expr)
  (extra-debug-print "** F **" fnum expr)
  (h fnum expr))

(defun g (fnum expr)
  (extra-debug-print "** G **" fnum expr)
  (h fnum expr))

(defun h (fnum expr)
  (extra-debug-print "** H **" fnum expr))
```

When the file `pvs-strategies` is changed, we need to reload it.
This can be done using the strategy `(load-files ...)`.
By default, this strategy remembers the files that have been uploaded
since the last time. Therefore, we don't need to provide any
arguments, i.e.,


```
Rule? (load-files)
...
Loaded files: /.../pvs-strategies
...
```

Then,

```
Rule? (mystrat -1)
[*extra-debug-print*
** G **
  FNUM = -1
  EXPR = a > 0
*extra-debug-print*]

[*extra-debug-print*
** H **
  FNUM = -1
  EXPR = a > 0
*extra-debug-print*]

gg
...
```

By default, verbose mode is disable, so `extra-debug-print` and
`extra-debug-break` only print what is explicitly provide in their
arguments.  When verbose mode is enabled, these functions print
the execution stack, upto to a user-specified number of frames (by
default 1), and the stack of strategies. The strategy command
`set-debug-mode` is used to
enable verbose mode and change the number of frames, e.g.,

```
Rule? (set-debug-mode :verbose t :frames 2)

*extra-debug-frame-count*: 2
*extra-debug-files*: ...
*extra-debug-verbose*: T
*extra-debug-suppress*: Printing and breaking are suppressed never
Debug mode: ENABLED

```

If the number of frames is 0, no execution frame is printed. A
negative number specifies that the whole execution stack shoul be printed.
This stack is usually long.

```
Rule? (then (mystrat 1) (mystrat -1))
[*extra-debug-print*
** F **
  FNUM = 1
  EXPR = EXISTS (b: real | b > 0): b < a

[TOP FRAME] (F 1 EXISTS (b: real | b > 0): b < a)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1) (EXPR 'EXISTS (b: real | b > 0): b < a))
               (F FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (FF 1 EXISTS (b: real | b > 0): b < a)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]

[*extra-debug-print*
** H **
  FNUM = 1
  EXPR = EXISTS (b: real | b > 0): b < a

[TOP FRAME] (H 1 EXISTS (b: real | b > 0): b < a)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1) (EXPR 'EXISTS (b: real | b > 0): b < a))
               (F FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (FF 1 EXISTS (b: real | b > 0): b < a)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]

ff

...

[*extra-debug-print*
** G **
  FNUM = -1
  EXPR = a > 0

[TOP FRAME] (G -1 a > 0)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '-1) (EXPR 'a > 0))
               (G FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (GG -1 a > 0)>
[STRAT  -1] #<rule:  (MYSTRAT -1)>
*extra-debug-print*]

[*extra-debug-print*
** H **
  FNUM = -1
  EXPR = a > 0

[TOP FRAME] (H -1 a > 0)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '-1) (EXPR 'a > 0))
               (G FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (GG -1 a > 0)>
[STRAT  -1] #<rule:  (MYSTRAT -1)>
*extra-debug-print*]

gg
```

The command `set-debug-mode` also accepts 
the option `:suppress <supp>`, where `<supp>` is a suppress function.
The suppress function is used by
by `(extra-debug-print ...)` and `(extra-debug-break ...)` to decide
if their actions  should be suppressed or no.
For example, to only enable printing from function `h`, we could write

```
Rule? (set-debug-mode :suppress (suppress-but h))

*extra-debug-frame-count*: 2
*extra-debug-files*: ...
*extra-debug-verbose*: T
*extra-debug-suppress*: Printing and breaking are suppressed except from H
...

Rule? (then (mystrat 1) (mystrat -1))
[*extra-debug-print*
** H **
  FNUM = 1
  EXPR = EXISTS (b: real | b > 0): b < a

[TOP FRAME] (H 1 EXISTS (b: real | b > 0): b < a)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1) (EXPR 'EXISTS (b: real | b > 0): b < a))
               (F FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (FF 1 EXISTS (b: real | b > 0): b < a)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]

ff

...

[*extra-debug-print*
** H **
  FNUM = -1
  EXPR = a > 0

[TOP FRAME] (H -1 a > 0)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '-1) (EXPR 'a > 0))
               (G FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (GG -1 a > 0)>
[STRAT  -1] #<rule:  (MYSTRAT -1)>
*extra-debug-print*]

gg
```

Only calls to `(extra-debug-print ...)` and `(extra-debug-break ...)`
are affected by the `:suppress` setting. The function
`(extra-debug-println ...)`always prints information.

Now, let's assume that we want to suppress all printing except from
`h` when `FNUM` is positive.  We could do the follow.

```
Rule? (set-debug-mode :suppress (suppress-not (suppress-and (suppress-from "h") (suppress-when (> fnum 0)))))

*extra-debug-frame-count*: 2
*extra-debug-files*: ...
*extra-debug-verbose*: T
*extra-debug-suppress*: Printing and breaking are suppressed (except (from h and when expression (> FNUM 0) holds))

Rule? (then (mystrat 1) (mystrat -1))
[*extra-debug-print*
** H **
  FNUM = 1
  EXPR = EXISTS (b: real | b > 0): b < a

[TOP FRAME] (H 1 EXISTS (b: real | b > 0): b < a)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1) (EXPR 'EXISTS (b: real | b > 0): b < a))
               (F FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (FF 1 EXISTS (b: real | b > 0): b < a)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]

ff
```

Of course, Boolean logic still applies, so the above suppress function
could also be written as follows.

```
Rule? (set-debug-mode :suppress (suppress-or (suppress-but "h") (suppress-unless (> fnum 0))))


*extra-debug-frame-count*: 2
*extra-debug-files*: ...
*extra-debug-verbose*: T
*extra-debug-suppress*: Printing and breaking are suppressed (except from h or unless expression (> FNUM 0) holds)
Debug mode: ENABLED

Rule? (then (mystrat 1) (mystrat -1))
[*extra-debug-print*
** H **
  FNUM = 1
  EXPR = EXISTS (b: real | b > 0): b < a

[TOP FRAME] (H 1 EXISTS (b: real | b > 0): b < a)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1) (EXPR 'EXISTS (b: real | b > 0): b < a))
               (F FNUM EXPR))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (FF 1 EXISTS (b: real | b > 0): b < a)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]

ff
```

The strategy command `set-debug-mode`doesn't require reloading files afterwards.

### Technical Notes
 * The stack of frames is provided by the SBCL function `(sb-debug:list-backtrace ...)`,
   Functions that are inlined by the compiler don't appear in the stack. Therefore, if `FUN`
   is the name of a function inlined by the compiler, `(suppress-in-scope FUN)` never holds and
   `(suppress-out-scope FUN)` always holds.
 * The stack of strategies is provided by PVS global variable `*proof-strategy-stack*`.
   Glass-box strategies are inlined by the theorem prover, so they don't appear in
   the strategy stack. Therefore, if `STRAT` is the name a glass-box strategy, `suppress-in-scope-strat STRAT)`
   never holds and `(suppress-out-scope-strat STRAT)` always holds.
 * `EXPR` in `(suppress-when EXPR)` and `(suppress-unless EXPR)` can be an arbitrary Lisp code including
   global variables, variables that are printed by extra-debug-print and extra-debug-break, and tags
   that are specified by those functions.

### Compilation Error
Loading code instrumented with `extra-debug-print` or `extra-debug-println` without enabling debug mode will result in the following error.

```
Rule? (mystrat 1)
...
%%%
%%% This code requires the feature :extra-debug. To avoid this error at compile time, 
%%% use the conditional compilation directive #+extra-debug before (extra-debug-println ...)
%%% (extra-debug-print ...), and (extra-debug-break ...).
%%%
```

This behavior is intentional to discourage the presence of debug code
in the code base. The error could be avoided by adding the compilation
directive `#+extra-debug` as indicated in the error message, e.g.,

```
 (let ((expr (extra-get-formula fnum))
	    #+extra-debug
		(dummy (extra-debug-print "**HERE**" fnum expr))
  ...)
```

### Detailed Documentation

For detailed documentation of the strategies mentioned in this document, please use the `help` proof command. For instance, `(help set-debug-mode)`.

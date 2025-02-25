**Extrategies** is a collection of strategies, strategy combinators, and Lisp functions for writting strategies in PVS. It provides improved versions of standard PVS strategies for skolemizing, naming, labeling, replacing, splitting, etc. Notably, it also contains a framework to debug PVS strategies.


# Summary of Provided Strategies

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

# Debugging Strategies

## Light Debugging

The strategies `(set-debug-mode [<files>] :mode [toggle|enable|disable])`, `(enable-debug-mode [<files>])` and `(disable-debug-mode [<files>])` enable/disable, respectively debug mode. Once debug mode is enabled, the lisp functions `(extra-debug-print [val-or-expr]*)` and `(extra-debug-println [val-or-expr]*)` can be used to print values and expressions for debugging.

These functions will fail if debug mode is not enabled. Therefore, the typical way to use these functions is to add them to the lisp file containing the code to be debugged, e.g., `myfile.lisp`, and then to enable debug mode in that file, e.g., through the strategy `(enable-debug-mode "myfile.lisp")`. To discourage having debug code in the PVS binaries, function calls `(extra-debug-println ..)` or `(extra-debug-println ..)` will **fail** at compilation time. To avoid the compilation error, a conditional compilation directive `#+extra-debug` could be use, e.g.,

```
#+extra-debug (extra-debug-println ...)
```

The function `(extra-debug-println ..)` prints one line of information. The function `(extra-debug-print ..)` is more verbose, e.g., it prints back traces up to a user-specified number of frames, and allows a developer to suppress messages depending on the scope where the call appears.

## Use Case

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

To print the value of `fnum`, whether `fnum` is positive, `expr`, and whether `expr` is an existential expression, we could write

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

This strategy will enable debug mode. Furthermore, it will load all lisp files from directory `"."` (current directory) including the file `pvs-strategies`. After that, running `mystrat` on

```
{-1}  a > 0
  |-------
{1}   EXISTS (b: real | b > 0): b < a
```

results in

```
Rule? (mystrat 1)
[*extra-debug-println*] **HERE**, FNUM = 1, (> FNUM 0) = T, EXPR = EXISTS (b: real | b > 0): b < a, (EXISTS-EXPR?
                                                                                        EXPR) = T
...

Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL, EXPR = a > 0, (EXISTS-EXPR?
                                                                      EXPR) = NIL
...
```

The function `extra-debug-print` is more verbose than `extra-debug-println` and allows for more control on the information that is printed based on the scope where `(extra-debug-print ...)` is called. Let's assume that `mystrat` calls functions `f` and `g`, both of which call function `h`, and all functions `f`, `g`, and `h` have calls to `extra-debug-print`, e.g.,

```
(defstep mystrat (fnum)
  ...
  (let ((expr  (extra-get-formula fnum))
	(dummy (extra-debug-println "**HERE**" 
                fnum (> fnum 0))))
    (if (> fnum 0)
	(... (f fnum))
      (... (g num)))
  ...
)  

(defun f (x)
  ...
  (extra-debug-print "** F **" x)
  (h y)
  ...
)

(defun g (x)
  ...  
  (extra-debug-print "** G **" x)
  (h x)
  ...
)

(defun h (x)
 ...
  (extra-debug-print "** H **" x)
 ...
```

Since the file `pvs-strategies` has changed, we need to reload it. This can be done using the strategy `(load-files ...)`. By default, this strategy remembers the files that have been uploaded since the last time. Therefore, we don't need to provide any arguments, i.e.,

```
Rule? (load-files)
...
Loaded files: /.../pvs-strategies
...
```

Then,

```
Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL

[*extra-debug-print*
[TOP FRAME] (G -1)

** G **
  X = -1
*extra-debug-print*]

[*extra-debug-print*
[TOP FRAME] (H -1)

** H **
  X = -1
*extra-debug-print*]
...
```

By default, `extra-debug-print` prints the frame on the top of the stack. The number of frames to be printed is set by the strategy `(set-debug-mode :frames <n>)`, where `<n>` is the number of frames to be printed. The value 0 suppresses this information. Furthermore, the option `:suppress <supp>`, where `<supp>` is a predicate on a list of frames used to filter the information that is printed. This predicate could be a Lisp function defined by the user, but several pre-defined predicates are already available. For example, to only enable printing from function `h`, we could do

```
Rule? (set-debug-mode :suppress (suppress-but "h"))

*extra-debug-frames*: 1
*extra-debug-files*: (/.../pvs-strategies)
(extra-debug-print ...): Printing is suppressed except from h
Debug mode: ENABLED
...

Rule? (mystrat 1)
[*extra-debug-println*] **HERE**, FNUM = 1, (> FNUM 0) = T

[*extra-debug-print*
[TOP FRAME] (H 1)

** H **
  X = 1
*extra-debug-print*]
...

Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL

[*extra-debug-print*
[TOP FRAME] (H -1)

** H **
  X = -1
*extra-debug-print*]
...
```

Notice that only calls to `(extra-debug-print ...)` are affected by this setting. Let's assume that we want to only enable printing of `h`, when it appears in the scope of `f`. To do this, we need to provide a maximum depth for the search, i.e., the number of frames counting from the top one. In this example, we will use 2 frames. As filtering function, we could use

```
Rule? (set-debug-mode :frames 2 :suppress (suppress-not (suppress-and (suppress-from "h") (suppress-in-scope "f"))

*extra-debug-frames*: 2
*extra-debug-files*: (/.../pvs-strategies)
(extra-debug-print ...): Printing is suppressed (except (from h and within the scope of f up to *extra-debug-frames* (2)))
Rule? (mystrat 1)
[*extra-debug-println*] **HERE**, FNUM = 1, (> FNUM 0) = T

[*extra-debug-print*
[TOP FRAME] (H 1)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1)
                    (EXPR 'EXISTS (b: real | b > 0): b < a)
                    (DUMMY 'NIL))
               (F FNUM))
             #<NULL-LEXENV>)

** H **
  X = 1
*extra-debug-print*]
...

Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL

...
```

In the case of `(mystrat -1)`, printing from `h` is disabled since in that instance `h` doesn't appear in the scope of `f`.

## Suppress-Printing Functions

Other pre-defined suppress printing functions are:

* `suppress-all`: Suppresses all printing
* `suppress-none`: Suppresses nothing
* `(suppress-from f1 .. fn)`: Suppresses printing from `f1`,.., and `fn`
* `(suppress-but f1 .. fn)`: Suppresses printing except from `f1`,.., or `fn`
* `(suppress-in-scope f1 .. fn)`: Suppresses printing within the scope of `f1`,.., and `fn` up to `*extra-debug-frames*`
* `(suppress-out-scope f1 .. fn)`: Suppresses printing except outside the scope of `f1`,.., or `fn` up to `*extra-debug-frames*` 
 
Furthermore, suppress predicates can be combined using `(suppress-and <supp1> .. <suppn>)`, `(suppress-or <supp1> .. <suppn>)` and `(suppress-not <supp>)`. If provided by the developer, a suppress predicate should be a Lisp function on a list of current frames, up to `*extra-debug-frames*`, used to suppress printing of `(extra-debug-print ...)`.

The options `:frames` and `:suppress` can be dynamically changed using the strategy `(set-debug-mode :frames <frames> :suppress <suppress-function>)` and don't require loading files afterwards.

## Compilation Error

Calling the strategy `mystrat` instrumented with `extra-debug-print` or `extra-debug-println` without enabling debug mode will result in the following error:

```
Rule? (mystrat 1)
...
%%%
%%% This code requires the feature :extra-debug. To avoid this error at compile time, 
%%% use the conditional compilation directive #+extra-debug before (extra-debug-print ...) 
%%% and (extra-debug-println ...).
%%%
```

This behavior is intentional to discourage the presence of debug code in the code base. The error could be avoided by adding the compilation directive `#+extra-debug` before function calls to `(extra-debug-print ...)` and `(extra-debug-println ...)`, e.g.,

```
 (let ((expr (extra-get-formula fnum))
	#+extra-debug
       	(dummy (extra-debug-print "**HERE**" fnum expr))
  ...)
```

## Detailed Documentation

For detailed documentation of the strategies mentioned in this document, please use the `help` proof command. For instance, `(help set-debug-mode)`.
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

The strategies `(set-debug-mode [<files>] :mode
[toggle|enable|disable])`, `(enable-debug-mode [<files>])` and
`(disable-debug-mode [<files>])` enable/disable, respectively, debug
mode. Once debug mode is enabled, the lisp functions
`(extra-debug-println [string|fmted-string|expr]*)`,
`(extra-debug-print [string|fmted-string|expr]*)`, and
`(extra-debug-break [string|fmted-string|expr]*)
can be used to
print messages, formatted messages, or lisp expressions and their
values. A `fmted-string` has the form `(:fmt <string> <e1> .. <en>)`
and will be printed as `(format t <string> <e1> .. <en>)`.  A Lisp
expression `<expr>` will be printed as `EXPR = VAL` where `EXPR`
is the string representation of `<expr>` and `VAL` is its Lisp value.

These functions will fail if debug mode is not enabled. Therefore, the
typical way to use these functions is to add them to the lisp file
containing the code to be debugged, e.g., `myfile.lisp`, and then to
enable debug mode in that file through the strategy
`(enable-debug-mode "myfile.lisp")`. To discourage having debug code
in the PVS binaries, function calls `(extra-debug-println ..)` or
`(extra-debug-println ..)` will **fail** at compilation time. To avoid
the compilation error, a conditional compilation directive
`#+extra-debug` could be use, e.g.,


```
#+extra-debug (extra-debug-println ...)
```

The function `(extra-debug-println ..)` prints one line of
information. The function `(extra-debug-print ..)` also prints back
the stack of execution frames up to a user-specified number of frames, and
allows a user to suppress messages via programmatic high-level
functions.
The function  `(extra-debug-break ..)` behaves exactly as
`(extra-debug-print ..)` but breaks into the debugger before 
it ends its execution.

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

The function `extra-debug-print` is more verbose than `extra-debug-println` and allows for more control on the information that is printed. Let's assume that `mystrat` calls functions `f` and `g`, both of which call function `h`, and all functions `f`, `g`, and `h` have calls to `extra-debug-print`, e.g.,

```
(defstep mystrat (fnum)
  ...
  (let ((expr  (extra-get-formula fnum))
	(dummy (extra-debug-println "**HERE**" fnum (> fnum 0))))
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
** G **
  X = -1
  
[TOP FRAME] (G -1)

[TOP STRAT] #<strat: (GG -1)>
[STRAT  -1] #<strat: (MYSTRAT -1)>
*extra-debug-print*]

[*extra-debug-print*
** H **
  X = -1
[TOP FRAME] (H -1)

[TOP STRAT] #<strat: (GG -1)>
[STRAT  -1] #<strat: (MYSTRAT -1)>
*extra-debug-print*]
...
```

By default, `extra-debug-print` prints the frame at the top of the
execution stack and the stack of strategies. The number of frames to
be printed is set by the strategy `(set-debug-mode :frames <n>)`,
where `<n>` is the number of frames to be printed. The value 0
suppresses this information and a negative value means all frames.

The command `set-debug-mode` also accepts 
the option `:suppress <supp>`, where `<supp>` is a lambda function on
an association list of tags and their respective values that is called
by `(extra-debug-print ...)` and `(extra-debug-break ...)` to decide
if their actions  should be suppressed or no. The association list has
the tags `:frame-stack`, associated to the current stack of frames,
and `:strategy-stack`,  associated to the curent stack of strategies,
in addition to those defined by the user. This function could be a Lisp
lambda expression defined by the user, but several pre-defined predicates are
already available. For example, to only enable printing from function
`h`, we could write

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
** H **
  X = 1
  
[TOP FRAME] (H 1)

[TOP STRAT] #<strat: (FF 1)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]
...

Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL

[*extra-debug-print*
** H **
  X = -1
  
[TOP FRAME] (H -1)

[TOP STRAT] #<strat: (GG -1)>
[STRAT  -1] #<strat: (MYSTRAT -1)>
*extra-debug-print*]
...
```

Only calls to `(extra-debug-print ...)` and `(extra-debug-break ...)`
are affected by the `:suppress` setting. The function
`(extra-debug-println ...)`always
prints information.

Now, let's assume that we want to only enable printing of `h`, when it
appears in the scope of `f`. The default depth for the search is 1,
i.e., the number of stack frames counting from the top but we
can change this value. In this example, we will use 2 frames.
As filtering function, we could use

```
Rule? (set-debug-mode :frames 2 :suppress (suppress-not (suppress-and (suppress-from "h") (suppress-in-scope "f"))))

*extra-debug-frames*: 2
*extra-debug-files*: (/.../pvs-strategies)
(extra-debug-print ...): Printing is suppressed (except (from h and within the scope of f up to *extra-debug-frames* (2)))
Rule? (mystrat 1)
[*extra-debug-println*] **HERE**, FNUM = 1, (> FNUM 0) = T

[*extra-debug-print*
** H **
  X = 1
[TOP FRAME] (H 1)
[FRAME  -1] (SB-INT:SIMPLE-EVAL-IN-LEXENV
             (LET* ((FNUM '1))
               (F FNUM))
             #<NULL-LEXENV>)

[TOP STRAT] #<strat: (FF 1)>
[STRAT  -1] #<strat: (MYSTRAT 1)>
*extra-debug-print*]
...

Rule? (mystrat -1)
[*extra-debug-println*] **HERE**, FNUM = -1, (> FNUM 0) = NIL

...
```

In the case of `(mystrat -1)`, printing from `h` is disabled since in that instance `h` doesn't appear in the scope of `f`.

The options `:frames` and `:suppress` can be dynamically changed using
the strategy `(set-debug-mode :frames <frames> :suppress
<suppress-function>)` and don't require loading files afterwards.

### Suppress Printing Functions

Other pre-defined suppress printing functions are:

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
* `(suppress-unless expr)`: Suppress unless lisp expression `expr` holds

Furthermore, suppress predicates can be combined using `(suppress-and
<supp1> .. <suppn>)`, `(suppress-or <supp1> .. <suppn>)` and
`(suppress-not <supp>)`. The global variable `*extra-debug-suppress*`
has the current suppress function, which can be combined to form a new one.

### Technical Notes
* The stack of frames is provided by the SBCL function `(sb-debug:list-backtrace ...)`,
   Functions that are inlined by the compiler don't appear in the stack. Therefore, if `FUN`
   is the name of a function inlined by the compiler, `(suppress-in-scope FUN` never holds and
   `(suppress-out-scope FUN)` always holds.
* The stack of strategies is provided by PVS global variable `*proof-strategy-stack*`.
   Glass-box strategies are inlined by the theorem prover, so they don't appear in
   the stack. Therefore, if `STRAT` is the name a glass-box strategy, `(suppress-in-scope-strat STRAT)`
   never holds and `(suppress-out-scope-strat STRAT)` always holds.
 * `EXPR` in `(suppress-when EXPR)` and `(suppress-unless EXPR)` can be an arbitrary lisp code
   globally scoped, i.e., it can use global variables.

### Compilation Error

Calling the strategy `mystrat` instrumented with `extra-debug-print` or `extra-debug-println` without enabling debug mode will result in the following error:

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

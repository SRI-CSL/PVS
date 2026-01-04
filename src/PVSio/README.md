# Exceptions

Exceptions can return a value of any type. The type of this value is
provided through the theory parameter `ExceptionType`.  Exceptions are associated to a formatter, i.e.,
a function of type `[ExceptionType->string]` that defines the message that is printed when the exception is raised (i.e., thrown but not caught).

The basic operations on exceptions are creation, via `make_exception`, throwing, via
`throw`, and catching, via `catch`.

## Creating Exceptions
An exception that returns a natural number can be created as follows:

```pvs
MyException0 : Exception[nat]
```
In this case, since the declaration of the exception does not have body, it will
be associated with a default formatter that prints the name of the exception, i.e.,
`MyException0`, and the returned value, which is provided when the exception is thrown.

A user-defined formatter can be provided  using the PVSio function `make_exception`, e.g.,

```pvs
MyException1 : Exception[nat] = make_exception(LAMBDA(n:nat):<MSSG>)
```

Typically, `<MSSG>` is a string created using the PVSio function
`format`, e.g.,

```pvs
  MyException1 : Exception[nat] = make_exception(LAMBDA(n:nat):
    format("The thrown exception value is ~a.",n))
```

 If `<MSSG>` is an unformatted message, then the exception could be simply defined
```pvs
MyException2 : Exception[nat] = "An exception value was thrown."
```

 Exceptions are hierarchical. The constant `PVSioException`, of type
 `Exception`, is at the root  of the hierarchy. An exception catches
 all thrown exceptions that are below
 in the hierarchy. Therefore,  the exception `PVSioException` can be
 used to catch all exceptions.
 For example, `MyException0`, `MyException1`, and `MyException2` can be caught by
themselves and by `PVSioException`.

The following declaration
```pvs
MyException1a : Exception[nat] = make_exception(MyException1,<FMTER>)
```
creates an exception below `MyException1a` with a formatter `<FMTER>`, i.e., a function of type
 `[ExceptionType->string]`. This parameter could also be provided as  an unformatted string,
 or  simply be omitted. The exception
 `MyException1a` can be caught by itself, by `MyException1`, or by
 `PVSioException`. Let's assume that `MyException1a` was defined as
 follows.
```
MyException1a : Exception[nat] = make_exception(MyException1)
```
 In this case, the formatter of `MyException1a` will be inherited from `MyExcpetion1`.

A `SimpleException` is an `Exception` that does not return a value. It
is defined just as an exception but does not require an
`ExceptionType`. Furthermore, the formatter, if provided, is simply a string,
e.g.,
```
  MySimpleException: SimpleException
```

## Throwing Exceptions
The function `throw(<EXC>,<VAL>)` throws an exception `<EXC>` with a value
`<VAL>`. The exception `<EXC>` should have
the type `Exception[<T>]`, where `<T>` is the type of `<VAL>`. For example,
the following code throws `MyException0` at the first element of the
list `l` that is zero. The exception value is the index of the element
in the list.

```
<PVSio> LET l : list[int] = (: 2, 1, 0, 4, -1 :) IN
   FORALL(i:below(length(l))):
    IF nth(l,i) = 0 THEN throw(MyException0,i) ELSE TRUE ENDIF;

[PVSioException::MyException0] 2
```

The function `throw` has two type parameters, which are
often automatically inferred. The first one is the `ResultType`, i.e.,
the type of the throw expression, in the case of the expression above,
`throw(MyException0,i)` is a boolean. The second type is
`ExceptionType`, i.e., the type of the value thrown by the exception,
in this case `nat`.  Therefore, the throw expression in expanded form
is `throw[bool,nat](MyException0,i)`.

Throwing `MyException1`, `MyException1a`, `MyException2` prints
the following messages.
```
<PVSio> LET l : list[int] = (: 2, 1, 0, 4, -1 :) IN
   FORALL(i:below(length(l))):
     IF nth(l,i) = 0 THEN throw(MyException1,i) ELSE TRUE ENDIF;

[PVSioException::MyException1] The exception value is 2.

 LET l : list[int] = (: 2, 1, 0, 4, -1 :) IN
   FORALL(i:below(length(l))):
     IF nth(l,i) = 0 THEN throw(MyException1a,i) ELSE TRUE ENDIF;

[PVSioException::..::MyException1a] The exception value is 2.

<PVSio> LET l : list[int] = (: 2, 1, 0, 4, -1 :) IN
   FORALL(i:below(length(l))):
     IF nth(l,i) = 0 THEN throw(MyException2,i) ELSE TRUE ENDIF;

[PVSioException::MyException2] An exception value was thrown.

```
In the latter case, the index is not printed since the formatter was a
simple string. It is possible to overwrite the formatter, when the
exception is thrown, e.g.,

```
<PVSio> LET l : list[int] = (: 2, 1, 0, 4, -1 :) IN
   FORALL(i:below(length(l))):
     IF nth(l,i) = 0 THEN throw(MyException2,i,format("The index with a zero value is ~a.",i))
	 ELSE TRUE ENDIF;

[PVSioException::MyException2] The index with a zero value is 2.
```

A `SimpleException` could be thrown as follows.

 - `throw(<MSSG>)` : Throw a pre-defined `PVSioSimpleException`,
	which has  type `SimpleException`.

 - `throw(<EXC>)`: Throw `<EXC>`, which is a user-defined
    `SimpleException`, using the default formatter.

 - `throw(<EXC>,<MSSG>)`: Throw `<EXC>`, which is a user-defined
    `SimpleException`, and overwrite the formatter with the message
    `<MSSG>`

## Catching Exceptions
The function `catch(<EXC>,<EXPR>,<HANDLER>)` catches an exception
`<EXC>` possibly thrown by `<EXPR>`. If the exception is thrown,
uses `<HANDLER>` to address the exception. Otherwise,
returns the evaluation of `<EXPR>`. Like the function `throw`, the
function `catch` has two type parameters, which are ofter inferred
automatically. The first type, i.e., `ResultType`, is the type of the
`catch` expressions. The second type, i.e.,`ExceptionType`, is the
type of the value returned by the exception `<EXC>`. Therefore,
`<EXPR>` has the type `ResultType` and  `<EXC>` has the type
`Exception[ExceptionType]`. Furthermore, `<HANDLER>` has the type
`[ExceptionType->ResultType]`. If `<HANDLER>` is a constant
function `LAMBDA(x:ReultType):<CNST>`, the constant `<CNST>` could be
provided instead of `<HANDLER>`.

For example, the following (impure) function, returns the index of the
first element of the list that is equal to zero. It returns the length
of the list if no such element exists.

```
  find_zero(l:list[int]) : nat =
    catch(
      MyException1a,
        LET find =
          FORALL(i:below(length(l))):
          IF nth(l,i) = 0 THEN throw(MyException1a,i) ELSE TRUE ENDIF
        IN length(l),
        LAMBDA(n:nat):n)

<PVSio> find_zero((:3,-1,2,0,1:));
==>
3

<PVSio> find_zero((:3,-1,2,10,1:));
==>
5
```

In the function above, the exception `MyException1a` could be caught
by `PVSioException`, `MyException1`, or `MyException1a`. However,
`MyException2` does not catch `MyException1a` as illustrated by the
following example.

```
<PVSio> let l : list[int] = (:3,-1,2,0,1:) IN
    catch(
      MyException2,
        LET find =
          FORALL(i:below(length(l))):
          IF nth(l,i) = 0 THEN throw(MyException1a,i) ELSE TRUE ENDIF
        IN length(l),
        LAMBDA(n:nat):n);

[PVSioException::..::MyException1a] The exception value is 3.

```

The first parameter `catch` could be a list of
exceptions. This allows for catching exceptions of the same type that are not in the
same hierarchy as illustrated by the following contrived example.

```
  find_zero_exc(l:list[int]) : nat =
    catch(
      (: MyException1,MyException2 :),
        LET find =
          FORALL(i:below(length(l))):
          IF nth(l,i) = 0 THEN throw(MyException1a,i) ELSE TRUE ENDIF
        IN throw(MyException2,length(l)),
        LAMBDA(n:nat):n)

<PVSio> find_zero_exc((:3,-1,2,0,1:));
==>
3

<PVSio> find_zero_exc((:3,-1,2,10,1:));
==>
5
```

A `SimpleException` can be caught by `catch(<EXPR>,<CNST>)`,
where `<EXPR>` is an expression, possibly throwing a `SimpleException`,
and `<CNST>` is an expression of the same type as `<EXPR>` that is
returned if the exception is caught.

##  Core Exceptions
It is possible to create exceptions that are not in the
`PVSioException` hierarchy. Indeed, PVSio support user-defined
exception hierarchies. Root exceptions are called core exceptions and
they are created as follows.
```
  ACoreException : CoreException[nat]
```
Exceptions in that hierarchy of `ACoreExcpetion` can be created  as
usual.
```
  MyExceptionC : Exception[nat] = make_exception(ACoreException)
```

Core exceptions and exceptions in the hierarchy of core exceptions
behave exactly as regular exceptions. The only difference is that they
are caught by default by the root of the hierarchy instead of the
the pre-defined `PVSioException`, e.g.,

```
<PVSio> throw[nat,nat](ACoreException,2);

[ACoreException] 2

<PVSio> throw[nat,nat](MyExceptionC,2);

[ACoreException::MyExceptionC] 2

<PVSio> catch[nat,nat](PVSioException[nat],throw[nat,nat](MyExceptionC,2),0);

[ACoreException::MyExceptionC] 2

<PVSio> catch[nat,nat](ACoreException,throw[nat,nat](MyExceptionC,2),0);
==>
0
```

# Semantic Attachments

Semantic attachments are Common Lisp functions that implement
uninterpreted PVS functions. They can be defined in a working
directory inside a file called `pvs-attachments` or in the home directory's
file `~/.pvs-attachments`. PVSio automatically loads semantic attachment
files at startup. During a PVS session, the Emacs command
`M-x pvs-load-attachments` reloads all
semantic attachment files available in the current context.
This command is useful when semantic attachment files have been modified during
the PVS session.

## Defining Semantic Attachements
Semantic attachments are written using the special
macros `attachments` and `defattach`. The macro `attachments` is
used to define semantic attachments for a given theory, e.g.,

```Lisp
(attachments |<theory-id>|
  ...
  (defattach |<fun-id>| (<parameters>)
	  [ <docstr> ]
	  <body>)
  ...
)
```

In the declaration above, `<theory-id>` is the symbol identifying a
PVS theory and `<fun-id>` is an uninterpreted PVS function in
`<theory-id>`. If `<theory-id>` is ambiguous in the working context,
the theory identifier should fully qualified, i.e.,`<lib>@<theory-id>`.
In contrast to Common Lisp, PVS is case sensitive. Therefore,
the PVS theory and function identifiers should be surrounded by the character
bar, i.e., `|`, to preserve their casing. The macro `attachments`
removes all previous semantic attachments defined for `theory-id`. To
define a semantic attachment without removing other semantic
attachments in the theory, the macro `defattach` should appear outside `attachments`
with the name of the uninterpreted PVS function fully qualified by the theory, i.e.,

```Lisp
(defattach |<theory-id>.<fun-id>| (<parameters>)
	  [ <docstr> ]
	  <body>)
```

In both cases, the macro `defattach` attaches the Lisp form
`<body>` as the definition of the PVS function `<fun-id>` declared
in `<theory-id>`. The documentation string `<docstr>` is
optional. If provided, it is used by the Emacs commands
`M-x help-pvs-attachment (C-c C-h a)` and
`M-x help-pvs-theory-attachments (C-c C-h t)` to display a help
message. Typically, `<parameters>` is a list of
names corresponding to the formal
parameters of the PVS function that the semantic attachment is
associated to.
If `<parameters>` is empty, `<fun-id>` represents an
uninterpreted PVS constant.

To associate a semantic attachment to a PVS uninterpreted function, PVSio uses
the fully qualified name of the attachment and the length of the list of formal parameters, which
should correspond to the name and declared arity of the PVS function.
However, as explained in the following section, associating a semantic
attachment to an overloaded PVS declaration
may require type annotations in the semantic attachment parameters.

Semantic attachments for the uninterpreted function
`sqrt`, of arity 1, and the uninterpreted  constant `pi`, of arity 0,
defined in the NASALib theories `reals@sqrt` and `trig@pi_def`, respectively, can
be defined as follows.

```Lisp
(defattach |reals@sqrt.sqrt| (x)
  "Square root of X"
  (rational (sqrt x)))

(defattach |trig@pi_def.pi| ()
  "Number π"
   (rational pi))
```

In the previous example,  `rational` is the Common lisp function that
produces a rational number from a floating-point one, `sqrt` is the
Common Lisp floating-point function square root, and `pi`
is the Common Lisp floating-point constant representing `π`.

It should be noted that the arity of a PVS function is not
unique.  In the following example, the function `f` is applied to
zero, one, and two arguments.

```pvs
point : THEORY
BEGIN
  Point : TYPE = [real,real]

  Zero : Point = (0,0)

  f(p:Point) : real

  hof(ho:[Point->real]): real = ho(Zero)

  g : real =
    hof(f) + f(Zero) + f(1,1)

END point
```

To provide a semantic attachment for `f`, the length of the list of parameters should
correspond to the declared arity, i.e., 1.

```Lisp
(defattach |point.f| (p)
	(let ((x (aref p 0))
		  (y (aref p 1)))
		 ...))
```

Any PVS object, represented in Common Lisp, could be
passed as parameter and returned by a semantic attachement. The Common
Lisp representation of PVS objects used by semantic attachments is suitable
for evaluation, so it is different from the CLOS representation used
by the theorem prover. In
practice, it is better to restrict the types of parameters and returned
values to PVS built-in types such as numbers, booleans, strings,
characters, and lists, which are self-denoted.
Other PVS types, such as tuples, records, and abstract datatypes, are represented using
Common Lisp vectors.

## Overloading Semantic Attachments

PVSio fully supports PVS overloading. For example, the PVSio theory `stdstr`
contains the following PVS declarations of `tostr` that convert, respectively, real
numbers to string and Boolean values to string:

```pvs
tostr(r:real): string = real2str(r)
tostr(b:bool): string = bool2str(b)
```

The functions `real2str` and `bool2str` are appropriately defined via
semantic attachments.

```
<PVSio> tostr(1/7);
==>
"0.142857"

<PVSio> tostr(EXISTS (x:below(5)): x*x=9);
==>
"TRUE"

```

PVS provides a powerful overloading feature, where two different
functions defined in the same context may have the same name as long
as their types are different. This poses some challenges when associating
a semantic attachment to a PVS declaration that is
overloaded. Consider the following example.

```pvs
th : THEORY
BEGIN
  f(x:real) : nat
  f(x:real,y:bool) : nat
END th
```

In this example, the declared
arity could be used to distinguish between the two declarations
of `f`. Therefore, there is no ambiguity in the following semantic
attachment definitions.

```Lisp
(attachments |th|
  (defattach |f| (x) ...)
  (defattach |f| (x y) ...)
)
```

However, consider the following valid PVS theory declaration.
```
th : THEORY
BEGIN
  f(x:real) : nat
  f(x:real,y:bool) : nat
  f(x:nat,y:bool)  : nat
  f(x:nat,y:bool)  : bool
 END th
```

Without type annotations at the semantic attachment level,
it would be impossible to define semantic attachments for these
declarations. In contrast to Common Lisp, PVS is a strongly typed
language that supports predicate sub-typing, dependent types,
and prenex polymorphism. The PVS type system is so rich that
type-checking and type equality are undecidable. Bringing all this richness to semantic
attachments, which are essentially untyped Common Lisp code,
woud add too much overhead. Instead, semantic attachments allow for
optional type name annotations on the parameters and return type
of their associated PVS function declarations.  The user only needs to
provide enough annotations to uniquely associate each attachment to a
PVS declaration. For example, the following semantic attachments are
unambiguously associated to the overloaded definitions of `f` in `th`.

```Lisp
(attachments |th|
  (defattach |f| (x) ...)
  (defattach |f| ((x |real|) y) ...)
  (defattach |f| (x y (:return |nat|)) ...)
  (defattach |f| (x y (:return |bool|)) ...)
  )
```

As illustrated by the previous example, type annotations are provided
in the list `<parameters>` using the special form `(<VAR>
<TYPE-ID>)`, where `<TYPE-ID>` is a valid PVS type identifier. As
usual, the type identifier should be surrounded by bars to preserve casing.
The variable symbol `<VAR>` could also be the keyword `:return`, which
represents the value returned by the PVS function. This idiom is used
to annotate the type of the range of the PVS function.

It should be
noted that PVSio literally uses the declared type name, without performing any
type inference. For instance, the attachments

```Lisp
(defattach |th.h| ((b |bool|)) ...)

(defattach |th.h| ((i |int|)) ...)
```

are not associated to the following PVS definitions in the theory `th`
even although `boolean` and `bool` are the same type and `int` is a
supertype of `(odd?)`.

```
  h(b:boolean):nat
  h(i:(odd?)):nat
```

Providing unambiguous semantic attachments for these definitions may
require rewriting the PVS definitions into semantically equivalent ones as illustrated below.

```
th : THEORY
BEGIN
...
  h(b:boolean):nat

  Odd : TYPE = (odd?)
  h(i:Odd):nat
  ...
END th
```

The following semantic attachments are unambiguously associated to the
previous definitions.

```Lisp
(defattach |th.h| ((b |boolean|)) ...)

(defattach |th.h| ((i |Odd|)) ...)
```

## Accessing Theory Parameters and Declaration Formals

Semantic attachements provide access to declaration formals, including
theory parameters, and declaration types through the special list
forms `(<VAR> :decl-form <PARAM>)` and `(<VAR> :decl-type)` in
`<parameters>`. The former declares the variable `<VAR>` and binds it
to the value of the declaration formal, or theory parameter, `<PARM>`. Since
`<PARAM>` is a PVS identifier, it is should be surrounded by bars to
preserve casing. The latter declares the variable `<VAR>` and binds it
to the declaration type. In contrast to  PVS constants, which
are presented using Common Lisp values, PVS types in
semantic attachments are CLOS objects. For instance, the semantic
attachment for the uninterpreted PVS function `type2str`, which returns
the string representation of a PVS type, declared in the
prelude library `stdpvs`
```pvs
  type2str[T:TYPE]: string
```
is defined as follows.
```Lisp
(defattach |stdpvs.type2str| ((the-type :decl-formal |T|))
  "Returns the string value of the type T"
  (format nil "~a" (or (print-type the-type) the-type)))
```

## Evaluating PVS Expressions from Semantic Attachments

Executable PVS functions can be called
from semantic attachments using the Lisp form
`(pvsio-funcall <PVSFUN> <ARG1> ... <ARGn>)`, where `<PVSFUN>` is the string name of
a PVS function, which should be available in the typing context,
and `<ARG1> .. <ARGn>` are
its arguments, which are represented in Common Lisp.
For instance, consider the PVS function `sqrt_fast_approx`
defined in the NASALib theory `fast_approx@fast_approx`

```pvs
  sqrt_fast_approx(x, eps): nnreal = ...
```

This PVS function has 2 parameters: a non-negative number `x` and a positive
error `eps`. It computes a rational number that is provably within
`eps` of the square root of `x`. It can be called from a semantic attachment as follows,
where `x` is bound to `2` and `eps` is bound to `0.00001`.

```Lisp
(pvsio-funcall "fast_approx.sqrt_fast_approx" 2 (expt 10 (- 5)))
```
Executable PVS expressions can be called from semantic attachements
using the Lisp form `(pvsio-eval-lisp <EXPR>)`, where `<EXPR>` is the
string representation of a PVS expressions, which should be well-typed
in the typing context. For example, the PVS
expression `fast_approx.sqrt_fast_approx(2,10^(-5))` could be
evaluated as follows.
```Lisp
(pvsio-eval-lisp "fast_approx.sqrt_fast_approx(2,10^(-5))")
```
In both examples, the computed rational is `665857/470832`, wich is approx.  1.414213.

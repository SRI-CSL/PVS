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

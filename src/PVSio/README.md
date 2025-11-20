# Exceptions

Exceptions can return a value of any type. The type of this value is provided through the theory parameter `ExceptionType`.  The concrete type `SimpleException` can be used for exceptions that don't return a value. Exceptions are associated to a formatter, i.e.,
a function of type `[ExceptionType->string]` that defines the message that is printed when the exception is raised (i.e., thrown but not caught).

The basic operations on exceptions are creation, via `make_exception`, throwing, via
`throw`, and catching, via `catch`.

## Creation 
An exception that returns a natural number can be created as follows:

```pvs
MyException0 : Exception[nat]
```
In this case, since the declaration of the exception doesn't have body, it will
be associated with a default formatter that prints the name of the exception, i.e.,
`MyException0`, and the returned value, which is provided when the exception is thrown.

A user-defined formatter can be provided  using the PVSio function `make_exception`, e.g.,

```pvs
MyException1 : Exception[nat] = make_exception(LAMBDA(t:nat):<MSSG>)
```

Typically, `<MSSG>` is a string created using the PVSio function `format`.
If `<MSSG>` is an unformatted message, then the exception could be simply defined

```pvs
MyException1 : Exception[nat] = <MSSG>
```

Exceptions are hierarchical. The constant `PVSioException`, of type `Exception`, is at the root of the hierarchy. An exception catches all thrown exceptions that are below in the hierarchy. Therefore,  the exception `PVSioException` can be used to catch all exceptions. For example, `MyException0` and `MyException1` can only be caught by
themselves and by `PVSioException`. 

The following declaration
```pvs
MyException2 : Exception[nat] = make_exception(MyException1,<FMTER>)
```
creates an exception below `MyException1` with a given formatter `<FMTER>`,
which can be function of type `[ExceptionType->string]`, an unformatted string, or be be omitted.  The exception `MyException2` can be caught by itself, by `MyException1`, or by `PVSioException`.

## Throwing

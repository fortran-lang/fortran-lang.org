---
layout: book
title: transfer
permalink: /learn/intrinsics/TRANSFER
---
## __Name__

__transfer__(3) - \[TYPE:MOLD\] Transfer bit patterns


## __Syntax__
```fortran
result = transfer(source, mold, size)
```
## __Description__

Interprets the bitwise representation of __source__ in memory as if it
is the representation of a variable or array of the same type and type
parameters as __mold__.

This is approximately equivalent to the C concept of \*casting\* one
type to another.

## __Arguments__

  - __source__
    : Shall be a scalar or an array of any type.

  - __mold__
    : Shall be a scalar or an array of any type.

  - __size__
    : (Optional) shall be a scalar of type _integer_.

## __Returns__

The result has the same type as __mold__, with the bit level representation
of __source__. If __size__ is present, the result is a one-dimensional array of
length __size__. If __size__ is absent but __mold__ is an array (of any size or
shape), the result is a one-dimensional array of the minimum length
needed to contain the entirety of the bitwise representation of __source__.
If __size__ is absent and __mold__ is a scalar, the result is a scalar.

If the bitwise representation of the result is longer than that of
__source__, then the leading bits of the result correspond to those of
__source__ and any trailing bits are filled arbitrarily.

When the resulting bit representation does not correspond to a valid
representation of a variable of the same type as __mold__, the results are
undefined, and subsequent operations on the result cannot be guaranteed
to produce sensible behavior. For example, it is possible to create
_logical_ variables for which __var__ and .not. var both appear to be true.

## __Examples__

Sample program:
```fortran
program demo_transfer
use,intrinsic :: iso_fortran_env, only : int32, real32
integer(kind=int32) :: i = 2143289344
real(kind=real32)   :: x
character(len=10)   :: string
character(len=1)    :: chars(10)
   x=transfer(i, 1.0)    ! prints "nan" on i686
   ! the bit patterns are the same
   write(*,'(b0,1x,g0)')x,x ! create a NaN
   write(*,'(b0,1x,g0)')i,i

   ! a string to an array of characters
   string='abcdefghij'
   chars=transfer(string,chars)
   write(*,'(*("[",a,"]":,1x))')string
   write(*,'(*("[",a,"]":,1x))')chars
end program demo_transfer
```
Results:
```text
   1111111110000000000000000000000 NaN
   1111111110000000000000000000000 2143289344
   [abcdefghij]
   [a] [b] [c] [d] [e] [f] [g] [h] [i] [j]
```
## __Comments__

_Joe Krahn_: Fortran uses __molding__ rather than __casting__.

Casting, as in C, is an in-place reinterpretation. A cast is a device
that is built around an object to change its shape.

Fortran TRANSFER reinterprets data out-of-place. It can be considered
__molding__ rather than casting. A __mold__ is a device that
confers a shape onto an object placed into it.

The advantage of molding is that data is always valid in the context
of the variable that holds it. For many cases, a decent compiler should
optimize TRANSFER into a simple assignment.

There are disadvantages of this approach. It is problematic to define a
union of data types because you must know the largest data object, which
can vary by compiler or compile options. In many cases, an EQUIVALENCE
would be far more effective, but Fortran Standards committees seem
oblivious to the benefits of EQUIVALENCEs when used sparingly.

## __Standard__

Fortran 90 and later

###### fortran-lang intrinsic descriptions

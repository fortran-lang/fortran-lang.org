---
layout: book
title: int
permalink: /learn/intrinsics/INT
---
## __Name__

__int__(3) - \[NUMERIC:TYPE\] Convert to integer type by truncating towards zero

## __Syntax__
```fortran
result = int(a, kind)

 integer(kind=KIND) elemental function int(a,kind)
 TYPE(kind=KIND),intent(in),optional :: a
 integer,optional :: kind
```
## __Description__

Convert to integer type by truncating towards zero.

## __Arguments__

  - __a__
    : Shall be of type _integer_, _real_, or _complex_ or a BOZ-literal-constant.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

    If not present the returned type is that of default integer type.

## __Returns__

returns an _integer_ variable or array applying the following rules:

 __Case__:

 1.  If __a__ is of type _integer_, __int__(a) = a

 2.  If __a__ is of type _real_ and __|a| \< 1, int(a)__ equals __0__. If __|a| \>=
     1__, then __int(a)__ equals the integer whose magnitude does not exceed
     __a__ and whose sign is the same as the sign of __a__.

 3.  If __a__ is of type _complex_, rule 2 is applied to the _real_ part of __a__.

 4.  If _a_ is a boz-literal constant, it is treated as an _integer_
     with the _kind_ specified.

     The interpretation of a bit sequence whose most significant bit is
     __1__ is processor dependent.

The result is undefined if it cannot be represented in the specified integer type.

## __Examples__

Sample program:

```fortran
program demo_int
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
implicit none
integer :: i = 42 
complex :: z = (-3.7, 1.0)
real :: x=-10.5, y=10.5

   print *, int(x), int(y)

   print *, int(i)

   print *, int(z), int(z,8)
   ! elemental
   print *, int([-10.9,-10.5,-10.3,10.3,10.5,10.9])
   ! note int(3) truncates towards zero

   ! CAUTION:
   ! a number bigger than a default integer can represent
   ! produces an incorrect result and is not required to
   ! be detected by the program. 
   x=real(huge(0))+1000.0
   print *, int(x),x
   ! using a larger kind
   print *, int(x,kind=int64),x

   print *, int(B"111111111111111111111111111111111111111111111111111111111111111",kind=int64)
   print *, int(O"777777777777777777777",kind=int64)
   print *, int(Z"7FFFFFFFFFFFFFFF",kind=int64)

end program demo_int
```
  Results:
```text
         -10          10
          42
          -3                    -3
         -10         -10         -10          10          10          10
 -2147483648  2.1474847E+09
            2147484672  2.1474847E+09
   9223372036854775807
   9223372036854775807
   9223372036854775807
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (@urbanjost)

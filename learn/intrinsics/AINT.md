---
layout: book
title: aint
permalink: /learn/intrinsics/AINT
---
## __Name__

__aint__(3) - \[NUMERIC\] Truncate to a whole number
(GFDL)

## __Syntax__
```fortran
result = aint(x, kind)
```
## __Description__

__aint(x, kind)__ truncates its argument to a whole number.

## __Arguments__

  - __x__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _real_ with the kind type parameter of the
argument if the optional __kind__ is absent; otherwise, the kind type
parameter will be given by __kind__. If the magnitude of __x__ is less than one,
__aint(x)__ returns zero. If the magnitude is equal to or greater than
one then it returns the largest whole number that does not exceed its
magnitude. The sign is the same as the sign of __x__.

## __Examples__

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real ::  x4
real(kind=real64) :: x8
   x4 = 1.23456E0_4
   x8 = 4.3210_real64
   print *, aint(x4), aint(x8)
   x4 = aint(x4,kind=real64)
   x8 = aint(x8,kind=real64)
   print *, x8,x4
end program demo_aint
```
Results:
```text
   1.000000       4.00000000000000     
   4.00000000000000     4.000000 
```
## __Standard__

FORTRAN 77 and later

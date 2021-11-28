---
layout: book
title: atan
permalink: /learn/intrinsics/ATAN
---
-------------------------------------------------------------------------------
## __Name__

__atan__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function
(GFDL)

## __Syntax__

  - result = __atan(X)__

  - result = __atan(Y, X)__

## __Description__

__atan(X)__ computes the arctangent of __X__.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_; if Y is present, __X__ shall be _real_.

  - __Y__
    Shall be of the same type and kind as __X__.

## __Returns__

The return value is of the same type and kind as __X__. If __Y__ is present, the
result is identical to __atan2(Y,X)__. Otherwise, it the arc tangent of
__X__, where the real part of the result is in radians and lies in the range

    __-PI/2 \<= atan(X) \<= PI/2__

## __Examples__

Sample program:

```fortran
program demo_atan
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 2.866_real64
    x = atan(x)
end program demo_atan
```

## __Standard__

FORTRAN 77 and later for a complex argument; and for two
arguments Fortran 2008 or later

## __See Also__

[__atan2__(3)](ATAN2), [__tan__(3)](TAN)

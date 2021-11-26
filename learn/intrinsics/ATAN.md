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

  - result = __atan__(x)

  - result = __atan__(y, x)

## __Description__

__atan__(x) computes the arctangent of X.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_; if Y is present, X shall be _real_.

  - __Y__
    Shall be of the same type and kind as X.

## __Returns__

The return value is of the same type and kind as X. If Y is present, the
result is identical to __atan2__(y,x). Otherwise, it the arc tangent of
X, where the real part of the result is in radians and lies in the range

__-PI__/2 \<= __atan__(x) \<= PI/2.

## __Examples__

Sample program:

```
   program demo_atan
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
   real(kind=real64) :: x = 2.866_real64
      x = atan(x)
   end program demo_atan
```

## __Standard__

FORTRAN 77 and later; for a complex argument and for two
arguments Fortran 2008 or later

## __See Also__

[__atan2__(3)](ATAN2), [__tan__(3)](TAN)

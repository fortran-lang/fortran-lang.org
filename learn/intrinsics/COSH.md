---
layout: book
title: cosh
permalink: /learn/intrinsics/COSH
---
## __Name__

__cosh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function
(GFDL)

## __Syntax__

x = __cosh__(x)

## __Description__

__cosh__(x) computes the hyperbolic cosine of X.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as X. If X is complex, the
imaginary part of the result is in radians. If X is _real_, the return
value has a lower bound of one, __cosh__(x) \>= 1.

## __Examples__

Sample program:

```
   program demo_cosh
   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
   implicit none
   real(kind=real64) :: x = 1.0_real64
      x = cosh(x)
   end program demo_cosh
```

## __Standard__

FORTRAN 77 and later, for a complex argument - Fortran 2008 or later

## __See Also__

Inverse function: __acosh__(3)

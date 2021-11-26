---
layout: book
title: bessel_y1
permalink: /learn/intrinsics/BESSEL_Y1
---
## __Name__

__bessel\_y1__(3) - \[MATHEMATICS\] Bessel function of the second kind of order 1
(GFDL)

## __Syntax__

result = __bessel\_y1__(x)

## __Description__

__bessel\_y1__(x) computes the \[\[Bessel function\]\] of the second
kind of order 1 of X.

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is _real_. It has the same kind as X.

## __Examples__

Sample program:

```
   program demo_besy1
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
     real(kind=real64) :: x = 1.0_real64
     x = bessel_y1(x)
   end program demo_besy1
```

## __Standard__

Fortran 2008 and later

## __See Also__

__bessel\_j0__(3), __bessel\_j1__(3), __bessel\_jn__(3),
__bessel\_y0__(3), __bessel\_yn__(3)

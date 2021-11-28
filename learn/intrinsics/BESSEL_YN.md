---
layout: book
title: bessel_yn
permalink: /learn/intrinsics/BESSEL_YN
---
## __Name__

__bessel\_yn__(3) - \[MATHEMATICS\] Bessel function of the second kind
(GFDL)

## __Syntax__

  - __result = bessel\_yn(n, x)__

  - __result = bessel\_yn(n1, n2, x)__

## __Description__

__bessel\_yn__(n, x) computes the Bessel function of the second
kind of order __n__ of __x__. If __n__ and __x__ are arrays, their ranks and shapes
shall conform.

__bessel\_yn__(n1, n2, x) returns an array with the Bessel
function\|Bessel functions of the first kind of the orders __n1__ to __n2__.

## __Arguments__

  - __n__
    Shall be a scalar or an array of type _integer_.

  - __n1__
    Shall be a non-negative scalar of type _integer_.

  - __n2__
    Shall be a non-negative scalar of type _integer_.

  - __x__
    Shall be a scalar or an array of type _real_; for __bessel\_yn__(n1,
    n2, x) it shall be scalar.

## __Returns__

The return value is _real_. It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besyn
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
  x = bessel_yn(5,x)
end program demo_besyn
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_j1__(3)](BESSEL_J1),
[__bessel\_jn__(3)](BESSEL_JN),
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1)

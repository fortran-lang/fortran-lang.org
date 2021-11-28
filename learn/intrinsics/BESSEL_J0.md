---
layout: book
title: bessel_j0
permalink: /learn/intrinsics/BESSEL_J0
---
## __Name__

__bessel\_j0__(3) - \[MATHEMATICS\] Bessel function of the first kind of order 0
(GFDL)

## __Syntax__

result = __bessel\_j0__(x)

## __Description__

__bessel\_j0__(x) computes the \[\[Bessel function\]\] of the first kind
of order 0 of X.

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is of type _real_ and lies in the range __-0.4027__ \<=
__Bessel__(0,x) \<= 1. It has the same kind as X.

## __Examples__

Sample program:

```fortran
program demo_besj0
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.0_real64
   x = bessel_j0(x)
end program demo_besj0
```

## __Standard__

Fortran 2008 and later

## __See Also__

__bessel\_j1__(3), __bessel\_jn__(3), __bessel\_y0__(3),
__bessel\_y1__(3), __bessel\_yn__(3)

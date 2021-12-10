---
layout: book
title: bessel_j1
permalink: /learn/intrinsics/BESSEL_J1
---
## __Name__

__bessel\_j1__(3) - \[MATHEMATICS\] Bessel function of the first kind of order 1
(GFDL)

## __Syntax__
```fortran
    result = bessel_j1(x)
```
## __Description__

__bessel\_j1(x)__ computes the Bessel function of the first kind
of order __1__ of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_.

## __Returns__

The return value is of type _real_ and lies in the range __-0.5818 \<=
bessel(0,x) \<= 0.5818__ . It has the same kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_besj1
use, intrinsic :: iso_fortran_env, only : real_kinds, &
 & real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
   x = bessel_j1(x)
end program demo_besj1
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__bessel\_j0__(3)](BESSEL_J0),
[__bessel\_jn__(3)](BESSEL_JN), 
[__bessel\_y0__(3)](BESSEL_Y0),
[__bessel\_y1__(3)](BESSEL_Y1), 
[__bessel\_yn__(3)](BESSEL_YN)

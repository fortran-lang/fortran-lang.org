---
layout: book
title: cos
permalink: /learn/intrinsics/COS
---
## __Name__

__cos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function
(GFDL)

## __Syntax__

result = __cos__(x)

## __Description__

__cos__(x) computes the cosine of X.

## __Arguments__

  - __X__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of the same type and kind as X. The real part of the
result is in radians. If X is of the type _real_, the return value lies in
the range __-1__ \<= __cos__(x) \<= 1.

## __Examples__

Sample program:

```fortran
program demo_cos
implicit none
doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
end program demo_cos
```

Expected output:

```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```

## __Standard__

FORTRAN 77 and later

## __See Also__

[__acos__(3)](ACOS),
[__sin__(3)](SIN),
[__tan__(3)](TAN)

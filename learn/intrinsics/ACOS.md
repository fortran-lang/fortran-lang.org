---
layout: book
title: acos
permalink: /learn/intrinsics/ACOS
---
## __Name__
__acos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] arccosine function
(GFDL)

## __Syntax__
```fortran
   result = acos(x)
    real(kind=KIND),elemental  :: acos
    real(kind=KIND),intent(in) :: x
```
    where KIND may be any supported KIND for the type _real_.

## __Description__

__acos(X)__ computes the arccosine of __X__ (inverse of __cos(X)__).

## __Arguments__

  - __X__
    The type shall be _real_ with a magnitude that is less than one.
## __Returns__

The return value is of the same type and kind as __X__. The real part of the
result is in radians and lies in the range

     __0 \<= acos(X) \<= PI__

## __Examples__
Sample program:
```fortran
program demo_acos
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
real(kind=real64) :: x = 0.866_real64
real(kind=real64),parameter :: dwr=acos(-1.0_real64)/180.0_real64
    write(*,*)'acos(',x,') is ', acos(x)
    write(*,*)'90 degrees is ', d2r*90.0_real64, ' radians'
    write(*,*)'180 degrees is ', d2r*180.0_real64, ' radians'
    write(*,*)'for reference &
    &PI= 3.14159265358979323846264338327950288419716939937510'
end program demo_acos
```
Results:
```
    acos(  0.86599999999999999      ) is   0.52364958093182890
    90 degrees is    1.5707963267948966       radians
    180 degrees is    3.1415926535897931       radians
    for reference PI= 3.14159265358979323846264338327950288419716939937510
```
## __Standard__
FORTRAN 77 and later; for a _complex_ argument - Fortran 2008 and later

## __See Also__
Inverse function: [__cos__(3](COS))

---
layout: book
title: acos
permalink: /learn/intrinsics/ACOS
---
## __Name__
__acos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] arccosine (inverse cosine) function

## __Syntax__
```fortran
  result = acos(x)

   TYPE(kind=KIND),elemental :: acos

   TYPE(kind=KIND,intent(in) :: x
```
where __TYPE__ may be _real_ or _complex_ and __KIND__ may be any __KIND__ supported
by the associated type.

## __Description__

__acos(x)__ computes the arccosine of __x__ (inverse of __cos(x)__).

## __Arguments__

  - __x__
    : Must be type _real_ or _complex_. If the type is _real_, the value
    must satisfy |__x__| <= 1.

## __Returns__

The return value is of the same type and kind as __x__. The _real_ part of
the result is in radians and lies in the range __0 \<= acos(x%re) \<= PI__ .

## __Examples__
Sample program:
```fortran
program demo_acos
use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
implicit none
character(len=*),parameter :: all='(*(g0,1x))'
real(kind=real64) :: x = 0.866_real64
real(kind=real64),parameter :: d2r=acos(-1.0_real64)/180.0_real64

    print all,'acos(',x,') is ', acos(x)
    print all,'90 degrees is ', d2r*90.0_real64, ' radians'
    print all,'180 degrees is ', d2r*180.0_real64, ' radians'
    print all,'for reference &
    &PI ~ 3.14159265358979323846264338327950288419716939937510'
    print all,'elemental',acos([-1.0,-0.5,0.0,0.50,1.0])

end program demo_acos
```
  Results:
```text
   acos( .8660000000000000 ) is  .5236495809318289
   90 degrees is  1.570796326794897  radians
   180 degrees is  3.141592653589793  radians
   for reference PI ~ 3.14159265358979323846264338327950288419716939937510
   elemental 3.141593 2.094395 1.570796 1.047198 .000000
```
## __Standard__
FORTRAN 77 and later; for a _complex_ argument - Fortran 2008 and later

## __See Also__

 - [wikipedia: inverse trigonometric functions](https://en.wikipedia.org/wiki/Inverse_trigonometric_functions)

Inverse function: [__cos__(3](COS))

###### fortran-lang intrinsic descriptions (license: MIT))

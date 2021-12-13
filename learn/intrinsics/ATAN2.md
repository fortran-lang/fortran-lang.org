---
layout: book
title: atan2
permalink: /learn/intrinsics/ATAN2
---
## __Name__

__atan2__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function

## __Syntax__
```fortran
result = atan2(y, x)
```
## __Description__

__atan2(y, x)__ computes the arctangent of the complex number
( __x__ + i __y__ ) .

This function can be used to transform from Cartesian into polar
coordinates and allows to determine the angle in the correct quadrant.
To convert from Cartesian Coordinates __(x,y)__ to polar coordinates

(r,theta): $$ \\begin{aligned} r &= \\sqrt{x\*\*2 + y\*\*2} \\\\ \\theta
&= \\tan\*\*{__-1__}(y / x) \\end{aligned} $$

## __Arguments__

  - __y__
    : The type shall be _real_.

  - __x__
    : The type and kind type parameter shall be the same as __y__. If __y__ is
    zero, then __x__ must be nonzero.

## __Returns__

The return value has the same type and kind type parameter as __y__. It is
the principal value of the complex number __(x + i, y)__. If x is nonzero,
then it lies in the range __-PI \<= atan(x) \<= PI__. The sign is
positive if __y__ is positive. If __y__ is zero, then the return value is zero
if __x__ is strictly positive, __PI__ if __x__ is negative and __y__ is positive zero
(or the processor does not handle signed zeros), and __-PI__ if __x__ is
negative and __Y__ is negative zero. Finally, if __x__ is zero, then the
magnitude of the result is __PI/2__.

## __Examples__

Sample program:

```fortran
program demo_atan2
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=sp) :: x = 1.e0_sp, y = 0.5e0_sp, z
   z = atan2(y,x)
   write(*,*)x,y,z
end program demo_atan2
```
Results:
```text
      1.00000000      0.500000000      0.463647604    
```

## __Standard__

FORTRAN 77 and later

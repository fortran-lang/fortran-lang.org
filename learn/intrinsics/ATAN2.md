---
layout: book
title: atan2
permalink: /learn/intrinsics/ATAN2
---
## __Name__

__atan2__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function
(GFDL)

## __Syntax__

result = __atan2(Y, X)__

## __Description__

__atan2(Y, X)__ computes the arctangent of the complex number

```
      X + i Y.
```

This function can be used to transform from Cartesian into polar
coordinates and allows to determine the angle in the correct quadrant.
To convert from Cartesian Coordinates __(X,Y)__ to polar coordinates

(r,theta): $$ \\begin{aligned} r &= \\sqrt{x\*\*2 + y\*\*2} \\\\ \\theta
&= \\tan\*\*{__-1__}(y / x) \\end{aligned} $$

## __Arguments__

  - __Y__
    : The type shall be ___real___.

  - __X__
    : The type and kind type parameter shall be the same as __Y__. If __Y__ is
    zero, then __X__ must be nonzero.

## __Returns__

The return value has the same type and kind type parameter as __Y__. It is
the principal value of the complex number __(X + i, Y)__. If X is nonzero,
then it lies in the range __-PI \<= atan(x) \<= PI__. The sign is
positive if __Y__ is positive. If __Y__ is zero, then the return value is zero
if __X__ is strictly positive, __PI__ if __X__ is negative and __Y__ is positive zero
(or the processor does not handle signed zeros), and __-PI__ if __X__ is
negative and __Y__ is negative zero. Finally, if __X__ is zero, then the
magnitude of the result is __PI/2__.

## __Examples__

Sample program:

```fortran
program demo_atan2
implicit none
real(4) :: x = 1.e0_4, y = 0.5e0_4
    x = atan2(y,x)
end program demo_atan2
```

## __Standard__

FORTRAN 77 and later

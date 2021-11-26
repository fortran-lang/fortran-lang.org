---
layout: book
title: asinh
permalink: /learn/intrinsics/ASINH
---
-------------------------------------------------------------------------------
## __Name__

__asinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function
(GFDL)

## __Syntax__

result = __asinh__(x)

## __Description__

__asinh__(x) computes the inverse hyperbolic sine of X.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_.

## __Returns__

The return value is of the same type and kind as X. If X is complex, the
imaginary part of the result is in radians and lies between __-PI__/2
\<= __AIMAG__(ASINH(X)) \<= PI/2.

## __Examples__

Sample program:

```
   program demo_asinh
   implicit none
   real(8), dimension(3) :: x = [ -1.0, 0.0, 1.0 ]
      write (*,*) asinh(x)
   end program demo_asinh
```

## __Standard__

Fortran 2008 and later

## __See Also__

Inverse function: [__sinh__(3)](SINH)

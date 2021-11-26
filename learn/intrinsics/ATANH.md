---
layout: book
title: atanh
permalink: /learn/intrinsics/ATANH
---
-------------------------------------------------------------------------------
## __Name__

__atanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function
(GFDL)

## __Syntax__

result = __atanh__(x)

## __Description__

__atanh__(x) computes the inverse hyperbolic tangent of X.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as X. If X is complex, the
imaginary part of the result is in radians and lies between

__-PI__/2 \<= __AIMAG__(ATANH(X)) \<= PI/2.

## __Examples__

Sample program:

```
    program demo_atanh
    implicit none
    real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]
       write (*,*) atanh(x)
    end program demo_atanh
```

## __Standard__

Fortran 2008 and later

## __See Also__

Inverse function: [__tanh__(3)](TANH)

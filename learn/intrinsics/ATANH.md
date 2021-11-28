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

result = __atanh(X)__

## __Description__

__atanh(X)__ computes the inverse hyperbolic tangent of __X__.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __X__. If __X__ is complex, the
imaginary part of the result is in radians and lies between

__-PI/2 \<= aimag(atanh(X)) \<= PI/2__

## __Examples__

Sample program:

```fortran
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

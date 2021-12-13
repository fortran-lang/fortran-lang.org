---
layout: book
title: atanh
permalink: /learn/intrinsics/ATANH
---
## __Name__

__atanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function
(GFDL)

## __Syntax__
```fortran
result = atanh(x)
```
## __Description__

__atanh(x)__ computes the inverse hyperbolic tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians and lies between

__-PI/2 \<= aimag(atanh(x)) \<= PI/2__

## __Examples__

Sample program:

```fortran
program demo_atanh
implicit none
real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]

   write (*,*) atanh(x)

end program demo_atanh
```
  Results:
```text
   -Infinity   0.00000000             Infinity
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__tanh__(3)](TANH)

###### fortran-lang intrinsic descriptions

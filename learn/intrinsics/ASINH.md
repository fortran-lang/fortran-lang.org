---
layout: book
title: asinh
permalink: /learn/intrinsics/ASINH
---
## __Name__

__asinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function
(GFDL)

## __Syntax__
```fortran
result = asinh(x)

    elemental TYPE(kind=KIND) function asinh(x)
    TYPE(kind=KIND) :: x
```
Where the returned value has the kind of the input value 
and TYPE may be _real_ or _complex_
## __Description__

__asinh(x)__ computes the inverse hyperbolic sine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value is of the same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians and lies between 
__-PI/2 \<= aimag(asinh(x)) \<= PI/2__.

## __Examples__

Sample program:

```fortran
program demo_asinh
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
real(kind=dp), dimension(3) :: x = [ -1.0d0, 0.0d0, 1.0d0 ]

    write (*,*) asinh(x)

end program demo_asinh
```
  Results:
```text
  -0.88137358701954305  0.0000000000000000  0.88137358701954305     
```

## __Standard__

Fortran 2008 and later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__sinh__(3)](SINH)

---
layout: book
title: cosh
permalink: /learn/intrinsics/COSH
---
## __Name__

__cosh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function
(GFDL)

## __Syntax__
```fortran
    result = cosh(x)

     TYPE(kind=KIND) elemental function cosh(x)
     TYPE(kind=KIND),intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any 
supported kind for the associated type. The returned __value__
will be the same type and kind as the input value __x__.

## __Description__

__cosh(x)__ computes the hyperbolic cosine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is _complex_, the
imaginary part of the result is in radians.

If __x__ is _real_, the return value has a lower bound of one, 
__cosh(x) \>= 1__.

## __Examples__

Sample program:

```fortran
program demo_cosh
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 1.0_real64
    x = cosh(x)
end program demo_cosh
```

## __Standard__

FORTRAN 77 and later, for a complex argument - Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

Inverse function: [__acosh__(3)](ACOSH)

###### fortran-lang intrinsic descriptions

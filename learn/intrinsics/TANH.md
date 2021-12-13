---
layout: book
title: tanh
permalink: /learn/intrinsics/TANH
---
## __Name__

__tanh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic tangent function
(GFDL)

## __Syntax__
```fortran
x = tanh(x)
```
## __Description__

__tanh(x)__ computes the hyperbolic tangent of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__. If __x__ is complex, the
imaginary part of the result is in radians. If __x__ is _real_, the return
value lies in the range

```
      -1 <= tanh(x) <= 1.
```
## __Examples__

Sample program:

```fortran
program demo_tanh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = 2.1_real64
   write(*,*)x, tanh(x)
end program demo_tanh
```
  Results:
```text
      2.1000000000000001       0.97045193661345386     
```
## __Standard__

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

## __See Also__
- [Wikipedia:hyperbolic functions](https://en.wikipedia.org/wiki/Hyperbolic_functions)

[__atanh__(3)](ATANH)

###### fortran-lang intrinsic descriptions

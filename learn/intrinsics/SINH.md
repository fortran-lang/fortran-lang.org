---
layout: book
title: sinh
permalink: /learn/intrinsics/SINH
---
## __Name__

__sinh__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function
(GFDL)

## __Syntax__
```fortran
result = sinh(x)
```

## __Description__

__sinh(x)__ computes the hyperbolic sine of __x__.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.

## __Returns__

The return value has same type and kind as __x__.

## __Examples__

Sample program:

```fortran
program demo_sinh
use, intrinsic :: iso_fortran_env, only : &
& real_kinds, real32, real64, real128
implicit none
real(kind=real64) :: x = - 1.0_real64
   x = sinh(x)
end program demo_sinh
```

## __Standard__

Fortran 95 and later, for a complex argument Fortran 2008 or later

## __See Also__

[__asinh__(3)](ASINH)

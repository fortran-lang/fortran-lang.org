---
layout: book
title: sqrt
permalink: /learn/intrinsics/SQRT
---
## __Name__

__sqrt__(3) - \[MATHEMATICS\] Square-root function
(GFDL)

## __Syntax__

result = __sqrt__(x)

## __Description__

__sqrt__(x) computes the square root of X.

## __Arguments__

  - __X__
    The type shall be _real_ or _complex_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as X.

## __Examples__

Sample program:

```
    program demo_sqrt
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
      real(kind=real64) :: x = 2.0_real64
      complex :: z = (1.0, 2.0)
      x = sqrt(x)
      z = sqrt(z)
    end program demo_sqrt
```

## __Standard__

FORTRAN 77 and later

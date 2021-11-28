---
layout: book
title: log10
permalink: /learn/intrinsics/LOG10
---
## __Name__

__log10__(3) - \[MATHEMATICS\] Base 10 logarithm function
(GFDL)

## __Syntax__

result = __LOG10__(x)

## __Description__

__LOG10__(X) computes the base 10 logarithm of X.

## __Arguments__

  - __X__
    The type shall be _real_.

## __Returns__

The return value is of type _real_ or _complex_. The kind type parameter is
the same as X.

## __Examples__

Sample program:

```fortran
    program demo_log10
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    real(kind=real64) :: x = 10.0_real64
      x = log10(x)
    end program demo_log10
```

## __Standard__

FORTRAN 77 and later

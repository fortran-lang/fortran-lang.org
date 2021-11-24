---
layout: book
title: log10
permalink: /learn/intrinsics/LOG10
---
#### NAME

__log10__(3f) - \[MATHEMATICS\] Base 10 logarithm function
(GFDL)

#### SYNTAX

result = __LOG10__(x)

#### DESCRIPTION

__LOG10__(X) computes the base 10 logarithm of X.

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is of type REAL or COMPLEX. The kind type parameter is
the same as X.

#### EXAMPLE

Sample program:

```
    program demo_log10
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    real(kind=real64) :: x = 10.0_real64
      x = log10(x)
    end program demo_log10
```

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\|Elemental function

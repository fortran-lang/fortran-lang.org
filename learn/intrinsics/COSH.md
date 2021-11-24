---
layout: book
title: cosh
permalink: /learn/intrinsics/COSH
---
#### NAME

__cosh__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic cosine function
(GFDL)

#### SYNTAX

x = __cosh__(x)

#### DESCRIPTION

__cosh__(x) computes the hyperbolic cosine of X.

#### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

#### RETURN VALUE

The return value has same type and kind as X. If X is complex, the
imaginary part of the result is in radians. If X is REAL, the return
value has a lower bound of one, __cosh__(x) \>= 1.

#### EXAMPLE

Sample program:

```
   program demo_cosh
   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
   implicit none
   real(kind=real64) :: x = 1.0_real64
      x = cosh(x)
   end program demo_cosh
```

#### STANDARD

FORTRAN 77 and later, for a complex argument - Fortran 2008 or later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

Inverse function: __acosh__(3)

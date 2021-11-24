---
layout: book
title: bessel_y1
permalink: /learn/intrinsics/BESSEL_Y1
---
#### NAME

__bessel\_y1__(3f) - \[MATHEMATICS\] Bessel function of the second kind of order 1
(GFDL)

#### SYNTAX

result = __bessel\_y1__(x)

#### DESCRIPTION

__bessel\_y1__(x) computes the \[\[Bessel function\]\] of the second
kind of order 1 of X.

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is REAL. It has the same kind as X.

#### EXAMPLE

Sample program:

```
   program demo_besy1
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
     real(kind=real64) :: x = 1.0_real64
     x = bessel_y1(x)
   end program demo_besy1
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__bessel\_j0__(3), __bessel\_j1__(3), __bessel\_jn__(3),
__bessel\_y0__(3), __bessel\_yn__(3)

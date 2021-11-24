---
layout: book
title: bessel_y0
permalink: /learn/intrinsics/BESSEL_Y0
---
#### NAME

__bessel\_y0__(3f) - \[MATHEMATICS\] Bessel function of the second kind of order 0
(GFDL)

#### SYNTAX

result = __bessel\_y0__(x)

#### DESCRIPTION

__bessel\_y0__(x) computes the \[\[Bessel function\]\] of the second
kind of order 0 of X.

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is of type REAL. It has the same kind as X.

#### EXAMPLE

Sample program:

```
   program demo_besy0
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
     real(kind=real64) :: x = 0.0_real64
     x = bessel_y0(x)
   end program demo_besy0
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__bessel\_j0__(3), __bessel\_j1__(3), __bessel\_jn__(3),
__bessel\_y1__(3), __bessel\_yn__(3)

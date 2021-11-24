---
layout: book
title: bessel_jn
permalink: /learn/intrinsics/BESSEL_JN
---
#### NAME

__bessel\_jn__(3f) - \[MATHEMATICS\] Bessel function of the first kind
(GFDL)

#### SYNTAX

  - result = __bessel\_jn__(n, x)

  - result = __bessel\_jn__(n1, n2, x)

#### DESCRIPTION

__bessel\_jn__(n, x) computes the \[\[Bessel function\]\] of the first
kind of order N of X. If N and X are arrays, their ranks and shapes
shall conform.

__bessel\_jn__(n1, n2, x) returns an array with the \[\[Bessel
function|Bessel functions\]\] of the first kind of the orders N1 to N2.

#### ARGUMENTS

  - __N__
    Shall be a scalar or an array of type INTEGER.

  - __N1__
    Shall be a non-negative scalar of type INTEGER.

  - __N2__
    Shall be a non-negative scalar of type INTEGER.

  - __X__
    Shall be a scalar or an array of type REAL. For __bessel\_jn__(n1,
    n2, x) it shall be scalar.

#### RETURN VALUE

The return value is a scalar of type REAL. It has the same kind as X.

#### EXAMPLE

Sample program:

```
   program demo_besjn
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
   real(kind=real64) :: x = 1.0_real64
     x = bessel_jn(5,x)
   end program demo_besjn
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental function, except for the transformational variant
__bessel\_jn__(n1, n2, x).

#### SEE ALSO

__bessel\_j0__(3), __bessel\_j1__(3), __bessel\_y0__(3),
__bessel\_y1__(3), __bessel\_yn__(3)

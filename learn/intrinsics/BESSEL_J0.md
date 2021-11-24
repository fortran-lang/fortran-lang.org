---
layout: book
title: bessel_j0
permalink: /learn/intrinsics/BESSEL_J0
---
#### NAME

__bessel\_j0__(3f) - \[MATHEMATICS\] Bessel function of the first kind of order 0
(GFDL)

#### SYNTAX

result = __bessel\_j0__(x)

#### DESCRIPTION

__bessel\_j0__(x) computes the \[\[Bessel function\]\] of the first kind
of order 0 of X.

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is of type REAL and lies in the range __-0.4027__ \<=
__Bessel__(0,x) \<= 1. It has the same kind as X.

#### EXAMPLE

Sample program:

```
   program demo_besj0
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
      implicit none
      real(kind=real64) :: x = 0.0_real64
      x = bessel_j0(x)
   end program demo_besj0
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__bessel\_j1__(3), __bessel\_jn__(3), __bessel\_y0__(3),
__bessel\_y1__(3), __bessel\_yn__(3)

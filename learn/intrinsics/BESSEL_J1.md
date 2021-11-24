---
layout: book
title: bessel_j1
permalink: /learn/intrinsics/BESSEL_J1
---
#### NAME

__bessel\_j1__(3f) - \[MATHEMATICS\] Bessel function of the first kind of order 1
(GFDL)

#### SYNTAX

result = __bessel\_j1__(x)

#### DESCRIPTION

__bessel\_j1__(x) computes the \[\[Bessel function\]\] of the first kind
of order 1 of X.

#### ARGUMENTS

  - __X__
    The type shall be REAL.

#### RETURN VALUE

The return value is of type REAL and lies in the range __-0.5818__ \<=
__Bessel__(0,x) \<= 0.5818 . It has the same kind as X.

#### EXAMPLE

Sample program:

```
   program demo_besj1
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
      implicit none
      real(kind=real64) :: x = 1.0_real64
      x = bessel_j1(x)
   end program demo_besj1
```

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__bessel\_j0__(3), __bessel\_jn__(3), __bessel\_y0__(3),
__bessel\_y1__(3), __bessel\_yn__(3)

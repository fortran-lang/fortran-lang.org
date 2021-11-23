---
layout: book
title: bessel_yn
permalink: /learn/intrinsics/BESSEL_YN
---
### NAME

**bessel\_yn**(3f) - \[MATHEMATICS\] Bessel function of the second kind
(GFDL)

### SYNTAX

  - result = **bessel\_yn**(n, x)

  - result = **bessel\_yn**(n1, n2, x)

### DESCRIPTION

**bessel\_yn**(n, x) computes the \[\[Bessel function\]\] of the second
kind of order N of X. If N and X are arrays, their ranks and shapes
shall conform.

**bessel\_yn**(n1, n2, x) returns an array with the \[\[Bessel
function|Bessel functions\]\] of the first kind of the orders N1 to N2.

### ARGUMENTS

  - **N**
    Shall be a scalar or an array of type INTEGER.

  - **N1**
    Shall be a non-negative scalar of type INTEGER.

  - **N2**
    Shall be a non-negative scalar of type INTEGER.

  - **X**
    Shall be a scalar or an array of type REAL; for **bessel\_yn**(n1,
    n2, x) it shall be scalar.

### RETURN VALUE

The return value is REAL. It has the same kind as X.

### EXAMPLE

Sample program:

```
   program demo_besyn
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
   real(kind=real64) :: x = 1.0_real64
     x = bessel_yn(5,x)
   end program demo_besyn
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function, except for the transformational function
**bessel\_yn**(n1, n2, x)

### SEE ALSO

**bessel\_j0**(3), **bessel\_j1**(3), **bessel\_jn**(3),
**bessel\_y0**(3), **bessel\_y1**(3)

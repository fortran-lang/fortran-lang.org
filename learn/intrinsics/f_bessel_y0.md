---
layout: book
title: bessel_y0
permalink: /learn/intrinsics/f_bessel_y0
---
### NAME

**bessel\_y0**(3f) - \[MATHEMATICS\] Bessel function
of the second kind of order 0

### SYNTAX

result = **bessel\_y0**(x)

### DESCRIPTION

**bessel\_y0**(x) computes the \[\[Bessel function\]\] of the second
kind of order 0 of X.

### ARGUMENTS

  - **X**
    The type shall be REAL.

### RETURN VALUE

The return value is of type REAL. It has the same kind as X.

### EXAMPLE

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

### STANDARD

Fortran 2008 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**bessel\_j0**(3), **bessel\_j1**(3), **bessel\_jn**(3),
**bessel\_y1**(3), **bessel\_yn**(3)

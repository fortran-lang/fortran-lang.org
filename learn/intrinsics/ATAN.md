---
layout: book
title: atan
permalink: /learn/intrinsics/ATAN
---
### NAME

**atan**(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Arctangent function
(GFDL)

### SYNTAX

  - result = **atan**(x)

  - result = **atan**(y, x)

### DESCRIPTION

**atan**(x) computes the arctangent of X.

### ARGUMENTS

  - **X**
    The type shall be REAL or COMPLEX; if Y is present, X shall be REAL.

  - **Y**
    Shall be of the same type and kind as X.

### RETURN VALUE

The return value is of the same type and kind as X. If Y is present, the
result is identical to **atan2**(y,x). Otherwise, it the arc tangent of
X, where the real part of the result is in radians and lies in the range

**-PI**/2 \<= **atan**(x) \<= PI/2.

### EXAMPLE

Sample program:

```
   program demo_atan
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
   real(kind=real64) :: x = 2.866_real64
      x = atan(x)
   end program demo_atan
```

### STANDARD

FORTRAN 77 and later; for a complex argument and for two
arguments Fortran 2008 or later

### CLASS

Elemental function

### SEE ALSO

**atan2**(3), **tan**(3)

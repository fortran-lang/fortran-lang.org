---
layout: book
title: tanh
permalink: /learn/intrinsics/TANH
---
### NAME

__tanh__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic tangent function
(GFDL)

### SYNTAX

x = __tanh__(x)

### DESCRIPTION

__tanh__(X) computes the hyperbolic tangent of X.

### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value has same type and kind as X. If X is complex, the
imaginary part of the result is in radians. If X is REAL, the return
value lies in the range

```
      -1 <= tanh(x) <= 1.
```

### EXAMPLE

Sample program:

```
   program demo_tanh
   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
   implicit none
     real(kind=real64) :: x = 2.1_real64
     x = tanh(x)
   end program demo_tanh
```

### STANDARD

FORTRAN 77 and later, for a complex argument Fortran 2008 or later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__atanh__(3)

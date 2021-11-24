---
layout: book
title: sinh
permalink: /learn/intrinsics/SINH
---
### NAME

__sinh__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Hyperbolic sine function
(GFDL)

### SYNTAX

result = __sinh__(x)

### DESCRIPTION

__sinh__(x) computes the hyperbolic sine of X.

### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value has same type and kind as X.

### EXAMPLE

Sample program:

```
   program demo_sinh
   use, intrinsic :: iso_fortran_env, only : real_kinds, real32, real64, real128
   implicit none
     real(kind=real64) :: x = - 1.0_real64
     x = sinh(x)
   end program demo_sinh
```

### STANDARD

Fortran 95 and later, for a complex argument Fortran 2008 or later

### CLASS

Elemental function

### SEE ALSO

__asinh__(3)

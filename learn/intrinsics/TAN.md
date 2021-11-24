---
layout: book
title: tan
permalink: /learn/intrinsics/TAN
---
### NAME

__tan__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Tangent function
(GFDL)

### SYNTAX

result = __tan__(x)

### DESCRIPTION

__tan__(x) computes the tangent of X.

### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value has the same type and kind as X.

### EXAMPLE

Sample program:

```
   program demo_tan
   use, intrinsic :: iso_fortran_env, only : real_kinds, &
   & real32, real64, real128
   implicit none
   real(kind=real64) :: x = 0.165_real64
     x = tan(x)
   end program demo_tan
```

### STANDARD

FORTRAN 77 and later. For a complex argument, Fortran 2008 or later.

### CLASS

Elemental function

### SEE ALSO

__atan__(3), __cos__(3), __sin__(3)

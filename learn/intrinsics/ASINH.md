---
layout: book
title: asinh
permalink: /learn/intrinsics/ASINH
---
### NAME

__asinh__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic sine function
(GFDL)

### SYNTAX

result = __asinh__(x)

### DESCRIPTION

__asinh__(x) computes the inverse hyperbolic sine of X.

### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value is of the same type and kind as X. If X is complex, the
imaginary part of the result is in radians and lies between __-PI__/2
\<= __AIMAG__(ASINH(X)) \<= PI/2.

### EXAMPLE

Sample program:

```
   program demo_asinh
   implicit none
   real(8), dimension(3) :: x = [ -1.0, 0.0, 1.0 ]
      write (*,*) asinh(x)
   end program demo_asinh
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

Inverse function: __sinh__(3)

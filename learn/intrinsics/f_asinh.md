---
layout: book
title: asinh
permalink: /learn/intrinsics/f_asinh
---
### NAME

**asinh**(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse
hyperbolic sine function

### SYNTAX

result = **asinh**(x)

### DESCRIPTION

**asinh**(x) computes the inverse hyperbolic sine of X.

### ARGUMENTS

  - **X**
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value is of the same type and kind as X. If X is complex, the
imaginary part of the result is in radians and lies between **-PI**/2
\<= **AIMAG**(ASINH(X)) \<= PI/2.

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

Inverse function: **sinh**(3)

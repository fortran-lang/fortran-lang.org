---
layout: book
title: atanh
permalink: /learn/intrinsics/ATANH
---
### NAME

__atanh__(3f) - \[MATHEMATICS:TRIGONOMETRIC\] Inverse hyperbolic tangent function
(GFDL)

### SYNTAX

result = __atanh__(x)

### DESCRIPTION

__atanh__(x) computes the inverse hyperbolic tangent of X.

### ARGUMENTS

  - __X__
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value has same type and kind as X. If X is complex, the
imaginary part of the result is in radians and lies between

__-PI__/2 \<= __AIMAG__(ATANH(X)) \<= PI/2.

### EXAMPLE

Sample program:

```
    program demo_atanh
    implicit none
    real, dimension(3) :: x = [ -1.0, 0.0, 1.0 ]
       write (*,*) atanh(x)
    end program demo_atanh
```

### STANDARD

Fortran 2008 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

Inverse function: __tanh__(3)

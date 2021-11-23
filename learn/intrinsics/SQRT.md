---
layout: book
title: sqrt
permalink: /learn/intrinsics/SQRT
---
### NAME

**sqrt**(3f) - \[MATHEMATICS\] Square-root function
(GFDL)

### SYNTAX

result = **sqrt**(x)

### DESCRIPTION

**sqrt**(x) computes the square root of X.

### ARGUMENTS

  - **X**
    The type shall be REAL or COMPLEX.

### RETURN VALUE

The return value is of type REAL or COMPLEX. The kind type parameter is
the same as X.

### EXAMPLE

Sample program:

```
    program demo_sqrt
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
      real(kind=real64) :: x = 2.0_real64
      complex :: z = (1.0, 2.0)
      x = sqrt(x)
      z = sqrt(z)
    end program demo_sqrt
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

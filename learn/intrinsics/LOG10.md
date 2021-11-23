---
layout: book
title: log10
permalink: /learn/intrinsics/LOG10
---
### NAME

**log10**(3f) - \[MATHEMATICS\] Base 10 logarithm function
(GFDL)

### SYNTAX

result = **LOG10**(x)

### DESCRIPTION

**LOG10**(X) computes the base 10 logarithm of X.

### ARGUMENTS

  - **X**
    The type shall be REAL.

### RETURN VALUE

The return value is of type REAL or COMPLEX. The kind type parameter is
the same as X.

### EXAMPLE

Sample program:

```
    program demo_log10
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    real(kind=real64) :: x = 10.0_real64
      x = log10(x)
    end program demo_log10
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

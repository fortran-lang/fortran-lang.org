---
layout: book
title: int
permalink: /learn/intrinsics/INT
---
### NAME

**int**(3f) - \[NUMERIC:TYPE\] Convert to integer type
(GFDL)

### SYNTAX

result = **int**(a \[, kind))

### DESCRIPTION

Convert to integer type

### ARGUMENTS

  - **A**
    Shall be of type INTEGER, REAL, or COMPLEX.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

These functions return a INTEGER variable or array under the following
rules:

> 1.  If A is of type INTEGER, **int**(a) = a
>
> 2.  If A is of type REAL and |a| \< 1, **int**(a) equals 0. If |a| \>=
>     1, then **int**(a) equals the largest integer that does not exceed
>     the range of A and whose sign is the same as the sign of A.
>
> 3.  If A is of type COMPLEX, rule 2 is applied to the real part of A.

### EXAMPLE

Sample program:

```
    program demo_int
    implicit none
      integer :: i = 42
      complex :: z = (-3.7, 1.0)
      print *, int(i)
      print *, int(z), int(z,8)
    end program demo_int
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

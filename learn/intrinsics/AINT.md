---
layout: book
title: aint
permalink: /learn/intrinsics/AINT
---
### NAME

__aint__(3f) - \[NUMERIC\] Truncate to a whole number
(GFDL)

### SYNTAX

result = __AINT__(A \[, KIND\])

### DESCRIPTION

__AINT__(A \[, KIND\]) truncates its argument to a whole number.

### ARGUMENTS

  - __A__
    the type of the argument shall be REAL.

  - __KIND__
    (optional) an INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type REAL with the kind type parameter of the
argument if the optional KIND is absent; otherwise, the kind type
parameter will be given by KIND. If the magnitude of X is less than one,
__aint__(x) returns zero. If the magnitude is equal to or greater than
one then it returns the largest whole number that does not exceed its
magnitude. The sign is the same as the sign of X.

### EXAMPLE

Sample program:

```
    program demo_aint
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    real ::  x4
    real(kind=real64) :: x8
       x4 = 1.234E0_4
       x8 = 4.321_real64
       print *, aint(x4), dint(x8)
       x8 = aint(x4,kind=real64)
    end program demo_aint
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental function

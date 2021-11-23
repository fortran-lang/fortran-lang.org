---
layout: book
title: conjg
permalink: /learn/intrinsics/CONJG
---
### NAME

**conjg**(3f) - \[NUMERIC\] Complex conjugate function
(GFDL)

### SYNTAX

z = **conjg**(z)

### DESCRIPTION

**conjg**(z) returns the conjugate of Z. If Z is (x, y) then the result
is (x, **-y**)

### ARGUMENTS

  - **Z**
    The type shall be COMPLEX.

### RETURN VALUE

The return value is of type COMPLEX.

### EXAMPLE

Sample program:

```
    program demo_conjg
    use, intrinsic :: iso_fortran_env, only : real_kinds, &
    & real32, real64, real128
    implicit none
    complex :: z = (2.0, 3.0)
    complex(kind=real64) :: dz = (&
    &  1.2345678901234567_real64, &
    & -1.2345678901234567_real64)
        z= conjg(z)
        print *, z
        dz = conjg(dz)
        print *, dz
    end program demo_conjg
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

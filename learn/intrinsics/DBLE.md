---
layout: book
title: dble
permalink: /learn/intrinsics/DBLE
---
### NAME

**dble**(3f) - \[NUMERIC:TYPE\] Double conversion function
(GFDL)

### SYNTAX

result = **DBLE**(A)

### DESCRIPTION

**DBLE**(A) Converts A to double precision real type.

### ARGUMENTS

  - **A**
    The type shall be INTEGER, REAL, or COMPLEX.

### RETURN VALUE

The return value is of type DOUBLEPRECISION.

### EXAMPLE

Sample program:

```
    program demo_dble
    implicit none
    real    :: x = 2.18
    integer :: i = 5
    complex :: z = (2.3,1.14)
       print *, dble(x), dble(i), dble(z)
    end program demo_dble
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**float**(3), **real**(3)

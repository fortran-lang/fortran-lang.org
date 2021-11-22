---
layout: book
title: float
permalink: /learn/intrinsics/f_float
---
### NAME

**float**(3f) - \[NUMERIC:TYPE\] Convert integer to
default real

### SYNTAX

result = **float**(a)

### DESCRIPTION

**float**(a) converts the integer A to a default real value.

### ARGUMENTS

  - **A**
    The type shall be INTEGER.

### RETURN VALUE

The return value is of type default REAL.

### EXAMPLE

Sample program:

```
    program demo_float
    implicit none
        integer :: i = 1
        if (float(i) /= 1.) stop ' FLOAT FAILED'
    end program demo_float
```

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**dble**(3), **real**(3)

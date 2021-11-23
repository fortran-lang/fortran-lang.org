---
layout: book
title: epsilon
permalink: /learn/intrinsics/EPSILON
---
### NAME

**epsilon**(3f) - \[NUMERIC MODEL\] Epsilon function
(GFDL)

### SYNTAX

result = **epsilon**(x)

### DESCRIPTION

**epsilon**(x) returns a nearly negligible number relative to 1.

### ARGUMENTS

  - **X**
    The type shall be REAL.

### RETURN VALUE

The return value is of same type as the argument.

### EXAMPLE

Sample program:

```
    program demo_epsilon
    implicit none
        real :: x = 3.143
        real(8) :: y = 2.33
        print *, epsilon(x)
        print *, epsilon(y)
    end program demo_epsilon
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function

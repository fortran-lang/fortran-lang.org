---
layout: book
title: minexponent
permalink: /learn/intrinsics/MINEXPONENT
---
### NAME

__minexponent__(3f) - \[NUMERIC MODEL\] Minimum exponent of a real kind
(GFDL)

### SYNTAX

result = __minexponent__(x)

### DESCRIPTION

__minexponent__(x) returns the minimum exponent in the model of the type
of X.

### ARGUMENTS

  - __X__
    Shall be of type REAL.

### RETURN VALUE

The return value is of type INTEGER and of the default integer kind.

### EXAMPLE

Sample program:

```
    program demo_minexponent
    implicit none
    real(kind=4) :: x
    real(kind=8) :: y
       print *, minexponent(x), maxexponent(x)
       print *, minexponent(y), maxexponent(y)
    end program demo_minexponent
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function

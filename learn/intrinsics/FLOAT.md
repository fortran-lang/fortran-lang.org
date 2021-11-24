---
layout: book
title: float
permalink: /learn/intrinsics/FLOAT
---
### NAME

__float__(3f) - \[NUMERIC:TYPE\] Convert integer to default real
(GFDL)

### SYNTAX

result = __float__(a)

### DESCRIPTION

__float__(a) converts the integer A to a default real value.

### ARGUMENTS

  - __A__
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

Elemental procedure\|Elemental function

### SEE ALSO

__dble__(3), __real__(3)

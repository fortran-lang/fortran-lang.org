---
layout: book
title: nearest
permalink: /learn/intrinsics/NEAREST
---
### NAME

**nearest**(3f) - \[MODEL\_COMPONENTS\] Nearest representable number
(GFDL)

### SYNTAX

result = **nearest**(x, s)

### DESCRIPTION

**nearest**(x, s) returns the processor-representable number nearest to
X in the direction indicated by the sign of S.

### ARGUMENTS

  - **X**
    Shall be of type REAL.

  - **S**
    Shall be of type REAL and not equal to zero.

### RETURN VALUE

The return value is of the same type as X. If S is positive, NEAREST
returns the processor-representable number greater than X and nearest to
it. If S is negative, NEAREST returns the processor-representable number
smaller than X and nearest to it.

### EXAMPLE

Sample program:

```
   program demo_nearest
   implicit none
     real :: x, y
     x = nearest(42.0, 1.0)
     y = nearest(42.0, -1.0)
     write (*,"(3(g20.15))") x, y, x - y
   end program demo_nearest
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

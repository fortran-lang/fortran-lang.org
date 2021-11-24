---
layout: book
title: fraction
permalink: /learn/intrinsics/FRACTION
---
### NAME

__fraction__(3f) - \[MODEL\_COMPONENTS\] Fractional part of the model representation
(GFDL)

### DESCRIPTION

__fraction__(x) returns the fractional part of the model representation
of X.

### SYNTAX

y = __fraction__(x)

### ARGUMENTS

  - __X__
    The type of the argument shall be a REAL.

### RETURN VALUE

The return value is of the same type and kind as the argument. The
fractional part of the model representation of X is returned; it is x \*
__radix__(x)\*\*(__-exponent__(x)).

### EXAMPLE

Sample program:

```
    program demo_fraction
    implicit none
      real :: x
      x = 178.1387e-4
      print *, fraction(x), x * radix(x)**(-exponent(x))
    end program demo_fraction
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

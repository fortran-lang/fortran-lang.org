---
layout: book
title: fraction
permalink: /learn/intrinsics/FRACTION
---
### NAME

**fraction**(3f) - \[MODEL\_COMPONENTS\] Fractional part of the model representation
(GFDL)

### DESCRIPTION

**fraction**(x) returns the fractional part of the model representation
of X.

### SYNTAX

y = **fraction**(x)

### ARGUMENTS

  - **X**
    The type of the argument shall be a REAL.

### RETURN VALUE

The return value is of the same type and kind as the argument. The
fractional part of the model representation of X is returned; it is x \*
**radix**(x)\*\*(**-exponent**(x)).

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

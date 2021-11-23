---
layout: book
title: set_exponent
permalink: /learn/intrinsics/SET_EXPONENT
---
### NAME

**set\_exponent**(3f) - \[MODEL\_COMPONENTS\] Set the exponent of the model
(GFDL)

### SYNTAX

result = **set\_exponent**(x, i)

### DESCRIPTION

**set\_exponent**(x, i) returns the real number whose fractional part is
that of X and whose exponent part is I.

### ARGUMENTS

  - **X**
    Shall be of type REAL.

  - **I**
    Shall be of type INTEGER.

### RETURN VALUE

The return value is of the same type and kind as X. The real number
whose fractional part is that that of X and whose exponent part if I is
returned; it is **fraction**(x) \* **radix**(x)\*\*i.

### EXAMPLE

Sample program:

```
    program demo_setexp
    implicit none
      real :: x = 178.1387e-4
      integer :: i = 17
      print *, set_exponent(x, i), fraction(x) * radix(x)**i
    end program demo_setexp
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

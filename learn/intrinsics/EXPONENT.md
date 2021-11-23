---
layout: book
title: exponent
permalink: /learn/intrinsics/EXPONENT
---
### NAME

**exponent**(3f) - \[MODEL\_COMPONENTS\] Exponent function
(GFDL)

### SYNTAX

result = **exponent**(x)

### DESCRIPTION

**exponent**(x) returns the value of the exponent part of X. If X is
zero the value returned is zero.

### ARGUMENTS

  - **X**
    The type shall be REAL.

### RETURN VALUE

The return value is of type default INTEGER.

### EXAMPLE

Sample program:

```
    program demo_exponent
    implicit none
      real :: x = 1.0
      integer :: i
      i = exponent(x)
      print *, i
      print *, exponent(0.0)
    end program demo_exponent
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

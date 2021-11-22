---
layout: book
title: maxexponent
permalink: /learn/intrinsics/f_maxexponent
---
### NAME

**maxexponent**(3f) - \[NUMERIC MODEL\] Maximum
exponent of a real kind

### SYNTAX

result = **maxexponent**(x)

### DESCRIPTION

**maxexponent**(x) returns the maximum exponent in the model of the type
of X.

### ARGUMENTS

  - **X**
    Shall be of type REAL.

### RETURN VALUE

The return value is of type INTEGER and of the default integer kind.

### EXAMPLE

Sample program:

```
    program demo_maxexponent
    implicit none
      real(kind=4) :: x
      real(kind=8) :: y

      print *, minexponent(x), maxexponent(x)
      print *, minexponent(y), maxexponent(y)
    end program demo_maxexponent
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function

---
layout: book
title: range
permalink: /learn/intrinsics/RANGE
---
### NAME

__range__(3f) - \[NUMERIC MODEL\] Decimal exponent range of a real kind
(GFDL)

### SYNTAX

result = __range__(x)

### DESCRIPTION

__range__(x) returns the decimal exponent range in the model of the type
of X.

### ARGUMENTS

  - __X__
    Shall be of type REAL or COMPLEX.

### RETURN VALUE

The return value is of type INTEGER and of the default integer kind.

### EXAMPLE

Sample program:

```
    program demo_range
    implicit none
    real(kind=4) :: x(2)
    complex(kind=8) :: y
       print *, precision(x), range(x)
       print *, precision(y), range(y)
    end program demo_range
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function

### SEE ALSO

__selected\_real\_kind__(3), __precision__(3)

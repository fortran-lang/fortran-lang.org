---
layout: book
title: precision
permalink: /learn/intrinsics/f_precision
---
### NAME

**precision**(3f) - \[NUMERIC MODEL\] Decimal
precision of a real kind

### SYNTAX

result = **precision**(x)

### DESCRIPTION

**precision**(x) returns the decimal precision in the model of the type
of X.

### ARGUMENTS

  - **X**
    Shall be of type REAL or COMPLEX.

### RETURN VALUE

The return value is of type INTEGER and of the default integer kind.

### EXAMPLE

Sample program:

```
    program demo_precision
    implicit none
      real(kind=4) :: x(2)
      complex(kind=8) :: y

      print *, precision(x), range(x)
      print *, precision(y), range(y)
    end program demo_precision
```

### STANDARD

Fortran 95 and later

### CLASS

Inquiry function

### SEE ALSO

**selected\_real\_kind**(3), **range**(3)

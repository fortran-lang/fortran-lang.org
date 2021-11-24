---
layout: book
title: ceiling
permalink: /learn/intrinsics/CEILING
---
### NAME

__ceiling__(3f) - \[NUMERIC\] Integer ceiling function
(GFDL)

### SYNTAX

result = __ceiling__(a \[, kind\])

### DESCRIPTION

__ceiling__(a) returns the least integer greater than or equal to A.

### ARGUMENTS

  - __A__
    The type shall be REAL.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type __integer__(kind) if KIND is present and a
default-kind INTEGER otherwise.

### EXAMPLE

Sample program:

```
    program demo_ceiling
    implicit none
    real :: x = 63.29
    real :: y = -63.59
       print *, ceiling(x) ! returns 64
       print *, ceiling(y) ! returns -63
    end program demo_ceiling
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__floor__(3), __nint__(3)

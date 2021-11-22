---
layout: book
title: ceiling
permalink: /learn/intrinsics/f_ceiling
---
### NAME

**ceiling**(3f) - \[NUMERIC\] Integer ceiling function

### SYNTAX

result = **ceiling**(a \[, kind\])

### DESCRIPTION

**ceiling**(a) returns the least integer greater than or equal to A.

### ARGUMENTS

  - **A**
    The type shall be REAL.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type **integer**(kind) if KIND is present and a
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

Elemental procedure|Elemental function

### SEE ALSO

**floor**(3), **nint**(3)

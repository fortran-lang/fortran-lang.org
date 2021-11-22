---
layout: book
title: floor
permalink: /learn/intrinsics/f_floor
---
### NAME

**floor**(3f) - \[NUMERIC\] Integer floor function

### DESCRIPTION

**floor**(a) returns the greatest integer less than or equal to X.

### SYNTAX

result = **floor**(a \[, kind\])

### ARGUMENTS

  - **A**
    The type shall be REAL.

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type **integer**(kind) if KIND is present and of
default-kind INTEGER otherwise.

### EXAMPLE

Sample program:

```
    program demo_floor
    implicit none
        real :: x = 63.29
        real :: y = -63.59
        print *, floor(x) ! returns 63
        print *, floor(y) ! returns -64
    end program demo_floor
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**ceiling**(3), **nint**(3)

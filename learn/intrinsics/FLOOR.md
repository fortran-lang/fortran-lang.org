---
layout: book
title: floor
permalink: /learn/intrinsics/FLOOR
---
#### NAME

__floor__(3f) - \[NUMERIC\] Integer floor function
(GFDL)

#### DESCRIPTION

__floor__(a) returns the greatest integer less than or equal to X.

#### SYNTAX

result = __floor__(a \[, kind\])

#### ARGUMENTS

  - __A__
    The type shall be REAL.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

#### RETURN VALUE

The return value is of type __integer__(kind) if KIND is present and of
default-kind INTEGER otherwise.

#### EXAMPLE

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

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__ceiling__(3), __nint__(3)

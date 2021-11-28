---
layout: book
title: floor
permalink: /learn/intrinsics/FLOOR
---
## __Name__

__floor__(3) - \[NUMERIC\] Integer floor function
(GFDL)

## __Description__

__floor__(a) returns the greatest integer less than or equal to X.

## __Syntax__

result = __floor__(a \[, kind\])

## __Arguments__

  - __A__
    The type shall be _real_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type __integer__(kind) if KIND is present and of
default-kind _integer_ otherwise.

## __Examples__

Sample program:

```fortran
    program demo_floor
    implicit none
        real :: x = 63.29
        real :: y = -63.59
        print *, floor(x) ! returns 63
        print *, floor(y) ! returns -64
    end program demo_floor
```

## __Standard__

Fortran 95 and later

## __See Also__

__ceiling__(3), __nint__(3)

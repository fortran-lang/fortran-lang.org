---
layout: book
title: ceiling
permalink: /learn/intrinsics/CEILING
---
## __Name__

__ceiling__(3) - \[NUMERIC\] Integer ceiling function
(GFDL)

## __Syntax__

result = __ceiling__(a \[, kind\])

## __Description__

__ceiling__(a) returns the least integer greater than or equal to A.

## __Arguments__

  - __A__
    The type shall be _real_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type __integer__(kind) if KIND is present and a
default-kind _integer_ otherwise.

## __Examples__

Sample program:

```fortran
    program demo_ceiling
    implicit none
    real :: x = 63.29
    real :: y = -63.59
       print *, ceiling(x) ! returns 64
       print *, ceiling(y) ! returns -63
    end program demo_ceiling
```

## __Standard__

Fortran 95 and later

## __See Also__

__floor__(3), __nint__(3)

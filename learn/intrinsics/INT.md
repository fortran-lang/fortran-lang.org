---
layout: book
title: int
permalink: /learn/intrinsics/INT
---
## __Name__

__int__(3) - \[NUMERIC:TYPE\] Convert to integer type
(GFDL)

## __Syntax__

result = __int__(a \[, kind))

## __Description__

Convert to integer type

## __Arguments__

  - __A__
    Shall be of type _integer_, _real_, or _complex_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

These functions return a _integer_ variable or array under the following
rules:

 1.  If A is of type _integer_, __int__(a) = a

 2.  If A is of type _real_ and |a| \< 1, __int__(a) equals 0. If |a| \>=
     1, then __int__(a) equals the largest integer that does not exceed
     the range of A and whose sign is the same as the sign of A.

 3.  If A is of type _complex_, rule 2 is applied to the real part of A.

## __Examples__

Sample program:

```fortran
    program demo_int
    implicit none
      integer :: i = 42
      complex :: z = (-3.7, 1.0)
      print *, int(i)
      print *, int(z), int(z,8)
    end program demo_int
```

## __Standard__

FORTRAN 77 and later

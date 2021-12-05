---
layout: book
title: dim
permalink: /learn/intrinsics/DIM
---
## __Name__

__dim__(3) - \[NUMERIC\] Positive difference
(GFDL)

## __Syntax__

result = __DIM__(X, Y)

## __Description__

__DIM__(X,Y) returns the difference X-Y if the result is positive;
otherwise returns zero.

## __Arguments__

  - __X__
    : The type shall be _integer_ or _real_

  - __Y__
    : The type shall be the same type and kind as X.

## __Returns__

The return value is of type _integer_ or _real_.

## __Examples__

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
integer :: i
real(kind=real64) :: x
    i = dim(4, 15)
    x = dim(4.345_real64, 2.111_real64)
    print *, i
    print *, x
end program demo_dim
```

## __Standard__

FORTRAN 77 and later

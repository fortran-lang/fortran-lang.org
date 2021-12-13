---
layout: book
title: dim
permalink: /learn/intrinsics/DIM
---
## __Name__

__dim__(3) - \[NUMERIC\] Positive difference
(GFDL)

## __Syntax__
```fortran
result = dim(x, y)
```
## __Description__

__dim(x,y)__ returns the difference __x - y__ if the result is positive;
otherwise returns zero.

## __Arguments__

  - __x__
    : The type shall be _integer_ or _real_

  - __y__
    : The type shall be the same type and kind as __x__.

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
  Results:
```text
              0
      2.2339999999999995     
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions

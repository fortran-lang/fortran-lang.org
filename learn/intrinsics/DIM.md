---
layout: book
title: dim
permalink: /learn/intrinsics/DIM
---
## __Name__

__dim__(3) - \[NUMERIC\] Positive difference


## __Syntax__
```fortran
result = dim(x, y)

    elemental function dim(x, y)
    type(TYPE(kind=KIND))            :: dim
    type(TYPE(kind=KIND)),intent(in) :: x, y
```
where TYPE may be _real_ or _integer_ and KIND is any supported kind for the type.
## __Description__

__dim(x,y)__ returns the difference __x - y__ if the result is positive;
otherwise it returns zero.

## __Arguments__

  - __x__
    : The type shall be _integer_ or _real_

  - __y__
    : The type shall be the same type and kind as __x__.

## __Returns__

The return value is the same type and kind as the input arguments __x__ and __y__.

## __Examples__

Sample program:

```fortran
program demo_dim
use, intrinsic :: iso_fortran_env, only : real64
implicit none
integer           :: i
real(kind=real64) :: x
    i = dim(4, 15)
    x = dim(4.321_real64, 1.111_real64)
    print *, i
    print *, x
    ! elemental
    print *, dim([1,2,3],2)
    print *, dim([1,2,3],[3,2,1])
    print *, dim(-10,[0,-10,-20])
end program demo_dim
```
Results:
```text
              0
      3.21000000000000     
              0           0           1
              0           0           2
              0           0          10
```
## __Standard__

FORTRAN 77 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost

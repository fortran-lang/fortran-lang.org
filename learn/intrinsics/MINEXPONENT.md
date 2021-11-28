---
layout: book
title: minexponent
permalink: /learn/intrinsics/MINEXPONENT
---
## __Name__

__minexponent__(3) - \[NUMERIC MODEL\] Minimum exponent of a real kind
(GFDL)

## __Syntax__

result = __minexponent__(X)

## __Description__

__minexponent(X)__ returns the minimum exponent in the model of the type
of __X__.

## __Arguments__

  - __X__
    Shall be of type _real_.

## __Returns__

The return value is of type _integer_ and of the default integer kind.

## __Examples__

Sample program:

```fortran
program demo_minexponent
use, intrinsic :: iso_fortran_env, only : &
 &real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x
real(kind=real64) :: y
    print *, minexponent(x), maxexponent(x)
    print *, minexponent(y), maxexponent(y)
end program demo_minexponent
```
Expected Results:
```
        -125         128
       -1021        1024
```
## __Standard__

Fortran 95 and later

---
layout: book
title: dim
permalink: /learn/intrinsics/DIM
---
#### NAME

__dim__(3f) - \[NUMERIC\] Positive difference
(GFDL)

#### SYNTAX

result = __DIM__(X, Y)

#### DESCRIPTION

__DIM__(X,Y) returns the difference X-Y if the result is positive;
otherwise returns zero.

#### ARGUMENTS

  - __X__
    The type shall be INTEGER or REAL

  - __Y__
    The type shall be the same type and kind as X.

#### RETURN VALUE

The return value is of type INTEGER or REAL.

#### EXAMPLE

Sample program:

```
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

#### STANDARD

FORTRAN 77 and later

#### CLASS

Elemental procedure\|Elemental function

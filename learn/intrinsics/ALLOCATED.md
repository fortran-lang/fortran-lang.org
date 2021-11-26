---
layout: book
title: allocated
permalink: /learn/intrinsics/ALLOCATED
---
-------------------------------------------------------------------------------
## __Name__

__allocated__(3) - \[ARRAY INQUIRY\] Status of an allocatable entity
(GFDL)

## __Syntax__

  - result = __ALLOCATED__(ARRAY)

  - result = __ALLOCATED__(SCALAR)

## __Description__

__ALLOCATED__(ARRAY) and __ALLOCATED__(SCALAR) check the allocation
status of ARRAY and SCALAR, respectively.

## __Arguments__

  - __ARRAY__
    the argument shall be an ALLOCATABLE array.

  - __SCALAR__
    the argument shall be an ALLOCATABLE scalar.

## __Returns__

The return value is a scalar _logical_ with the default logical kind type
parameter. If the argument is allocated then the result is .true.;
otherwise, it returns .false..

## __Examples__

Sample program:

```
    program demo_allocated
    implicit none
    integer :: i = 4
    real(4), allocatable :: x(:)
       if (allocated(x) .eqv. .false.) allocate(x(i))
    end program demo_allocated
```

## __Standard__

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

## __See Also__

[__move\_alloc__(3)](MOVE_ALLOC)

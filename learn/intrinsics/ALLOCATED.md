---
layout: book
title: allocated
permalink: /learn/intrinsics/ALLOCATED
---
#### NAME

__allocated__(3f) - \[ARRAY INQUIRY\] Status of an allocatable entity
(GFDL)

#### SYNTAX

  - result = __ALLOCATED__(ARRAY)

  - result = __ALLOCATED__(SCALAR)

#### DESCRIPTION

__ALLOCATED__(ARRAY) and __ALLOCATED__(SCALAR) check the allocation
status of ARRAY and SCALAR, respectively.

#### ARGUMENTS

  - __ARRAY__
    the argument shall be an ALLOCATABLE array.

  - __SCALAR__
    the argument shall be an ALLOCATABLE scalar.

#### RETURN VALUE

The return value is a scalar LOGICAL with the default logical kind type
parameter. If the argument is allocated then the result is .true.;
otherwise, it returns .false..

#### EXAMPLE

Sample program:

```
    program demo_allocated
    implicit none
    integer :: i = 4
    real(4), allocatable :: x(:)
       if (allocated(x) .eqv. .false.) allocate(x(i))
    end program demo_allocated
```

#### STANDARD

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

#### CLASS

Inquiry function

#### SEE ALSO

__move\_alloc__(3)

---
layout: book
title: any
permalink: /learn/intrinsics/ANY
---
#### NAME

__any__(3f) - \[ARRAY REDUCTION\] determines if any of the values in the logical array are true.
(GFDL)

#### SYNTAX

result = __any__(mask \[, dim\])

#### DESCRIPTION

__ANY__(MASK \[, DIM\]) determines if any of the values in the logical
array MASK along dimension DIM are .TRUE..

#### ARGUMENTS

  - __MASK__
    the type of the argument shall be LOGICAL and it shall not be
    scalar.

  - __DIM__
    (optional) DIM shall be a scalar integer with a value that lies
    between one and the rank of MASK.

#### RETURN VALUE

__ANY__(MASK) returns a scalar value of type LOGICAL where the kind type
parameter is the same as the kind type parameter of MASK. If DIM is
present, then __ANY__(MASK, DIM) returns an array with the rank of MASK
minus 1. The shape is determined from the shape of MASK where the DIM
dimension is elided.

1.  __ANY__(MASK) is true if any element of MASK is true; otherwise, it
    is false. It also is false if MASK has zero size.

2.  If the rank of MASK is one, then __ANY__(MASK, DIM) is equivalent to
    __ANY__(MASK). If the rank is greater than one, then __ANY__(MASK,
    DIM) is determined by applying ANY to the array sections.

#### EXAMPLE

Sample program:

```
    program demo_any
    implicit none
    logical l
       l = any([.true., .true., .true.])
       print *, l
       call section
       contains
         subroutine section
         integer a(2,3), b(2,3)
           a = 1
           b = 1
           b(2,2) = 2
           print *, any(a .eq. b, 1)
           print *, any(a .eq. b, 2)
         end subroutine section
    end program demo_any
```

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function

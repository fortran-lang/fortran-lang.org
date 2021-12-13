---
layout: book
title: any
permalink: /learn/intrinsics/ANY
---
## __Name__

__any__(3) - \[ARRAY REDUCTION\] determines if any of the values in the logical array are true.
(GFDL)

## __Syntax__
```fortran
result = any(mask, dim)
```
## __Description__

__any(mask, dim)__ determines if any of the values in the logical
array __mask__ along dimension __dim__ are __.true.__.

## __Arguments__

  - __mask__
    : the type of the argument shall be _logical_ and it shall not be
    scalar.

  - __dim__
    : (optional) dim shall be a scalar integer with a value that lies
    between one and the rank of mask.

## __Returns__

__any(mask)__ returns a scalar value of type _logical_ where the kind type
parameter is the same as the kind type parameter of __mask__. If __dim__ is
present, then __any(mask, dim)__ returns an array with the rank of __mask__
minus 1. The shape is determined from the shape of __mask__ where the __dim__
dimension is elided.

1.  __any(mask)__ is true if any element of __mask__ is true; otherwise, it
    is __.false.__. It also is false if __mask__ has zero size.

2.  If the rank of __mask__ is one, then __any(mask, dim)__ is equivalent to
    __any(mask)__. If the rank is greater than one, then __any(mask,
    dim)__ is determined by applying __any()__ to the array sections.

## __Examples__

Sample program:
```fortran
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
  Results:
```text
    T
    T T T
    T T
```
## __Standard__

Fortran 95 and later

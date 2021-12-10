---
layout: book
title: shape
permalink: /learn/intrinsics/SHAPE
---
## __Name__

__shape__(3) - \[ARRAY INQUIRY\] Determine the shape of an array
(GFDL)

## __Syntax__
```fortran
result = shape(source, kind)
```
## __Description__

Determines the shape of an array.

## __Arguments__

  - __source__
    : Shall be an array or scalar of any type. If __source__ is a pointer it
    must be associated and allocatable arrays must be allocated.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

An _integer_ array of rank one with as many elements as __source__ has
dimensions. The elements of the resulting array correspond to the extend
of __source__ along the respective dimensions. If __source__ is a scalar, the
result is the rank one array of size zero. If __kind__ is absent, the return
value has the default integer kind otherwise the specified kind.

## __Examples__

Sample program:

```fortran
program demo_shape
implicit none
integer, dimension(-1:1, -1:2) :: a
   write(*,*) shape(a)             ! [ 3, 4 ]
   write(*,*) size(shape(42))      ! [ ]
end program demo_shape
```

## __Standard__

Fortran 95 and later; with KIND argument Fortran 2003 and later

## __See Also__

[__reshape__(3)](RESHAPE),
[__size__(3)](SIZE)

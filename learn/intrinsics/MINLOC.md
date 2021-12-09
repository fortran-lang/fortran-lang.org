---
layout: book
title: minloc
permalink: /learn/intrinsics/MINLOC
---
## __Name__

__minloc__(3) - \[ARRAY LOCATION\] Location of the minimum value within an array
(GFDL)

## __Syntax__
```fortran
    result = minloc(array, dim, mask) 

    result = minloc(array, mask)
```
## __Description__

Determines the location of the element in the array with the minimum
value, or, if the __dim__ argument is supplied, determines the locations of
the minimum element along each row of the array in the __dim__ direction. If
__mask__ is present, only the elements for which __mask__ is __.true.__ are
considered. If more than one element in the array has the minimum value,
the location returned is that of the first such element in array element
order. If the array has zero size, or all of the elements of __mask__ are
.false., then the result is an array of zeroes. Similarly, if __dim__ is
supplied and all of the elements of __mask__ along a given row are zero, the
result value for that row is zero.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : Shall be an array of type _logical_, and conformable with __array__.

## __Returns__

If __dim__ is absent, the result is a rank-one array with a length equal to
the rank of __array__. If __dim__ is present, the result is an array with a rank
one less than the rank of __array__, and a size corresponding to the size of
__array__ with the __dim__ dimension removed. If __dim__ is present and __array__ has a
rank of one, the result is a scalar. In all cases, the result is of
default _integer_ type.

## __Examples__

sample program:

```fortran
program demo_minloc
implicit none
integer,save :: ints(3,5)= reshape([&
   4, 10,  1,  7, 13, &
   9, 15,  6, 12,  3, &
  14,  5, 11,  2,  8  &
],shape(ints),order=[2,1])
    write(*,*) minloc(ints)
    write(*,*) minloc(ints,dim=1)
    write(*,*) minloc(ints,dim=2)
    ! where in each column is the smallest number .gt. 10 ?
    write(*,*) minloc(ints,dim=2,mask=ints.gt.10)
    ! a one-dimensional array with dim=1 explicitly listed returns a scalar
    write(*,*) minloc(pack(ints,.true.),dim=1) ! scalar
end program demo_minloc
```
Results:

```text
         1       3
         1       3       1       3       2
         3       5       4
         5       4       3
         7
```
## __Standard__

Fortran 95 and later

## __See Also__

[__min__(3)](MIN),
[__minval__(3)](MINVAL)

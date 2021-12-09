---
layout: book
title: maxloc
permalink: /learn/intrinsics/MAXLOC
---
## __Name__

__maxloc__(3) - \[ARRAY LOCATION\] Location of the maximum value within an array
(GFDL)

## __Syntax__

result = __maxloc(array, dim \[, mask\]) result = maxloc(array \[, mask\])__

## __Description__

Determines the location of the element in the array with the maximum
value, or, if the __dim__ argument is supplied, determines the locations of
the maximum element along each row of the array in the __dim__ direction. If
__mask__ is present, only the elements for which __mask__ is __.true.__ are
considered. If more than one element in the array has the maximum value,
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

The value returned is reference to the offset from the beginning of the
array, not necessarily the subscript value if the array subscripts do
not start with one.

## __Examples__

sample program:

```fortran
program demo_maxloc
implicit none
integer      :: ii
integer,save :: i(-3:3)=[(abs(abs(ii)-50),ii=-3,3)]
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
   10, 20, 30, 40, 50, &
   11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

    write(*,*) maxloc(ints)
    write(*,*) maxloc(ints,dim=1)
    write(*,*) maxloc(ints,dim=2)
    ! when array bounds do not start with one remember MAXLOC(3) returns the
    ! offset relative to the lower bound-1 of the location of the maximum
    ! value, not the subscript of the maximum value. When the lower bound of
    ! the array is one, these values are the same. In other words, MAXLOC(3)
    ! returns the subscript of the value assuming the first subscript of the
    ! array is one no matter what the lower bound of the subscript actually
    ! is.
    write(*,'(g0,1x,g0)') (ii,i(ii),ii=lbound(i,dim=1),ubound(i,dim=1))
    write(*,*)maxloc(i)

end program demo_maxloc

expected output:
```text
      3       5
      3       3       3       3       3
      5       5       5
   -3 47
   -2 48
   -1 49
   0 50
   1 49
   2 48
   3 47
```
## __Standard__

Fortran 95 and later

## __See Also__

[__max__(3)](MAX),
[__maxval__(3)](MAXVAL)

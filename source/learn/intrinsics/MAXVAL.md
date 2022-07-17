---
layout: book
title: maxval
permalink: /learn/intrinsics/MAXVAL
---
# MAXVAL
## __Name__

__maxval__(3) - \[ARRAY REDUCTION\] determines the maximum value in an array or row


## __Syntax__
```fortran
result = maxval(array, dim, mask)
```
   or 
```fortran
result = maxval(array, mask)
```
## __Description__

Determines the maximum value of the elements in an array value, or, if
the __dim__ argument is supplied, determines the maximum value along each
row of the array in the __dim__ direction. If __mask__ is present, only the
elements for which __mask__ is __.true.__ are considered. If the array has zero
size, or all of the elements of __mask__ are .false., then the result is the
most negative number of the type and kind of __array__ if __array__ is numeric,
or a string of nulls if __array__ is of character type.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_, or _character_.

  - __dim__
    : (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of __array__, inclusive. It may not be an optional
    dummy argument.

  - __mask__
    : (Optional) Shall be an array of type _logical_, and conformable with
    __array__.

## __Returns__

If __dim__ is absent, or if __array__ has a rank of one, the result is a scalar.
If __dim__ is present, the result is an array with a rank one less than the
rank of __array__, and a size corresponding to the size of __array__ with the
__dim__ dimension removed. In all cases, the result is of the same type and
kind as __array__.

## __Examples__

sample program:

```fortran
program demo_maxval
implicit none
integer,save :: ints(3,5)= reshape([&
   1,  2,  3,  4,  5, &
  10, 20, 30, 40, 50, &
  11, 22, 33, 44, 55  &
],shape(ints),order=[2,1])

   write(*,*) maxval(ints)
   write(*,*) maxval(ints,dim=1)
   write(*,*) maxval(ints,dim=2)
   ! find biggest number less than 30 with mask
   write(*,*) maxval(ints,mask=ints.lt.30)
end program demo_maxval
```

Results:

```
   55
   11     22     33     44     55
    5     50     55
   22
```

## __Standard__

Fortran 95 and later

## __See Also__

[__max__(3)](MAX),
[__maxloc__(3)](MAXLOC)

###### fortran-lang intrinsic descriptions

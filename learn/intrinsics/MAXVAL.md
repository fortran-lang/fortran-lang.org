---
layout: book
title: maxval
permalink: /learn/intrinsics/MAXVAL
---
## __Name__

__maxval__(3) - \[ARRAY REDUCTION\] determines the maximum value in an array or row
(GFDL)

## __Syntax__

result = __maxval__(array, dim \[, mask\])

result = __maxval__(array \[, mask\])

## __Description__

Determines the maximum value of the elements in an array value, or, if
the DIM argument is supplied, determines the maximum value along each
row of the array in the DIM direction. If MASK is present, only the
elements for which MASK is .true. are considered. If the array has zero
size, or all of the elements of MASK are .false., then the result is the
most negative number of the type and kind of ARRAY if ARRAY is numeric,
or a string of nulls if ARRAY is of character type.

## __Arguments__

  - __ARRAY__
    Shall be an array of type _integer_, _real_, or CHARACTER.

  - __DIM__
    (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of ARRAY, inclusive. It may not be an optional
    dummy argument.

  - __MASK__
    (Optional) Shall be an array of type _logical_, and conformable with
    ARRAY.

## __Returns__

If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.
If DIM is present, the result is an array with a rank one less than the
rank of ARRAY, and a size corresponding to the size of ARRAY with the
DIM dimension removed. In all cases, the result is of the same type and
kind as ARRAY.

## __Examples__

sample program:

```
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

__max__(3), __maxloc__(3)

## __Category__

intrinsics

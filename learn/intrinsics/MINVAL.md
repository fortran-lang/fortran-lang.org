---
layout: book
title: minval
permalink: /learn/intrinsics/MINVAL
---
## __Name__

__minval__(3) - \[ARRAY REDUCTION\] Minimum value of an array
(GFDL)

## __Syntax__

result = __minval__(array, dim \[, mask\]) result = __minval__(array \[,
mask\])

## __Description__

Determines the minimum value of the elements in an array value, or, if
the DIM argument is supplied, determines the minimum value along each
row of the array in the DIM direction. If MASK is present, only the
elements for which MASK is .true. are considered. If the array has zero
size, or all of the elements of MASK are .false., then the result is
__huge__(array) if ARRAY is numeric, or a string of __char__(255)
characters if ARRAY is of character type.

## __Arguments__

  - __ARRAY__
    Shall be an array of type _integer_, _real_, or CHARACTER.

  - __DIM__
    (Optional) Shall be a scalar of type _integer_, with a value between
    one and the rank of ARRAY, inclusive. It may not be an optional
    dummy argument.

  - __MASK__
    Shall be an array of type _logical_, and conformable with ARRAY.

## __Returns__

If DIM is absent, or if ARRAY has a rank of one, the result is a scalar.
If DIM is present, the result is an array with a rank one less than the
rank of ARRAY, and a size corresponding to the size of ARRAY with the
DIM dimension removed. In all cases, the result is of the same type and
kind as ARRAY.

## __Examples__

sample program:

```fortran
    program demo_minval
    implicit none
    integer,save :: ints(3,5)= reshape([&
       1,  2,  3,  4,  5, &
      10, 20, 30, 40, 50, &
      11, 22, 33, 44, 55  &
    ],shape(ints),order=[2,1])
    write(*,*) minval(ints)
    write(*,*) minval(ints,dim=1)
    write(*,*) minval(ints,dim=2)
    end program demo_minval
```

results:

```text
 1
 1    2     3     4     5
 1   10    11
```

## __Standard__

Fortran 95 and later

## __See Also__

__min__(3), __minloc__(3)

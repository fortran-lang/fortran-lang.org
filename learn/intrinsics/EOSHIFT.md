---
layout: book
title: eoshift
permalink: /learn/intrinsics/EOSHIFT
---
## __Name__

__eoshift__(3) - \[TRANSFORMATIONAL FUNCTION\] End-off shift elements of an array
(GFDL)

## __Syntax__

result = __eoshift__(array, shift \[, boundary, dim\])

## __Description__

__eoshift__(array, shift\[, boundary, dim\]) performs an end-off shift
on elements of ARRAY along the dimension of DIM. If DIM is omitted it is
taken to be 1. DIM is a scalar of type _integer_ in the range of 1 \<= DIM
\<= n where "n" is the rank of ARRAY. If the rank of ARRAY is one, then
all elements of ARRAY are shifted by SHIFT places. If rank is greater
than one, then all complete rank one sections of ARRAY along the given
dimension are shifted. Elements shifted out one end of each rank one
section are dropped. If BOUNDARY is present then the corresponding value
of from BOUNDARY is copied back in the other end. If BOUNDARY is not
present then the following are copied in depending on the type of ARRAY.

\*Array Type\* - \*Boundary Value\*

   - Numeric 0 of the type and kind of ARRAY

   - Logical .false.

   - __Character__(LEN) LEN blanks

## __Arguments__

  - __ARRAY__
    May be any type, not scalar.

  - __SHIFT__
    The type shall be _integer_.

  - __BOUNDARY__
    Same type as ARRAY.

  - __DIM__
    The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the ARRAY argument.

## __Examples__

Sample program:

```fortran
    program demo_eoshift
    implicit none
        integer, dimension(3,3) :: a
        a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
        print '(3i3)', a(1,:)
        print '(3i3)', a(2,:)
        print '(3i3)', a(3,:)
        a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
        print *
        print '(3i3)', a(1,:)
        print '(3i3)', a(2,:)
        print '(3i3)', a(3,:)
    end program demo_eoshift
```

## __Standard__

Fortran 95 and later

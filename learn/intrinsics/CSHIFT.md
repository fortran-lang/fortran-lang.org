---
layout: book
title: cshift
permalink: /learn/intrinsics/CSHIFT
---
## __Name__

__cshift__(3) - \[TRANSFORMATIONAL FUNCTION\] Circular shift elements of an array
(GFDL)

## __Syntax__

result = __cshift__(array, shift \[, dim\])

## __Description__

__cshift__(array, shift \[, dim\]) performs a circular shift on elements
of ARRAY along the dimension of DIM. If DIM is omitted it is taken to be
1. DIM is a scalar of type _integer_ in the range of 1 \<= DIM \<= n,
where "n" is the rank of ARRAY. If the rank of ARRAY is one, then all
elements of ARRAY are shifted by SHIFT places. If rank is greater than
one, then all complete rank one sections of ARRAY along the given
dimension are shifted. Elements shifted out one end of each rank one
section are shifted back in the other end.

## __Arguments__

  - __ARRAY__
    Shall be an array of any type.

  - __SHIFT__
    The type shall be _integer_.

  - __DIM__
    The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the ARRAY argument.

## __Examples__

Sample program:

```fortran
    program demo_cshift
    implicit none
    integer, dimension(3,3) :: a
        a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
        print '(3i3)', a(1,:)
        print '(3i3)', a(2,:)
        print '(3i3)', a(3,:)
        a = cshift(a, SHIFT=[1, 2, -1], DIM=2)
        print *
        print '(3i3)', a(1,:)
        print '(3i3)', a(2,:)
        print '(3i3)', a(3,:)
    end program demo_cshift
```

## __Standard__

Fortran 95 and later

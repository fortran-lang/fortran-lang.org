---
layout: book
title: iany
permalink: /learn/intrinsics/IANY
---
## __Name__

__iany__(3) - \[BIT MANIPULATION\] Bitwise or of array elements
(GFDL)

## __Syntax__

  - result = __iany__(array\[, mask\])

  - result = __iany__(array, dim\[, mask\])

## __Description__

Reduces with bitwise or (inclusive or) the elements of ARRAY along
dimension DIM if the corresponding element in MASK is TRUE.

## __Arguments__

  - __ARRAY__
    Shall be an array of type _integer_

  - __DIM__
    (Optional) shall be a scalar of type _integer_ with a value in the
    range from "1" to "n", where "n" equals the rank of ARRAY.

  - __MASK__
    (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as ARRAY.

## __Returns__

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the bitwise OR of all elements in ARRAY
is returned. Otherwise, an array of rank "n-1", where "n" equals the
rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM
dropped is returned.

## __Examples__

Sample program:

```fortran
   program demo_iany
   use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
   implicit none
   integer(kind=int8) :: a(2)
     a(1) = int(b'00100100')
     a(2) = int(b'01101010')
     print '(b8.8)', iany(a)
   end program demo_iany
```

Results:

```
   01101110
```

## __Standard__

Fortran 2008 and later

## __See Also__

__iparity__(3), __iall__(3), __ior__(3)

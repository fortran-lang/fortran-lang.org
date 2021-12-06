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

Reduces with bitwise or (inclusive or) the elements of __array__ along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _or_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
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

[__iparity__(3)](IPARITY),
[__iall__(3)](IALL),
[__ior__(3)](IOR)

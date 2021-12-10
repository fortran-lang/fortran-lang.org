---
layout: book
title: iparity
permalink: /learn/intrinsics/IPARITY
---
## __Name__

__iparity__(3) - \[BIT MANIPULATION\] Bitwise exclusive or of array elements
(GFDL)

## __Syntax__
```fortran
  result = iparity(array, mask)

   or

  result = iparity(array, dim, mask)
```
## __Description__

Reduces with bitwise _xor_ (exclusive _or_) the elements of __array__ along
dimension __dim__ if the corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __"1" to "n"__, where __"n"__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the bitwise _xor_ of all elements in __array__
is returned. Otherwise, an array of rank __n-1__, where __n__ equals the
rank of __array__, and a shape similar to that of __array__ with dimension __dim__
dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_iparity
implicit none
integer, dimension(2) :: a
  a(1) = int(b'00100100')
  a(2) = int(b'01101010')
  print '(b8.8)', iparity(a)
end program demo_iparity
```

Results:

```
   01001110
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__iany__(3)](IANY),
[__iall__(3)](IALL),
[__ieor__(3)](IEOR),
[__parity__(3)](PARITY)

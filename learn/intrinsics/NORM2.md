---
layout: book
title: norm2
permalink: /learn/intrinsics/NORM2
---
## __Name__

__norm2__(3) - \[MATHEMATICS\] Euclidean vector norm
(GFDL)

## __Syntax__

result = __norm2__(array\[, dim\])

## __Description__

Calculates the Euclidean vector norm (L\_2 norm) of __array__ along
dimension __dim__.

## __Arguments__

  - __array__
    : Shall be an array of type _real_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from 1 to n, where n equals the rank of __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the square root of the sum of squares of
the elements of __array__ is returned. Otherwise, an array of rank __n-1__,
where __n__ equals the rank of __array__, and a shape similar to that of __array__
with dimension DIM dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_norm2
implicit none
   real :: x(5) = [ real :: 1, 2, 3, 4, 5 ]
   print *, norm2(x)  ! = sqrt(55.) ~ 7.416
end program demo_norm2
```

## __Standard__

Fortran 2008 and later

## __See Also__

[__product__(3)](PRODUCT),
[__sum__(3)](SUM),
[__hypot__(3)](HYPOT)

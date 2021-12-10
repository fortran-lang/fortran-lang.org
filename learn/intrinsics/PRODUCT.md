---
layout: book
title: product
permalink: /learn/intrinsics/PRODUCT
---
## __Name__

__product__(3) - \[ARRAY REDUCTION\] Product of array elements
(GFDL)

## __Syntax__

```fortran
  result = product(array, mask)
```
   or
```fortran
  result = product(array, dim, mask)
```
## __Description__

Multiplies the elements of __array__ along dimension __dim__ if the
corresponding element in __mask__ is __.true.__.

## __Arguments__

  - __array__
    : Shall be an array of type _integer_, _real_ or _complex_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

  - __mask__
    : (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as __array__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the product of all elements in __array__ is
returned. Otherwise, an array of rank __n-1__, where __n__ equals the rank of
__array__, and a shape similar to that of __array__ with dimension __dim__ dropped
is returned.

## __Examples__

Sample program:

```fortran
program demo_product
implicit none
integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
   print *, product(x)                    ! all elements, product = 120
   print *, product(x, mask=mod(x, 2)==1) ! odd elements, product = 15
end program demo_product
```

## __Standard__

Fortran 95 and later

## __See Also__

[__sum__(3)](SUM), note that an element by element multiplication is done
directly using the star character.

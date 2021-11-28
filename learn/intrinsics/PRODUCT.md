---
layout: book
title: product
permalink: /learn/intrinsics/PRODUCT
---
## __Name__

__product__(3) - \[ARRAY REDUCTION\] Product of array elements
(GFDL)

## __Syntax__

  - result = __product__(array\[, mask\])

  - result = __product__(array, dim\[, mask\])

## __Description__

Multiplies the elements of ARRAY along dimension DIM if the
corresponding element in MASK is TRUE.

## __Arguments__

  - __ARRAY__
    Shall be an array of type _integer_, _real_ or _complex_.

  - __DIM__
    (Optional) shall be a scalar of type _integer_ with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

  - __MASK__
    (Optional) shall be of type _logical_ and either be a scalar or an
    array of the same shape as ARRAY.

## __Returns__

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the product of all elements in ARRAY is
returned. Otherwise, an array of rank n-1, where n equals the rank of
ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped
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

__sum__(3), note that an element by element multiplication is done
directly using the star character.

---
layout: book
title: product
permalink: /learn/intrinsics/f_product
---
### NAME

**product**(3f) - \[ARRAY REDUCTION\] Product of array
elements

### SYNTAX

  - result = **product**(array\[, mask\])

  - result = **product**(array, dim\[, mask\])

### DESCRIPTION

Multiplies the elements of ARRAY along dimension DIM if the
corresponding element in MASK is TRUE.

### ARGUMENTS

  - **ARRAY**
    Shall be an array of type INTEGER, REAL or COMPLEX.

  - **DIM**
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

  - **MASK**
    (Optional) shall be of type LOGICAL and either be a scalar or an
    array of the same shape as ARRAY.

### RETURN VALUE

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the product of all elements in ARRAY is
returned. Otherwise, an array of rank n-1, where n equals the rank of
ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped
is returned.

### EXAMPLE

Sample program:

```
    program demo_product
    implicit none
      integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
      print *, product(x)                    ! all elements, product = 120
      print *, product(x, mask=mod(x, 2)==1) ! odd elements, product = 15
    end program demo_product
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

**sum**(3), note that an element by element multiplication is done
directly using the star character.

---
layout: book
title: iany
permalink: /learn/intrinsics/IANY
---
### NAME

__iany__(3f) - \[BIT MANIPULATION\] Bitwise or of array elements
(GFDL)

### SYNTAX

  - result = __iany__(array\[, mask\])

  - result = __iany__(array, dim\[, mask\])

### DESCRIPTION

Reduces with bitwise or (inclusive or) the elements of ARRAY along
dimension DIM if the corresponding element in MASK is TRUE.

### ARGUMENTS

  - __ARRAY__
    Shall be an array of type INTEGER

  - __DIM__
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from "1" to "n", where "n" equals the rank of ARRAY.

  - __MASK__
    (Optional) shall be of type LOGICAL and either be a scalar or an
    array of the same shape as ARRAY.

### RETURN VALUE

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the bitwise OR of all elements in ARRAY
is returned. Otherwise, an array of rank "n-1", where "n" equals the
rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM
dropped is returned.

### EXAMPLE

Sample program:

```
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

### STANDARD

Fortran 2008 and later

### CLASS

Transformational function

### SEE ALSO

__iparity__(3), __iall__(3), __ior__(3)

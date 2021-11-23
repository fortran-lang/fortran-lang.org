---
layout: book
title: iall
permalink: /learn/intrinsics/IALL
---
### NAME

**iall**(3f) - \[BIT MANIPULATION\] Bitwise and of array elements
(GFDL)

### SYNTAX

  - result = **iall**(array\[, mask\])

  - result = **iall**(array, dim\[, mask\])

### DESCRIPTION

Reduces with bitwise AND the elements of ARRAY along dimension DIM if
the corresponding element in MASK is TRUE.

### ARGUMENTS

  - **ARRAY**
    Shall be an array of type INTEGER

  - **DIM**
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from 1 to "n", where "n" equals the rank of ARRAY.

  - **MASK**
    (Optional) shall be of type LOGICAL and either be a scalar or an
    array of the same shape as ARRAY.

### RETURN VALUE

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the bitwise ALL of all elements in ARRAY
is returned. Otherwise, an array of rank "n-1", where "n" equals the
rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM
dropped is returned.

### EXAMPLE

Sample program:

```
   program demo_iall
   use, intrinsic :: iso_fortran_env, only : integer_kinds, &
   & int8, int16, int32, int64
   implicit none
   integer(kind=int8) :: a(2)

     a(1) = int(b'00100100')
     a(2) = int(b'01101010')

     ! prints 00100000
     print '(b8.8)', iall(a)
```

end program demo\_iall

### STANDARD

Fortran 2008 and later

### CLASS

Transformational function

### SEE ALSO

**iany**(3), **iparity**(3), **iand**(3)

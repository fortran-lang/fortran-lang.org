---
layout: book
title: iparity
permalink: /learn/intrinsics/IPARITY
---
#### NAME

__iparity__(3f) - \[BIT MANIPULATION\] Bitwise exclusive or of array elements
(GFDL)

#### SYNTAX

  - result = __iparity__(array\[, mask\])

  - result = __iparity__(array, dim\[, mask\])

#### DESCRIPTION

Reduces with bitwise XOR (exclusive OR) the elements of ARRAY along
dimension DIM if the corresponding element in MASK is TRUE.

#### ARGUMENTS

  - __ARRAY__
    Shall be an array of type INTEGER

  - __DIM__
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from "1" to "n", where "n" equals the rank of ARRAY.

  - __MASK__
    (Optional) shall be of type LOGICAL and either be a scalar or an
    array of the same shape as ARRAY.

#### RETURN VALUE

The result is of the same type as ARRAY.

If DIM is absent, a scalar with the bitwise XOR of all elements in ARRAY
is returned. Otherwise, an array of rank "n-1", where "n" equals the
rank of ARRAY, and a shape similar to that of ARRAY with dimension DIM
dropped is returned.

#### EXAMPLE

Sample program:

```
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

#### STANDARD

Fortran 2008 and later

#### CLASS

Transformational function

#### SEE ALSO

__iany__(3), __iall__(3), __ieor__(3), __parity__(3)

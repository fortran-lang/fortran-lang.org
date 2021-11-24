---
layout: book
title: sum
permalink: /learn/intrinsics/SUM
---
#### NAME

__sum__(3f) - \[ARRAY REDUCTION\] sum the elements of an array
(GFDL)

#### SYNTAX

Calling sequence:

```
   result = sum(array[, mask])
   result = sum(array, dim[, mask])
```

#### DESCRIPTION

Adds the elements of ARRAY along dimension DIM if the corresponding
element in MASK is TRUE.

#### ARGUMENTS

  - __array__
    Shall be an array of type INTEGER, REAL or COMPLEX.

  - __dim__
    (Optional) shall be a scalar of type INTEGER with a value in the
    range from 1 to n, where n equals the rank of ARRAY.

  - __mask__
    (Optional) shall be of type LOGICAL and either be a scalar or an
    array of the same shape as ARRAY.

#### RETURN VALUE

The result is of the same type as ARRAY.

If __dim__(3f) is absent, a scalar with the sum of all elements in ARRAY
is returned. Otherwise, an array of rank n-1, where n equals the rank of
ARRAY, and a shape similar to that of ARRAY with dimension DIM dropped
is returned.

#### EXAMPLE

Sample program:

```
    program simple_sum
    implicit none
      integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
      print *, sum(x)                        ! all elements, sum = 15
      print *, sum(x, mask=mod(x, 2)==1)     ! odd elements, sum = 9
    end program simple_sum
```

Demonstrate Fortran 90 SUM function with MASK option

```
   program demo_sum
   ! John Mahaffy  2/16/96
   implicit none
   integer nd, ndh, nduh, j
   parameter (nd=10,ndh=nd/2, nduh=nd-ndh)
   real csum, cpsum, cbpsum
   real, dimension(nd):: c=[(j, j=-1,nd-2)], b
   data b/ndh*-1.0, nduh*2.0/
      csum= sum(c(1:nd))
      cpsum= sum (c(1:nd), mask=c.gt.0)
      cbpsum= sum(c(1:nd), mask=b.gt.0.0)
      print *, 'Sum of all elements in c = ' , csum
      print *, 'Sum of Positive elements in c = ', cpsum
      print *, 'Sum of elements in c when corresponding elements in b>0', &
      & ' =', cbpsum
   end program demo_sum
```

Results:

```
    Sum of all elements in c =    35.0000000
    Sum of Positive elements in c =    36.0000000
    Sum of elements in c when corresponding elements in b>0 =   30.0000000
```

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function

#### SEE ALSO

intrinsics

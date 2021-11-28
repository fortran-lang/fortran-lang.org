---
layout: book
title: unpack
permalink: /learn/intrinsics/UNPACK
---
## __Name__

__unpack__(3) - \[ARRAY CONSTRUCTION\] Store the elements of a vector in an array of higher rank
(GFDL)

## __Syntax__

result = __unpack__(vector, mask, field)

## __Description__

Store the elements of VECTOR in an array of higher rank.

## __Arguments__

  - __VECTOR__
    Shall be an array of any type and rank one. It shall have at least
    as many elements as MASK has TRUE values.

  - __MASK__
    Shall be an array of type _logical_.

  - __FIELD__
    Shall be of the same type as VECTOR and have the same shape as MASK.

## __Returns__

The resulting array corresponds to FIELD with TRUE elements of MASK
replaced by values from VECTOR in array element order.

## __Examples__

Sample program:

```fortran
    program demo_unpack
    implicit none
      integer :: vector(2)  = [1,1]
      logical :: mask(4)  = [ .true., .false., .false., .true. ]
      integer :: field(2,2) = 0, unity(2,2)

      ! result: unity matrix
      unity = unpack(vector, reshape(mask, [2,2]), field)
    end program demo_unpack
```

## __Standard__

Fortran 95 and later

## __See Also__

__pack__(3), __spread__(3)

---
layout: book
title: unpack
permalink: /learn/intrinsics/UNPACK
---
## __Name__

__unpack__(3) - \[ARRAY CONSTRUCTION\] Store the elements of a vector in an array of higher rank
(GFDL)

## __Syntax__
```fortran
result = unpack(vector, mask, field)
```
## __Description__

Store the elements of __vector__ in an array of higher rank.

## __Arguments__

  - __vector__
    : Shall be an array of any type and rank one. It shall have at least
    as many elements as __mask__ has __.true.__ values.

  - __mask__
    : Shall be an array of type _logical_.

  - __field__
    : Shall be of the same type as __vector__ and have the same shape as __mask__.

## __Returns__

The resulting array corresponds to __field__ with __.true.__ elements of __mask__
replaced by values from __vector__ in array element order.

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

[__pack__(3)](PACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)   

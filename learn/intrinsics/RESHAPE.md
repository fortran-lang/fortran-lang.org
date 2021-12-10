---
layout: book
title: reshape
permalink: /learn/intrinsics/RESHAPE
---
## __Name__

__reshape__(3) - \[ARRAY RESHAPE\] Function to reshape an array
(GFDL)

## __Syntax__
```fortran
result = reshape(source, shape, pad, order)
```
## __Description__

Reshapes array __source__ to correspond to __shape__. If necessary, the new
array may be padded with elements from __pad__ or permuted as defined by
__order__.

## __Arguments__

  - __source__
    : an array of any type.

  - __shape__
    : an array of rank one and type _integer_. Its values must be positive
    or zero.

  - __pad__
    : (Optional) an array of the same type as __source__.

  - __order__
    : (Optional) an array of type _integer_ and the same shape as __shape__. Its
    values shall be a permutation of the numbers from 1 to n, where n is
    the size of __shape__. If __order__ is absent, the natural ordering shall be
    assumed.

## __Returns__

The result is an array of shape __shape__ with the same type as __source__.

## __Examples__

Sample program:

```fortran
program demo_reshape
implicit none
integer :: i
integer, dimension(4) :: x=[(i,i=10,40,10)]
    ! x is originally a vector with four elements
    write(*,*) shape(x)                     ! prints "4"
    write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"
end program demo_reshape
```
Results
```text
   4
   2 2
```
## __Standard__

Fortran 95 and later

## __See Also__

[__shape__(3)](SHAPE)

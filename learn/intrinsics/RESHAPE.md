---
layout: book
title: reshape
permalink: /learn/intrinsics/RESHAPE
---
#### NAME

__reshape__(3f) - \[ARRAY RESHAPE\] Function to reshape an array
(GFDL)

#### SYNTAX

result = __reshape__(source, shape\[, pad, order\])

#### DESCRIPTION

Reshapes array SOURCE to correspond to SHAPE. If necessary, the new
array may be padded with elements from PAD or permuted as defined by
ORDER.

#### ARGUMENTS

  - __SOURCE__
    an array of any type.

  - __SHAPE__
    an array of rank one and type INTEGER. Its values must be positive
    or zero.

  - __PAD__
    (Optional) an array of the same type as SOURCE.

  - __ORDER__
    (Optional) an array of type INTEGER and the same shape as SHAPE. Its
    values shall be a permutation of the numbers from 1 to n, where n is
    the size of SHAPE. If ORDER is absent, the natural ordering shall be
    assumed.

#### RETURN VALUE

The result is an array of shape SHAPE with the same type as SOURCE.

#### EXAMPLE

Sample program:

```
    program demo_reshape
    implicit none
    integer :: i
    integer, dimension(4) :: x=[(i,i=10,40,10)]
      ! X is originally a vector with four elements
      write(*,*) shape(x)                     ! prints "4"
      write(*,*) shape(reshape(x, [2, 2]))    ! prints "2 2"
    end program demo_reshape
```

Results

> 4
>
>   - __2__

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function

#### SEE ALSO

__shape__(3)

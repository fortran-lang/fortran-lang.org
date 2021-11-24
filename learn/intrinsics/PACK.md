---
layout: book
title: pack
permalink: /learn/intrinsics/PACK
---
#### NAME

__pack__(3f) - \[ARRAY CONSTRUCTION\] Pack an array into an array of rank one
(GFDL)

#### SYNTAX

result = __pack__(array, mask\[,vector\])

#### DESCRIPTION

Stores the elements of ARRAY in an array of rank one.

The beginning of the resulting array is made up of elements whose MASK
equals TRUE. Afterwards, positions are filled with elements taken from
VECTOR.

#### ARGUMENTS

  - __ARRAY__
    Shall be an array of any type.

  - __MASK__
    Shall be an array of type LOGICAL and of the same size as ARRAY.
    Alternatively, it may be a LOGICAL scalar.

  - __VECTOR__
    (Optional) shall be an array of the same type as ARRAY and of rank
    one. If present, the number of elements in VECTOR shall be equal to
    or greater than the number of true elements in MASK. If MASK is
    scalar, the number of elements in VECTOR shall be equal to or
    greater than the number of elements in ARRAY.

#### RETURN VALUE

The result is an array of rank one and the same type as that of ARRAY.
If VECTOR is present, the result size is that of VECTOR, the number of
TRUE values in MASK otherwise.

#### EXAMPLE

Sample program:

```
    program demo_pack
    implicit none
    call test1()
    call test2()
    call test3()
    contains
    !
    subroutine test1()
    ! gathering nonzero elements from an array:
    integer :: m(6)
      m = [ 1, 0, 0, 0, 5, 0 ]
      write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)  ! "1 5"
    end subroutine test1
    !
    subroutine test2()
    ! Gathering nonzero elements from an array and appending elements
    ! from VECTOR:
    integer :: m(4)
      m = [ 1, 0, 0, 2 ]
      write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])  ! "1 2 3 4"
    end subroutine test2
    !
    subroutine test3()
    ! select strings whose second character is "a"
    character(len=10) :: m(4)
    m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
      write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )  ! "bat" "cat"
    end subroutine test3
    !
    end program demo_pack
```

Results:

> 1 5 1 2 3 4
>
>   - __bat__
>     cat

#### STANDARD

Fortran 95 and later

#### CLASS

Transformational function

#### SEE ALSO

__unpack__(3)

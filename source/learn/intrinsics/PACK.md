---
layout: book
title: pack
permalink: /learn/intrinsics/PACK
---
# PACK
## __Name__

__pack__(3) - \[ARRAY CONSTRUCTION\] Pack an array into an array of rank one

## __Syntax__
```fortran
result = pack(array, mask,vector)

   TYPE(kind=KIND) function pack(array,mask,vector)
   TYPE(kind=KIND),option(in) :: array(*)
   logical  :: mask(*) 
   TYPE(kind=KIND),option(in),optional :: vector(*)
```
   where TYPE(kind=KIND) may be any type, where __array__ and __vector__
   and the returned value must by of the same type. __mask__ may be a
   scalar as well an an array.

## __Description__

Stores the elements of ARRAY in an array of rank one.

The beginning of the resulting array is made up of elements whose __mask__
equals __.true.__. Afterwards, positions are filled with elements taken from
__vector__.

## __Arguments__

  - __array__
    : Shall be an array of any type.

  - __mask__
    : Shall be an array of type _logical_ and of the same size as __array__.
    Alternatively, it may be a _logical_ scalar.

  - __vector__
    : (Optional) shall be an array of the same type as __array__ and of rank
    one. If present, the number of elements in __vector__ shall be equal to
    or greater than the number of true elements in __mask__. If __mask__ is
    scalar, the number of elements in __vector__ shall be equal to or
    greater than the number of elements in __array__.

## __Returns__

The result is an array of rank one and the same type as that of __array__.
If __vector__ is present, the result size is that of __vector__, the number of
__.true.__ values in __mask__ otherwise.

## __Examples__

Sample program:

```fortran
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
! from VECTOR till the size of the mask array (or array size if the
! mask is scalar):
integer :: m(4)

   m = [ 1, 0, 0, 2 ]
   write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])

end subroutine test2
!
subroutine test3()
! select strings whose second character is "a"
character(len=10) :: m(4)

m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
   write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )

end subroutine test3
!
end program demo_pack
```
  Results:
```text
   1 5 
   1 2 3 4 
   bat        cat        
```

## __Standard__

Fortran 95 and later

## __See Also__

[__unpack__(3)](UNPACK),
[__merge__(3)](MERGE),
[__pack__(3)](PACK),
[__spread__(3)](SPREAD),
[__unpack__(3)](UNPACK)   

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost

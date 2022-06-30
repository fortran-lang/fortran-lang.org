---
layout: book
title: allocated
permalink: /learn/intrinsics/ALLOCATED
---
## __Name__

__allocated__(3) - \[ARRAY INQUIRY\] Status of an allocatable entity


## __Syntax__
```fortran
  result = allocated(array)
```
   or
```fortran
  result = allocated(scalar)
```
## __Description__

__allocated(array)__ and __allocated(scalar)__ check the allocation
status of __array__ and __scalar__, respectively.

## __Arguments__

  - __array__
    : the argument shall be an _allocatable_ array.

  - __scalar__
    : the argument shall be an _allocatable_ scalar.

## __Returns__

The return value is a scalar _logical_ with the default logical kind type
parameter. If the argument is allocated then the result is .true.;
otherwise, it returns .false..

## __Examples__

Sample program:

```fortran
program demo_allocated
use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
implicit none
integer :: i = 4
real(kind=sp), allocatable :: x(:)

   ! if already allocated, deallocate
   if ( allocated(x) ) deallocate(x)

   ! only if not allocated, allocate
   if ( .not. allocated(x) ) allocate(x(i))

   write(*,*)allocated(x), size(x)
   if( allocated(x)) then
       write(*,*)'do things if allocated'
   else
       write(*,*)'do things if not allocated'
   endif
   call intentout(x)
   write(*,*)'note it is deallocated!',allocated(x)
   contains
   subroutine intentout(arr)
   ! note that if arr has intent(out) and is allocatable,
   ! arr is deallocated on entry
   real(kind=sp),intent(out),allocatable :: arr(:)
       write(*,*)'note it was allocated in calling program',allocated(arr)
   end subroutine intentout

end program demo_allocated
```
  Results:
```text
    T           4
    do things if allocated
    note it was allocated in calling program F
    note it is deallocated! F
```

## __Standard__

Fortran 95 and later. Note, the scalar= keyword and allocatable
scalar entities are available in Fortran 2003 and later.

## __See Also__

[__move\_alloc__(3)](MOVE_ALLOC)

###### fortran-lang intrinsic descriptions

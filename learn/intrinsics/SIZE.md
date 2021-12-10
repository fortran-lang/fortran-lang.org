---
layout: book
title: size
permalink: /learn/intrinsics/SIZE
---
## __Name__

__size__(3) - \[ARRAY INQUIRY\] Determine the size of an array
(GFDL)

## __Syntax__
```fortran
result = size(array, dim, kind)
```
## __Description__

Determine the extent of __array__ along a specified dimension __dim__,
or the total number of elements in __array__ if __dim__ is absent.

## __Arguments__

  - __array__
    : be an array of any type. If __array__ is a pointer it must be
    associated and allocatable arrays must be allocated.

  - __dim__
    : shall be a scalar of type _integer_ and its value shall be
    in the range from 1 to n, where n equals the rank of __array__.

  - __kind__
    : An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__
is absent, the return value is of default _integer_ kind.

## __Examples__

Sample program:

```fortran
program demo_size
implicit none
integer :: i, j
integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])
   write(*,*) 'SIZE of simple one-dimensional array=', &
   & size([ 11, 22, 33 ])    ! 3

   write(*,*)'body'
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is not "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)

   call interfaced(arr,arr)
   call nointerface(arr)
contains

subroutine interfaced(arr,arr2)
integer,intent(in)  :: arr(:,:)
integer,intent(in)  :: arr2(2,*)
   !
   write(*,*)'interfaced assumed-shape arr2ay'
   !
   ! source argument of shape intrinsic at (1) must not be
   ! an assumed size array
   !!write(*,*)'SHAPE(arr2)       :',shape(arr2)
   ! The upper bound in the last dimension must appear in the reference
   ! to the assumed size array    arr2    at (1)
   !!write(*,*)'SIZE(arr2)        :',size(arr2)
   write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
   !    dim    argument of    size    intrinsic at (1) is not
   !a valid dimension index
   !!write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   write(*,*)'LBOUND(arr2)      :',lbound(arr2)
   ! The upper bound in the last dimension must appear in the
   ! reference to the assumed size array    arr2    at (1)
   !!write(*,*)'UBOUND(arr2)      :',ubound(arr2)
   write(*,*)'LBOUND(arr2,DIM=1):',lbound(arr2,dim=1)
   write(*,*)'UBOUND(arr2,DIM=1):',ubound(arr2,dim=1)
   write(*,*)'LBOUND(arr2,DIM=2):',lbound(arr2,dim=2)
   !    dim    argument of    ubound    intrinsic at (1) is not
   ! a valid dimension index
   !!write(*,*)'UBOUND(arr2,DIM=2):',ubound(arr2,dim=2)
   !
   write(*,*)'interfaced'
   !
   write(*,*)'SHAPE(arr)       :',shape(arr)
   write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
   write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'LBOUND(arr)      :',lbound(arr)
   write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
   write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
   !
end subroutine interfaced
!!
! NOTE: If NOINTERFACE(3) had an assumed-shape argument with :
!       for dimensions it could only be properly called with
!       an explicit interface
!!
subroutine nointerface(arr)
integer,intent(in) :: arr(3,*)
   write(*,*)'nointerface'
 ! SHAPE(3) CANNOT BE USED ON AN ASSUMED SIZE ARRAY
 !!write(*,*)'SHAPE(arr)       :',shape(arr)
 !!write(*,*)'SIZE(arr)        :',size(arr)
   write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
 ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
 !!write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
   write(*,*)'note lower bound is "1"'
   write(*,*)'LBOUND(arr)      :',lbound(arr)
 !!write(*,*)'UBOUND(arr)      :',ubound(arr)
   write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
   write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
   write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
 !!write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
end subroutine nointerface
!!
end program demo_size
```
Expected results:

```text
    SIZE of simple one-dimensional array=           3
    body
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is not "1"
    LBOUND(arr)      :           0          -5
    UBOUND(arr)      :           2           5
    LBOUND(arr,DIM=1):           0
    UBOUND(arr,DIM=1):           2
    LBOUND(arr,DIM=2):          -5
    UBOUND(arr,DIM=2):           5
    interfaced assumed-shape arr2ay
    SIZE(arr2,DIM=1)  :           2
    note lower bound is "1"
    LBOUND(arr2)      :           1           1
    LBOUND(arr2)      :           1           1
    LBOUND(arr2,DIM=1):           1
    UBOUND(arr2,DIM=1):           2
    LBOUND(arr2,DIM=2):           1
    interfaced
    SHAPE(arr)       :           3          11
    SIZE(arr)        :          33
    SIZE(arr,DIM=1)  :           3
    SIZE(arr,DIM=2)  :          11
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr)      :           1           1
    UBOUND(arr)      :           3          11
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
    UBOUND(arr,DIM=2):          11
    nointerface
    SIZE(arr,DIM=1)  :           3
    note lower bound is "1"
    LBOUND(arr)      :           1           1
    LBOUND(arr,DIM=1):           1
    UBOUND(arr,DIM=1):           3
    LBOUND(arr,DIM=2):           1
```
## __Standard__

Fortran 95 and later, with __kind__ argument - Fortran 2003 and later

## __See Also__

[__shape__(3)](SHAPE),
[__reshape__(3])(RESHAPE)

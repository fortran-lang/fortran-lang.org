---
layout: book
title: move_alloc
permalink: /learn/intrinsics/MOVE_ALLOC
---
## __Name__

__move\_alloc__(3) - \[\] Move allocation from one object to another
(GFDL)

## __Syntax__
```fortran
call move_alloc(src, dest)
```
## __Description__

__move\_alloc__(src, dest) moves the allocation from SRC to DEST. SRC
will become deallocated in the process.

## __Arguments__

  - __src__
    : allocatable, __intent__(inout), may be of any type and kind.

  - __dest__
    : allocatable, __intent__(out), shall be of the same type, kind and
    rank as SRC.

## __Examples__

Basic Sample program to allocate a bigger grid

```fortran
program demo_move_alloc
implicit none
! Example to allocate a bigger GRID
real, allocatable :: grid(:), tempgrid(:)
integer :: n, i

   ! initialize small GRID
   n = 3
   allocate (grid(1:n))
   grid = [ (real (i), i=1,n) ]

   ! initialize TEMPGRID which will be used to replace GRID
   allocate (tempgrid(1:2*n))    ! Allocate bigger grid
   tempgrid(::2)  = grid         ! Distribute values to new locations
   tempgrid(2::2) = grid + 0.5   ! initialize other values

   ! move TEMPGRID to GRID
   call MOVE_ALLOC (from=tempgrid, to=grid)

   ! TEMPGRID should no longer be allocated
   ! and GRID should be the size TEMPGRID was
   if (size (grid) /= 2*n .or. allocated (tempgrid)) then
      print *, "Failure in move_alloc!"
   endif
   print *, allocated(grid), allocated(tempgrid)
   print '(99f8.3)', grid
end program demo_move_alloc
```

## __Standard__

Fortran 2003 and later

## __See Also__

[__allocated__(3)](ALLOCATED)

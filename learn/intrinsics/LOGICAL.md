---
layout: book
title: logical
permalink: /learn/intrinsics/LOGICAL
---
## __Name__

__logical__(3) - \[TYPE:LOGICAL\] Converts one kind of _logical_ variable to another
(GFDL)

## __Syntax__
```fortran
result = logical(l, kind)

 logical(kind=KIND) function logical(L,KIND)
  logical(kind=INK),intent(in) :: L
  integer,intent(in),optional :: KIND
```
## __Description__

Converts one kind of _logical_ variable to another.

## __Arguments__


  - __l__
    : The type shall be _logical_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is a _logical_ value equal to __l__, with a kind
corresponding to __kind__, or of the default logical kind if __kind__ is not
given.

## __Examples__
```fortran
program demo_logical
! Access array containing the kind type parameter values supported by this
! compiler for entities of logical type
use iso_fortran_env, only : logical_kinds

   ! list kind values supported on this platform, which generally vary
   ! in storage size
   do i =1, size(logical_kinds) 
      write(*,*)logical_kinds(i)
   enddo

end program demo_logical
```
  Results:
```text
              1
              2
              4
              8
             16
```
## __Standard__

Fortran 95 and later, related ISO_FORTRAN_ENV module - fortran 2009

## __See Also__

[__int__(3)](INT),
[__real__(3)](REAL),
[__cmplx__(3)](CMPLX)

###### fortran-lang intrinsic descriptions

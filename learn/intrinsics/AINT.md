---
layout: book
title: aint
permalink: /learn/intrinsics/AINT
---
## __Name__

__aint__(3) - \[NUMERIC\] Truncate to a whole number


## __Syntax__
```fortran
result = aint(x)

   real(kind=kind(x)),elemental  :: aint
   real(kind=kind(x)),intent(in) :: x
```
or
```fortran
result = aint(x, KIND)

   real(kind=KIND),elemental     :: aint
   integer,intent(in),optional   :: KIND
   real(kind=kind(x)),intent(in) :: x
```
## __Description__

__aint(x, kind)__ truncates its argument to a whole number.

## __Arguments__

  - __x__
    : the type of the argument shall be _real_.

  - __kind__
    : (optional) an _integer_ initialization expression indicating the
    kind parameter of the result.

## __Returns__

The return value is of type _real_ with the kind type parameter of
the argument if the optional __kind__ is absent; otherwise, the kind
type parameter will be given by __kind__. If the magnitude of __x__
is less than one, __aint(x)__ returns zero. If the magnitude is equal
to or greater than one then it returns the largest whole number that
does not exceed its magnitude. The sign is the same as the sign of __x__.

## __Examples__

Sample program:

```fortran
program demo_aint
use, intrinsic :: iso_fortran_env, only : real32, real64
implicit none
real(kind=real32) :: x4
real(kind=real64) :: x8

   x4 = 4.3210_real32
   x8 = 4.3210_real64
   print *, aint(x4), aint(x8)
   print *
   ! elemental
   print *,aint([ &
    &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
    &  0.0,   &
    &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

end program demo_aint
```
  Results:
```text
     4.00000000       4.0000000000000000     
   
    -2.00000000      -2.00000000      -2.00000000      -2.00000000
    -1.00000000      -1.00000000      -0.00000000       0.00000000
     0.00000000       1.00000000       1.00000000       2.00000000
     2.00000000       2.00000000       2.00000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__

[__anint__(3)](ANINT),
[__int__(3)](INT),
[__nint__(3)](NINT),
[__selected_int_kind__(3)](SELECTED_INT_KIND),
[__ceiling__(3)](CEILING),
[__floor__(3)](FLOOR)

###### fortran-lang intrinsic descriptions

---
layout: book
title: dprod
permalink: /learn/intrinsics/DPROD
---
## __Name__

__dprod__(3) - \[NUMERIC\] Double product function
(GFDL)

## __Syntax__
```fortran
result = dprod(x, y)
```
## __Description__

__dprod(x,y)__ produces a higher _doubleprecision_ product of default _real_
numbers __x__ and __y__.

The result has a value equal to a processor-dependent approximation to
the product of __x__ and __y__. It is recommended that the processor compute the
product in double precision, rather than in single precision and then
converted to double precision.

  - __x__
    : shall be default real.

  - __y__
    : shall be default real.

The setting of compiler options specifying _real_ size can affect this
function.

## __Arguments__

  - __x__
    : Must be of default _real(kind=kind(0.0))_ type

  - __y__
    : Must have the same type and kind parameters as __x__

## __Returns__

The return value is of type _real(kind=kind(0.0d0))_.

## __Examples__

Sample program:

```fortran
program demo_dprod
use, intrinsic :: iso_fortran_env, only : real_kinds, &
& real32, real64, real128
implicit none
integer,parameter :: dp=kind(0.0d0)
real :: x = 5.2
real :: y = 2.3
real(kind=dp) :: dd
   dd = dprod(x,y)
   print *, dd, x*y, kind(x), kind(dd), kind(dprod(x,y))
   ! interesting comparisons
   print *, 52*23
   print *, 52*23/100.0
   print *, 52*23/100.0d0

   !! common extension is to take doubleprecision arguments
   !! and return higher precision
   bigger: block
   doubleprecision :: xx = 5.2d0
   doubleprecision :: yy = 2.3d0
   real(kind=real128) :: ddd
   !ddd = dprod(xx,yy)
   !print *, ddd, xx*yy, kind(xx), kind(ddd), kind(dprod(xx,yy))
   endblock bigger

end program demo_dprod
```

## __Standard__

FORTRAN 77 and later

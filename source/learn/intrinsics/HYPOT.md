---
layout: book
title: hypot
permalink: /learn/intrinsics/HYPOT
---
# HYPOT
## __Name__

__hypot__(3) - \[MATHEMATICS\] returns the distance between the point and the origin.

## __Syntax__
```fortran
result = hypot(x, y)

   real(kind=KIND) elemental function hypot(x,y) result(value)
   real(kind=KIND),intent(in) :: x, y
```
   where __x,y,value__ shall all be of the same __kind__.

## __Description__

__hypot(x,y)__ is referred to as the Euclidean distance function. It is equal to
__sqrt(x**2 + y**2)__, without undue underflow or overflow.

In mathematics, the _Euclidean distance_ between two points in Euclidean
space is the length of a line segment between two points.
 
__hypot(x,y)__ returns the distance between the point __<x,y>__ and the origin.

## __Arguments__

  - __x__
    : The type shall be _real_.

  - __y__
    : The type and kind type parameter shall be the same as __x__.

## __Returns__

The return value has the same type and kind type parameter as __x__.

The result is the positive magnitude of the distance of the point __<x,y>__ from the
origin __<0.0,0.0>__ .

## __Examples__

Sample program:

```fortran
program demo_hypot
use, intrinsic :: iso_fortran_env, only : &
 & real_kinds, real32, real64, real128
implicit none
real(kind=real32) :: x, y 
real(kind=real32),allocatable :: xs(:), ys(:)
integer :: i
character(len=*),parameter :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

   x = 1.e0_real32
   y = 0.5e0_real32

   write(*,*)
   write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
   write(*,'(*(g0))')'units away from the origin'
   write(*,*)

   ! elemental
   xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
   ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

   write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
   write(*,f)"have distances from the origin of ",hypot(xs,ys)
   write(*,f)"the closest is",minval(hypot(xs,ys))

end program demo_hypot
```
Results:
```text
   point <1.00000000,0.500000000> is 1.11803401
   units away from the origin
   
   the points
      +1.00000000 +0.500000000
      +1.00000000 +0.250000000
      +10.0000000 -10.0000000
      +15.0000000 +0.250000000
      -1.00000000 -0.250000000
   have distances from the origin of 
      +1.11803401 +1.03077638
      +14.1421356 +15.0020828
      +1.03077638
   the closest is
      +1.03077638
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (license: MIT) @urbanjost

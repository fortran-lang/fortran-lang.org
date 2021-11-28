---
layout: book
title: hypot
permalink: /learn/intrinsics/HYPOT
---
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

In mathematics, the Euclidean distance between two points in Euclidean
space is the length of a line segment between two points.
 
__hypot(x,y)__ returns the distance between the point __<x,y>__ and the origin.


## __Arguments__

  - __x__
    The type shall be _real_.

  - __y__
    The type and kind type parameter shall be the same as __x__.

## __Returns__

The positive magnitude of the distance of the point __<x,y>__ from the
origin __<0.0,0.0>__ is returned.

The return value has the same type and kind type parameter as __x__.

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
character(len=*),parameter :: f='(a,/,*(g0,1x,g0,/))'

   x = 1.e0_real64
   y = 0.5e0_real64

   write(*,*)
   write(*,*)'point',x,y,'is ',hypot(x,y),
   write(*,*)'units away from the origin'
   write(*,*)

   ! elemental
   xs=[  x**2,  x*10.0,  x*15.0,  x**2  ]
   ys=[  y**2,  y*20.0,  y**2,    y**2  ]

   write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
   write(*,f)"have distances from the origin of ",hypot(xs,ys)
   write(*,f)"the closest is",minval(hypot(xs,ys))

end program demo_hypot
```
## __Standard__

Fortran 2008 and later

###### fortran-lang intrinsic descriptions (@urbanjost)

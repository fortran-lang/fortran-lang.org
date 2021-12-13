---
layout: book
title: cos
permalink: /learn/intrinsics/COS
---
## __Name__

__cos__(3) - \[MATHEMATICS:TRIGONOMETRIC\] Cosine function

## __Syntax__
```fortran
result = cos(x)

   TYPE(kind=KIND),elemental :: cos
   TYPE(kind=KIND,intent(in) :: x
```
where TYPE may be _real_ or _complex_ and KIND may be any KIND supported
by the associated type.

## __Description__

__cos(x)__ computes the cosine of an angle __x__ given the size of the
angle in radians.

The cosine of a _real_ value is the ratio of the adjacent side to the
hypotenuse of a right-angled triangle.

## __Arguments__

  - __x__
    : The type shall be _real_ or _complex_.
    __x__ is assumed to be in radians.

## __Returns__

The return value is of the same type and kind as __x__. 

If __x__ is of the type _real_, the return value lies in
the range __-1 \<= cos(x) \<= 1__ .

## __Examples__

Sample program:

```fortran
program demo_cos
implicit none
doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
   write(*,*)'COS(0.0)=',cos(0.0)
   write(*,*)'COS(PI)=',cos(PI)
   write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
   write(*,*)'COS(2*PI)=',cos(2*PI)
   write(*,*)'COS(-2*PI)=',cos(-2*PI)
   write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
   write(*,*)'COS(3000*PI)=',cos(3000*PI)
end program demo_cos
```
Results:
```
   COS(0.0)=        1.00000000
   COS(PI)=        -1.0000000000000000
   COS(PI/2.0d0)=   6.1232339957367660E-017
   EPSILON=         2.2204460492503131E-016
   COS(2*PI)=       1.0000000000000000
   COS(-2*PI)=      1.0000000000000000
   COS(-2000*PI)=   1.0000000000000000
```
## __Standard__

FORTRAN 77 and later

## __See Also__
- [Wikipedia:sine and cosine](https://en.wikipedia.org/wiki/Sine_and_cosine)

[__acos__(3)](ACOS),
[__sin__(3)](SIN),
[__tan__(3)](TAN)

###### fortran-lang intrinsic descriptions

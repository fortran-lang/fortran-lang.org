---
layout: book
title: norm2
permalink: /learn/intrinsics/NORM2
---
## __Name__

__norm2__(3) - \[MATHEMATICS\] Euclidean vector norm
(GFDL)

## __Syntax__
```fortran
result = norm2(array, dim)

real function result norm2(array, dim)

   real,intent(in) :: array(..)
   integer,intent(in),optional :: dim
```
## __Description__

Calculates the Euclidean vector norm (L\_2 norm) of __array__ along
dimension __dim__.

## __Arguments__

  - __array__
    : Shall be an array of type _real_.

  - __dim__
    : shall be a scalar of type _integer_ with a value in the
    range from __1__ to  __rank(array)__.

## __Returns__

The result is of the same type as __array__.

If __dim__ is absent, a scalar with the square root of the sum of squares of
the elements of __array__ is returned. 

Otherwise, an array of rank __n-1__,
where __n__ equals the rank of __array__, and a shape similar to that of __array__
with dimension DIM dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_norm2
implicit none
integer :: i

real :: x(3,3) = reshape([ &
   1, 2, 3, &
   4, 5 ,6, &
   7, 8, 9  &
],shape(x),order=[2,1])

write(*,*) 'x='
write(*,'(4x,3f4.0)')transpose(x)

write(*,*) 'norm2(x)=',norm2(x)

write(*,*) 'x**2='
write(*,'(4x,3f4.0)')transpose(x**2)
write(*,*)'sqrt(sum(x**2))=',sqrt(sum(x**2))

end program demo_norm2
```
Results:
```text
 x=
      1.  2.  3.
      4.  5.  6.
      7.  8.  9.
 norm2(x)=   16.88194    
 x**2=
      1.  4.  9.
     16. 25. 36.
     49. 64. 81.
 sqrt(sum(x**2))=   16.88194    
```
## __Standard__

Fortran 2008 and later

## __See Also__

[__product__(3)](PRODUCT),
[__sum__(3)](SUM),
[__hypot__(3)](HYPOT)

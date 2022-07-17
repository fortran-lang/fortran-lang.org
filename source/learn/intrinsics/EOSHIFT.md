---
layout: book
title: eoshift
permalink: /learn/intrinsics/EOSHIFT
---
# EOSHIFT
## __Name__

__eoshift__(3) - \[TRANSFORMATIONAL\] End-off shift elements of an array


## __Syntax__
```fortran
result = eoshift(array, shift, boundary, dim)
```
## __Description__

__eoshift(array, shift\[, boundary, dim\])__ performs an end-off shift
on elements of __array__ along the dimension of __dim__. If __dim__ is omitted it is
taken to be __1__. __dim__ is a scalar of type _integer_ in the range of __1 \<= DIM
\<= n__ where __"n"__ is the rank of __array__. If the rank of __array__ is one, then
all elements of __array__ are shifted by __shift__ places. If rank is greater
than one, then all complete rank one sections of __array__ along the given
dimension are shifted. Elements shifted out one end of each rank one
section are dropped. If __boundary__ is present then the corresponding value
of from __boundary__ is copied back in the other end. If __boundary__ is not
present then the following are copied in depending on the type of __array__.

\*Array Type\* - \*Boundary Value\*

   - Numeric 0 of the type and kind of __array__

   - Logical .false.

   - __Character(len)__ LEN blanks

## __Arguments__

  - __array__
    : May be any type, not scalar.

  - __shift__
    : The type shall be _integer_.

  - __boundary__
    : Same type as ARRAY.

  - __dim__
    : The type shall be _integer_.

## __Returns__

Returns an array of same type and rank as the __array__ argument.

## __Examples__

Sample program:

```fortran
program demo_eoshift
implicit none
    integer, dimension(3,3) :: a
    a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
    a = eoshift(a, SHIFT=[1, 2, 1], BOUNDARY=-5, DIM=2)
    print *
    print '(3i3)', a(1,:)
    print '(3i3)', a(2,:)
    print '(3i3)', a(3,:)
end program demo_eoshift
```
  Results:
```text
     1  4  7
     2  5  8
     3  6  9
   
     4  7 -5
     8 -5 -5
     6  9 -5
```
## __Standard__

Fortran 95 and later

###### fortran-lang intrinsic descriptions

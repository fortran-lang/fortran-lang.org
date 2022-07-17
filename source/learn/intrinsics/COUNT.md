---
layout: book
title: count
permalink: /learn/intrinsics/COUNT
---
# COUNT
## __Name__

__count__(3) - \[ARRAY REDUCTION\] Count function


## __Syntax__
```fortran
result = count(mask, dim, kind)
```
## __Description__

Counts the number of __.true.__ elements in a logical __mask__, or, if the __dim__
argument is supplied, counts the number of elements along each row of
the array in the __dim__ direction. If the array has zero size, or all of
the elements of __mask__ are false, then the result is __0__.

## __Arguments__

  - __mask__
    : The type shall be _logical_.

  - __dim__
    : (Optional) The type shall be _integer_.

  - __kind__
    : (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind __kind__. If __kind__ is absent,
the return value is of default integer kind. If __dim__ is present, the
result is an array with a rank one less than the rank of __array__, and a
size corresponding to the shape of __array__ with the __dim__ dimension removed.

## __Examples__

Sample program:

```fortran
program demo_count
implicit none
integer, dimension(2,3) :: a, b
logical, dimension(2,3) :: mymask
      a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
      b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
      print '(3i3)', a(1,:)
      print '(3i3)', a(2,:)
      print *
      print '(3i3)', b(1,:)
      print '(3i3)', b(2,:)
      print *
      mymask = a.ne.b
      print '(3l3)', mymask(1,:)
      print '(3l3)', mymask(2,:)
      print *
      print '(3i3)', count(mymask)
      print *
      print '(3i3)', count(mymask, 1)
      print *
      print '(3i3)', count(mymask, 2)
end program demo_count
```
   Expected Results:
```text
  1  3  5
  2  4  6
 
  0  3  5
  7  4  8
 
  T  F  F
  T  F  T
 
  3
 
  2  0  1
 
  1  2
```
## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003
and later

###### fortran-lang intrinsic descriptions

---
layout: book
title: count
permalink: /learn/intrinsics/COUNT
---
### NAME

__count__(3f) - \[ARRAY REDUCTION\] Count function
(GFDL)

### SYNTAX

result = __count__(mask \[, dim, kind\])

### DESCRIPTION

Counts the number of .true. elements in a logical MASK, or, if the DIM
argument is supplied, counts the number of elements along each row of
the array in the DIM direction. If the array has zero size, or all of
the elements of MASK are false, then the result is 0.

### ARGUMENTS

  - __MASK__
    The type shall be LOGICAL.

  - __DIM__
    (Optional) The type shall be INTEGER.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind. If DIM is present, the
result is an array with a rank one less than the rank of ARRAY, and a
size corresponding to the shape of ARRAY with the DIM dimension removed.

### EXAMPLE

Sample program:

```
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

   Expected Results:

     > 1  3  5
     > 2  4  6
     >
     > 0  3  5
     > 7  4  8
     >
     > T  F  F
     > T  F  T
     >
     > 3
     >
     > 2  0  1
     >
     > 1  2
```

### STANDARD

Fortran 95 and later, with KIND argument - Fortran 2003
and later

### CLASS

Transformational function

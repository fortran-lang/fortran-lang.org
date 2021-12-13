---
layout: book
title: parity
permalink: /learn/intrinsics/PARITY
---
## __Name__

__parity__(3) - \[TRANSFORMATIONAL\] Reduction with exclusive __OR__()
(GFDL)

## __Syntax__
```fortran
result = parity(mask, dim)
```
## __Description__

Calculates the parity (i.e. the reduction using .xor.) of __mask__ along
dimension __dim__.

## __Arguments__

  - __mask__
    : Shall be an array of type _logical_.

  - __dim__
    : (Optional) shall be a scalar of type _integer_ with a value in the
    range from __1 to n__, where __n__ equals the rank of __array__.

## __Returns__

The result is of the same type as __mask__.

If __dim__ is absent, a scalar with the parity of all elements in __mask__ is
returned: __.true.__ if an odd number of elements are __.true.__ and

where __n__ equals the rank of __mask__, and a shape similar to that of __mask__
with dimension __dim__ dropped is returned.

## __Examples__

Sample program:

```fortran
program demo_parity
implicit none
logical :: x(2) = [ .true., .false. ]
   print *, parity(x) ! T
end program demo_parity
```
  Results:
```text
    T
```
## __Standard__

Fortran 2008 and later

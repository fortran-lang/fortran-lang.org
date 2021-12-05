---
layout: book
title: rank
permalink: /learn/intrinsics/RANK
---
## __Name__

__rank__(3) - \[ARRAY INQUIRY\] Rank of a data object
(GFDL)

## __Syntax__

result = __rank__(a)

## __Description__

__rank(a)__ returns the rank of a scalar or array data object.

## __Arguments__

  - __a__
    : can be of any type

## __Returns__

The return value is of type _integer_ and of the default integer kind. For
arrays, their rank is returned; for scalars zero is returned.

## __Examples__

Sample program:

```fortran
program demo_rank
implicit none
integer :: a
real, allocatable :: b(:,:)
real  :: c(10,20,30)
   print *, rank(a), rank(b), rank(c)
end program demo_rank
```
Expected output:
```text
   0           2           3
```
## __Standard__

TS 29113

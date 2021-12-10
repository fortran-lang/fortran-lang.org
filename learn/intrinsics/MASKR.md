---
layout: book
title: maskr
permalink: /learn/intrinsics/MASKR
---
## __Name__

__maskr__(3) - \[\] Right justified mask
(GFDL)

## __Syntax__
```fortran
result = maskr(i, kind)
```
## __Description__

__maskr(i\[, kind\])__ has its rightmost __i__ bits set to 1, and the
remaining bits set to 0.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __kind__
    : Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If __kind__ is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__maskl__(3)](MASKL)

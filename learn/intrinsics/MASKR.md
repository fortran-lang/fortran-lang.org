---
layout: book
title: maskr
permalink: /learn/intrinsics/MASKR
---
## __Name__

__maskr__(3) - \[\] Right justified mask
(GFDL)

## __Syntax__

result = __maskr(i\[, kind\])__

## __Description__

__maskr(i\[, kind\])__ has its rightmost I bits set to 1, and the
remaining bits set to 0.

## __Arguments__

  - __I__
    Shall be of type _integer_.

  - __KIND__
    Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If KIND is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

## __Standard__

Fortran 2008 and later

## __See Also__

__maskl__(3)

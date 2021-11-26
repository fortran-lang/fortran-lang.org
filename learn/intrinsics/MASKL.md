---
layout: book
title: maskl
permalink: /learn/intrinsics/MASKL
---
## __Name__

__maskl__(3) - \[\] Left justified mask
(GFDL)

## __Synopsis__

result = __maskl__(i\[, *kind*\])

## __Description__

__maskl__(i\[, *kind*\]) has its leftmost I bits set to 1, and the
remaining bits set to 0.

## __Syntax__

## __Arguments__

  - __I__
    Shall be of type _integer_.

  - __KIND__
    Shall be a scalar constant expression of type _integer_.

## __Returns__

The return value is of type _integer_. If KIND is present, it specifies
the *kind* value of the return type; otherwise, it is of the default
integer *kind*.

## __Standard__

Fortran 2008 and later

## __See Also__

__maskr__(3)

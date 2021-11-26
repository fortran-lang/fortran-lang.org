---
layout: book
title: rrspacing
permalink: /learn/intrinsics/RRSPACING
---
## __Name__

__rrspacing__(3) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing
(GFDL)

## __Syntax__

result = __rrspacing__(x)

## __Description__

__rrspacing__(x) returns the reciprocal of the relative spacing of model
numbers near X.

## __Arguments__

  - __X__
    Shall be of type _real_.

## __Returns__

The return value is of the same type and kind as X. The value returned
is equal to __abs__(fraction(x)) \* __float__(radix(x))\*\*digits(x).

## __Standard__

Fortran 95 and later

## __See Also__

__spacing__(3)

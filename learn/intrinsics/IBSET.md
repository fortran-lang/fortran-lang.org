---
layout: book
title: ibset
permalink: /learn/intrinsics/IBSET
---
## __Name__

__ibset__(3) - \[BIT MANIPULATION\] Set bit
(GFDL)

## __Syntax__

result = __ibset__(i, pos)

## __Description__

IBSET returns the value of I with the bit at position POS set to one.

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __POS__
    The type shall be _integer_. A value of zero refers to the least
    significant bit. pos is an __INTENT__(IN) scalar or array of type
    _integer_. The value of pos must be within the range zero to
    (BIT\_SIZE(i)__-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as I.

## __Standard__

Fortran 95 and later

## __See Also__

__btest__(3), __ibclr__(3), __ibits__(3), __iand__(3), __ior__(3),
__ieor__(3), __mvbits__(3)

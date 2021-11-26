---
layout: book
title: ibclr
permalink: /learn/intrinsics/IBCLR
---
## __Name__

__ibclr__(3) - \[BIT MANIPULATION\] Clear bit
(GFDL)

## __Syntax__

result = __ibclr__(i, pos)

## __Description__

IBCLR returns the value of I with the bit at position POS set to zero.

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __POS__
    The type shall be _integer_. A value of zero refers to the least
    significant bit. POS is an __INTENT__(IN) scalar or array of type
    _integer_. The value of POS must be within the range zero to
    (BIT\_SIZE(i)__-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as I.

## __Standard__

Fortran 95 and later

## __See Also__

__ibits__(3), __ibset__(3), __iand__(3), __ior__(3), __ieor__(3),
__mvbits__(3)

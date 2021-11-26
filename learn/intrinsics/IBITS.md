---
layout: book
title: ibits
permalink: /learn/intrinsics/IBITS
---
## __Name__

__ibits__(3) - \[BIT MANIPULATION\] Bit extraction
(GFDL)

## __Syntax__

result = __ibits__(i, pos, len)

## __Description__

IBITS extracts a field of length LEN from I, starting from bit position
POS and extending left for LEN bits. The result is right-justified and
the remaining bits are zeroed. The value of pos+len must be less than or
equal to the value __bit\_size__(i).

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __POS__
    The type shall be _integer_. A value of zero refers to the least
    significant bit.

  - __LEN__
    The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as I.

## __Standard__

Fortran 95 and later

## __See Also__

__bit\_size__(3), __ibclr__(3), __ibset__(3), __iand__(3), __ior__(3),
__ieor__(3)

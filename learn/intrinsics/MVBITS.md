---
layout: book
title: mvbits
permalink: /learn/intrinsics/MVBITS
---
## __Name__

__mvbits__(3) - \[BIT MANIPULATION\] Move bits from one integer to another
(GFDL)

## __Syntax__

call __mvbits__(from, frompos, len, to, topos)

## __Description__

Moves LEN bits from positions FROMPOS through frompos+len-1 of FROM to
positions TOPOS through topos+len-1 of TO. The portion of argument TO
not affected by the movement of bits is unchanged. The values of
frompos+len-1 and topos+len-1 must be less than __bit\_size__(from).

## __Arguments__

  - __FROM__
    The type shall be _integer_.

  - __FROMPOS__
    The type shall be _integer_.

  - __LEN__
    The type shall be _integer_.

  - __TO__
    The type shall be _integer_, of the same kind as FROM.

  - __TOPOS__
    The type shall be _integer_.

## __Standard__

Fortran 95 and later

## __See Also__

__ibclr__(3), __ibset__(3), __ibits__(3), __iand__(3), __ior__(3)

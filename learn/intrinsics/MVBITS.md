---
layout: book
title: mvbits
permalink: /learn/intrinsics/MVBITS
---
## __Name__

__mvbits__(3) - \[BIT MANIPULATION\] Move bits from one integer to another
(GFDL)

## __Syntax__

call __mvbits(from, frompos, len, to, topos)__

## __Description__

Moves __LEN__ bits from positions __FROMPOS__ through __FROMPOS+LEN-1__ of __FROM__ to
positions __TOPOS__ through __TOPOS+LEN-1__ of __TO__. The portion of argument __TO__
not affected by the movement of bits is unchanged. The values of
__FROMPOS+LEN-1__ and __TOPOS+LEN-1__ must be less than __bit\_size__(from).

## __Arguments__

  - __FROM__
    The type shall be _integer_.

  - __FROMPOS__
    The type shall be _integer_.

  - __LEN__
    The type shall be _integer_.

  - __TO__
    The type shall be _integer_, of the same kind as __FROM__.

  - __TOPOS__
    The type shall be _integer_.

## __Standard__

Fortran 95 and later

## __See Also__

__ibclr__(3), __ibset__(3), __ibits__(3), __iand__(3), __ior__(3)

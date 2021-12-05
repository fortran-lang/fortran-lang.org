---
layout: book
title: ishft
permalink: /learn/intrinsics/ISHFT
---
## __Name__

__ishft__(3) - \[BIT MANIPULATION\] Shift bits
(GFDL)

## __Syntax__

result = __ishft__(i, shift)

## __Description__

ISHFT returns a value corresponding to I with all of the bits shifted
SHIFT places. A value of SHIFT greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of SHIFT is
greater than __bit\_size__(i), the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

## __Arguments__

  - __I__
    : The type shall be _integer_.

  - __SHIFT__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as I.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishftc__(3)](ISHFTC)

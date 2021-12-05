---
layout: book
title: shiftl
permalink: /learn/intrinsics/SHIFTL
---
## __Name__

__shiftl__(3) - \[BIT MANIPULATION\] shift bits left
(GFDL)

## __Syntax__

result = __SHIFTL__(I, SHIFT)

## __Description__

Returns a value corresponding to I with all of the bits shifted left by
SHIFT places. If the absolute value of SHIFT is greater than
__BIT\_SIZE__(I), the value is undefined. Bits shifted out from the left
end are lost, and bits shifted in from the right end are set to 0.

## __Arguments__

  - __I__
    : The type shall be _integer_.

  - __SHIFT__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as I.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shifta__(3)](SHIFTA),
[__shiftr__(3)](SHIFTR)

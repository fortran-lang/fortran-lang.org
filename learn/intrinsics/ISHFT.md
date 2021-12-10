---
layout: book
title: ishft
permalink: /learn/intrinsics/ISHFT
---
## __Name__

__ishft__(3) - \[BIT MANIPULATION\] Shift bits
(GFDL)

## __Syntax__
```fortran
result = ishft(i, shift)
```
## __Description__

__ishft__(3) returns a value corresponding to __i__ with all of the bits shifted
__shift__ places. A value of __shift__ greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of __shift__ is
greater than __bit\_size(i)__, the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishftc__(3)](ISHFTC)

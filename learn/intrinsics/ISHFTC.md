---
layout: book
title: ishftc
permalink: /learn/intrinsics/ISHFTC
---
## __Name__

__ishftc__(3) - \[BIT MANIPULATION\] Shift bits circularly
(GFDL)

## __Syntax__

result = __ishftc__(i, shift \[, size\])

## __Description__

ISHFTC returns a value corresponding to I with the rightmost SIZE bits
shifted circularly SHIFT places; that is, bits shifted out one end are
shifted into the opposite end. A value of SHIFT greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of SHIFT must be less than SIZE. If the SIZE argument is omitted,
it is taken to be equivalent to __bit\_size__(i).

## __Arguments__

  - __I__
    : The type shall be _integer_.

  - __SHIFT__
    : The type shall be _integer_.

  - __SIZE__
    : (Optional) The type shall be _integer_; the value must be greater than
    zero and less than or equal to __bit\_size__(i).

## __Returns__

The return value is of type _integer_ and of the same kind as I.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishft__(3)](ISHFT)

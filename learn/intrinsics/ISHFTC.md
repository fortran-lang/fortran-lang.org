---
layout: book
title: ishftc
permalink: /learn/intrinsics/ISHFTC
---
## __Name__

__ishftc__(3) - \[BIT:MANIPULATION\] Shift bits circularly
(GFDL)

## __Syntax__
```fortran
result = ishftc(i, shift, size)
```
## __Description__

__ishftc__(3) returns a value corresponding to __i__ with the rightmost __size__ bits
shifted circularly __shift__ places; that is, bits shifted out one end are
shifted into the opposite end. A value of __shift__ greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of __shift__ must be less than __size__. If the __size__ argument is omitted,
it is taken to be equivalent to __bit\_size(i)__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

  - __size__
    : (Optional) The type shall be _integer_; the value must be greater than
    zero and less than or equal to __bit\_size__(i).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ishft__(3)](ISHFT)

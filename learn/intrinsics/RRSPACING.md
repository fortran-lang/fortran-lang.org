---
layout: book
title: rrspacing
permalink: /learn/intrinsics/RRSPACING
---
## __Name__

__rrspacing__(3) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing
(GFDL)

## __Syntax__
```fortran
result = rrspacing(x)
```
## __Description__

__rrspacing(x)__ returns the reciprocal of the relative spacing of model
numbers near __x__.

## __Arguments__

  - __x__
    : Shall be of type _real_.

## __Returns__

The return value is of the same type and kind as __x__. The value returned
is equal to __abs__(fraction(x)) \* __float__(radix(x))\*\*digits(x).

## __Standard__

Fortran 95 and later

## __See Also__

[digits(3)](DIGITS),
[epsilon(3)](EPSILON),
[exponent(3)](EXPONENT),
[fraction(3)](FRACTION),
[huge(3)](HUGE),
[maxexponent(3)](MAXEXPONENT),
[minexponent(3)](MINEXPONENT),
[nearest(3)](NEAREST),
[precision(3)](PRECISION),
[radix(3)](RADIX),
[range(3)](RANGE),
[scale(3)](SCALE),
[set_exponent(3)](SET_EXPONENT),
[spacing(3)](SPACING),
[tiny(3)](TINY)


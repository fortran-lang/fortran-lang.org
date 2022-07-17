---
layout: book
title: shifta
permalink: /learn/intrinsics/SHIFTA
---
# SHIFTA
## __Name__

__shifta__(3) - \[BIT:SHIFT\] shift bits right with fill


## __Syntax__
```fortran
result = shifta(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted right by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the
right end are lost. The fill is arithmetic: the bits shifted in from the
left end are equal to the leftmost bit, which in two's complement
representation is the sign bit.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __shift__
    : The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__shiftl__(3)](SHIFTL),
[__shiftr__(3)](SHIFTR)

###### fortran-lang intrinsic descriptions

---
layout: book
title: shiftl
permalink: /learn/intrinsics/SHIFTL
---
## __Name__

__shiftl__(3) - \[BIT:SHIFT\] shift bits left
(GFDL)

## __Syntax__
```fortran
result = shiftl(i, shift)
```
## __Description__

Returns a value corresponding to __i__ with all of the bits shifted left by
__shift__ places. If the absolute value of __shift__ is greater than
__bit\_size(i)__, the value is undefined. Bits shifted out from the left
end are lost, and bits shifted in from the right end are set to __0__.

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

[__shifta__(3)](SHIFTA),
[__shiftr__(3)](SHIFTR)

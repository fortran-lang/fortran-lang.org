---
layout: book
title: dshiftl
permalink: /learn/intrinsics/DSHIFTL
---
## __Name__

__dshiftl__(3) - \[BIT:MANIPULATION\] combines bits of arguments I and J
(GFDL)

## __Syntax__
```fortran
result = dshiftl(i, j, shift)
```
## __Description__

__dshiftl(i, j, shift)__ combines bits of __i__ and __j__. The rightmost __shift__
bits of the result are the leftmost __shift__ bits of __j__, and the remaining
bits are the rightmost bits of __i__.

## __Arguments__

  - __i__
    : Shall be of type _integer_.

  - __j__
    : Shall be of type _integer_, and of the same kind as __i__.

  - __shift__
    : Shall be of type _integer_.

## __Returns__

The return value has same type and kind as __i__.

## __Standard__

Fortran 2008 and later

## __See Also__

[__dshiftr__(3)](DSHIFTR)

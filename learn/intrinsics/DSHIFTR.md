---
layout: book
title: dshiftr
permalink: /learn/intrinsics/DSHIFTR
---
## __Name__

__dshiftr__(3) - \[BIT MANIPULATION\] combines bits of arguments __i__ and __j__
(GFDL)

## __Syntax__

result = __dshiftl__(i, j, shift)

## __Description__

__dshiftr(i, j, shift)__ combines bits of __i__ and __j__. The leftmost __shift__
bits of the result are the rightmost __shift__ bits of __i__, and the remaining
bits are the leftmost bits of __j__.

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

[__dshiftl__(3)](DSHIFTL)

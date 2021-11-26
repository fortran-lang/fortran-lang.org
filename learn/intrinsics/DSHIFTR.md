---
layout: book
title: dshiftr
permalink: /learn/intrinsics/DSHIFTR
---
## __Name__

__dshiftr__(3) - \[BIT MANIPULATION\] combines bits of arguments I and J
(GFDL)

## __Syntax__

result = __DSHIFTL__(I, J, SHIFT)

## __Description__

__DSHIFTR__(I, J, SHIFT) combines bits of I and J. The leftmost SHIFT
bits of the result are the rightmost SHIFT bits of I, and the remaining
bits are the leftmost bits of J.

## __Arguments__

  - __I__
    Shall be of type _integer_.

  - __J__
    Shall be of type _integer_, and of the same kind as I.

  - __SHIFT__
    Shall be of type _integer_.

## __Returns__

The return value has same type and kind as I.

## __Standard__

Fortran 2008 and later

## __See Also__

__dshiftl__(3)

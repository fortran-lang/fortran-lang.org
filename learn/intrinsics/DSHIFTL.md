---
layout: book
title: dshiftl
permalink: /learn/intrinsics/DSHIFTL
---
## __Name__

__dshiftl__(3) - \[BIT MANIPULATION\] combines bits of arguments I and J
(GFDL)

## __Syntax__

result = __DSHIFTL__(I, J, SHIFT)

## __Description__

__DSHIFTL__(I, J, SHIFT) combines bits of I and J. The rightmost SHIFT
bits of the result are the leftmost SHIFT bits of J, and the remaining
bits are the rightmost bits of I.

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

__dshiftr__(3)

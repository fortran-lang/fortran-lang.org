---
layout: book
title: bgt
permalink: /learn/intrinsics/BGT
---
## __Name__

__bgt__(3) - \[BIT COMPARE\] Bitwise greater than
(GFDL)

## __Syntax__

result = __bgt__(i, j)

## __Description__

Determines whether an integer is bitwise greater than another.

## __Arguments__

  - __I__
    Shall be of _integer_ type or a BOZ literal constant.

  - __J__
    Shall be of _integer_ type, and of the same kind as I; or a BOZ
    literal constant.

## __Returns__

The return value is of type _logical_ and of the default kind. The result
is true if the sequence of bits represented by I is greater than the
sequence of bits represented by J, otherwise the result is false.

## __Standard__

Fortran 2008 and later

## __See Also__

__bge__(3), __ble__(3), __blt__(3)

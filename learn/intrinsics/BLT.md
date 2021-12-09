---
layout: book
title: blt
permalink: /learn/intrinsics/BLT
---
## __Name__

__blt__(3) - \[BIT COMPARE\] Bitwise less than
(GFDL)

## __Syntax__
```fortran
    result = blt(i, j)
```
## __Description__

Determines whether an integer is bitwise less than another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__.

## __Returns__

The return value is of type _logical_ and of the default kind.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3)](BGE),
[__bgt__(3)](BGT),
[__ble__(3)](BLE)

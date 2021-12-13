---
layout: book
title: bgt
permalink: /learn/intrinsics/BGT
---
## __Name__

__bgt__(3) - \[BIT:COMPARE\] Bitwise greater than
(GFDL)

## __Syntax__
```fortran
    result = bgt(i, j)
```
## __Description__

Determines whether an integer is bitwise greater than another.

## __Arguments__

  - __i__
    : Shall be of _integer_ type or a BOZ literal constant.

  - __j__
    : Shall be of _integer_ type, and of the same kind as __i__; or a BOZ
    literal constant.

## __Returns__

The return value is of type _logical_ and of the default kind. The result
is true if the sequence of bits represented by _i_ is greater than the
sequence of bits represented by _j_, otherwise the result is false.

## __Standard__

Fortran 2008 and later

## __See Also__

[__bge__(3),](BGE),
[__ble__(3),](BLE),
[__blt__(3)](BLT)

###### fortran-lang intrinsic descriptions

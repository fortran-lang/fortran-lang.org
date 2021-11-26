---
layout: book
title: ieor
permalink: /learn/intrinsics/IEOR
---
## __Name__

__ieor__(3) - \[BIT MANIPULATION\] Bitwise logical exclusive or
(GFDL)

## __Syntax__

result = __ieor__(i, j)

## __Description__

IEOR returns the bitwise Boolean exclusive-OR of I and J.

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __J__
    The type shall be _integer_, of the same kind as I.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Standard__

Fortran 95 and later

## __See Also__

__ior__(3), __iand__(3), __ibits__(3), __ibset__(3), __ibclr__(3),
__not__(3)

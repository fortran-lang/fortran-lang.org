---
layout: book
title: ieor
permalink: /learn/intrinsics/IEOR
---
## __Name__

__ieor__(3) - \[BIT MANIPULATION\] Bitwise logical exclusive or
(GFDL)

## __Syntax__

__result = ieor(i, j)__

## __Description__

__ieor__ returns the bitwise Boolean exclusive-__or__ of __i__ and __j__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __j__
    : The type shall be _integer_, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__mvbits__(3)](MVBITS)

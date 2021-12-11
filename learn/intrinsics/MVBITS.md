---
layout: book
title: mvbits
permalink: /learn/intrinsics/MVBITS
---
## __Name__

__mvbits__(3) - \[BIT:MANIPULATION\] Move bits from one integer to another
(GFDL)

## __Syntax__
```fortran
call mvbits(from, frompos, len, to, topos)
```
## __Description__

Moves __len__ bits from positions __frompos__ through __frompos+len-1__ of __from__ to
positions __topos__ through __topos+len-1__ of __to__. The portion of argument __to__
not affected by the movement of bits is unchanged. The values of
__frompos+len-1__ and __topos+len-1__ must be less than __bit\_size__(from).

## __Arguments__

  - __from__
    : The type shall be _integer_.

  - __frompos__
    : The type shall be _integer_.

  - __len__
    : The type shall be _integer_.

  - __to__
    : The type shall be _integer_, of the same kind as __from__.

  - __topos__
    : The type shall be _integer_.

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
[__ieor__(3)](IEOR)

---
layout: book
title: ibclr
permalink: /learn/intrinsics/IBCLR
---
## __Name__

__ibclr__(3) - \[BIT:MANIPULATION\] Clear bit
(GFDL)

## __Syntax__
```fortran
result = ibclr(i, pos)
```
## __Description__

__ibclr__ returns the value of __i__ with the bit at position __pos__ set to zero.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __pos__
    : The type shall be _integer_. A value of zero refers to the least
    significant bit. __pos__ is an __intent(in)__ scalar or array of type
    _integer_. The value of __pos__ must be within the range zero to
    __(bit\_size(i)-1__).

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

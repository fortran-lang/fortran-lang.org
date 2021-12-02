---
layout: book
title: ibits
permalink: /learn/intrinsics/IBITS
---
## __Name__

__ibits__(3) - \[BIT MANIPULATION\] Bit extraction
(GFDL)

## __Syntax__

__result = ibits(i, pos, len)__

## __Description__

__ibits__ extracts a field of length __len__ from __i__, starting from bit position
__pos__ and extending left for __len__ bits. The result is right-justified and
the remaining bits are zeroed. The value of pos+len must be less than or
equal to the value __bit\_size(i)__.

## __Arguments__

  - __i__
    The type shall be _integer_.

  - __pos__
    The type shall be _integer_. A value of zero refers to the least
    significant bit.

  - __len__
    The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)
layout: book
title: ibits
permalink: /learn/intrinsics/IBITS
---
## __Name__

__ibits__(3) - \[BIT MANIPULATION\] Bit extraction
(GFDL)

## __Syntax__

__result = ibits(i, pos, len)__

## __Description__

__ibits__ extracts a field of length __len__ from __i__, starting from bit position
__pos__ and extending left for __len__ bits. The result is right-justified and
the remaining bits are zeroed. The value of pos+len must be less than or
equal to the value __bit\_size(i)__.

## __Arguments__

  - __i__
    The type shall be _integer_.

  - __pos__
    The type shall be _integer_. A value of zero refers to the least
    significant bit.

  - __len__
    The type shall be _integer_.

## __Returns__

The return value is of type _integer_ and of the same kind as __i__.

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

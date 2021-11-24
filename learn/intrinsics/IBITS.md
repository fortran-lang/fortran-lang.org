---
layout: book
title: ibits
permalink: /learn/intrinsics/IBITS
---
#### NAME

__ibits__(3f) - \[BIT MANIPULATION\] Bit extraction
(GFDL)

#### SYNTAX

result = __ibits__(i, pos, len)

#### DESCRIPTION

IBITS extracts a field of length LEN from I, starting from bit position
POS and extending left for LEN bits. The result is right-justified and
the remaining bits are zeroed. The value of pos+len must be less than or
equal to the value __bit\_size__(i).

#### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __POS__
    The type shall be INTEGER. A value of zero refers to the least
    significant bit.

  - __LEN__
    The type shall be INTEGER.

#### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__bit\_size__(3), __ibclr__(3), __ibset__(3), __iand__(3), __ior__(3),
__ieor__(3)

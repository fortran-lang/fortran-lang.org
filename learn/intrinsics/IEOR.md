---
layout: book
title: ieor
permalink: /learn/intrinsics/IEOR
---
#### NAME

__ieor__(3f) - \[BIT MANIPULATION\] Bitwise logical exclusive or
(GFDL)

#### SYNTAX

result = __ieor__(i, j)

#### DESCRIPTION

IEOR returns the bitwise Boolean exclusive-OR of I and J.

#### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __J__
    The type shall be INTEGER, of the same kind as I.

#### RETURN VALUE

The return type is INTEGER, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__ior__(3), __iand__(3), __ibits__(3), __ibset__(3), __ibclr__(3),
__not__(3)

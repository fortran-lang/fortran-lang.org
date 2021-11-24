---
layout: book
title: ishft
permalink: /learn/intrinsics/ISHFT
---
#### NAME

__ishft__(3f) - \[BIT MANIPULATION\] Shift bits
(GFDL)

#### SYNTAX

result = __ishft__(i, shift)

#### DESCRIPTION

ISHFT returns a value corresponding to I with all of the bits shifted
SHIFT places. A value of SHIFT greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of SHIFT is
greater than __bit\_size__(i), the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

#### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __SHIFT__
    The type shall be INTEGER.

#### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__ishftc__(3)

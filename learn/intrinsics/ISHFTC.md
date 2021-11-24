---
layout: book
title: ishftc
permalink: /learn/intrinsics/ISHFTC
---
#### NAME

__ishftc__(3f) - \[BIT MANIPULATION\] Shift bits circularly
(GFDL)

#### SYNTAX

result = __ishftc__(i, shift \[, size\])

#### DESCRIPTION

ISHFTC returns a value corresponding to I with the rightmost SIZE bits
shifted circularly SHIFT places; that is, bits shifted out one end are
shifted into the opposite end. A value of SHIFT greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of SHIFT must be less than SIZE. If the SIZE argument is omitted,
it is taken to be equivalent to __bit\_size__(i).

#### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __SHIFT__
    The type shall be INTEGER.

  - __SIZE__
    (Optional) The type shall be INTEGER; the value must be greater than
    zero and less than or equal to __bit\_size__(i).

#### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__ishft__(3)

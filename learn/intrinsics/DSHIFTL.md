---
layout: book
title: dshiftl
permalink: /learn/intrinsics/DSHIFTL
---
#### NAME

__dshiftl__(3f) - \[BIT MANIPULATION\] combines bits of arguments I and J
(GFDL)

#### SYNTAX

result = __DSHIFTL__(I, J, SHIFT)

#### DESCRIPTION

__DSHIFTL__(I, J, SHIFT) combines bits of I and J. The rightmost SHIFT
bits of the result are the leftmost SHIFT bits of J, and the remaining
bits are the rightmost bits of I.

#### ARGUMENTS

  - __I__
    Shall be of type INTEGER.

  - __J__
    Shall be of type INTEGER, and of the same kind as I.

  - __SHIFT__
    Shall be of type INTEGER.

#### RETURN VALUE

The return value has same type and kind as I.

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental function

#### SEE ALSO

__dshiftr__(3)

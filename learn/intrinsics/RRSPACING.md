---
layout: book
title: rrspacing
permalink: /learn/intrinsics/RRSPACING
---
#### NAME

__rrspacing__(3f) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing
(GFDL)

#### SYNTAX

result = __rrspacing__(x)

#### DESCRIPTION

__rrspacing__(x) returns the reciprocal of the relative spacing of model
numbers near X.

#### ARGUMENTS

  - __X__
    Shall be of type REAL.

#### RETURN VALUE

The return value is of the same type and kind as X. The value returned
is equal to __abs__(fraction(x)) \* __float__(radix(x))\*\*digits(x).

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__spacing__(3)

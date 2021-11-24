---
layout: book
title: blt
permalink: /learn/intrinsics/BLT
---
#### NAME

__blt__(3f) - \[BIT COMPARE\] Bitwise less than
(GFDL)

#### SYNTAX

result = __blt__(i, j)

#### DESCRIPTION

Determines whether an integer is bitwise less than another.

#### ARGUMENTS

  - __I__
    Shall be of INTEGER type.

  - __J__
    Shall be of INTEGER type, and of the same kind as I.

#### RETURN VALUE

The return value is of type LOGICAL and of the default kind.

#### STANDARD

Fortran 2008 and later

#### CLASS

Elemental function

#### SEE ALSO

__bge__(3), __bgt__(3), __ble__(3)

---
layout: book
title: ble
permalink: /learn/intrinsics/BLE
---
#### NAME

__ble__(3f) - \[BIT COMPARE\] Bitwise less than or equal to
(GFDL)

#### DESCRIPTION

Determines whether an integer is bitwise less than or equal to another.

#### SYNTAX

result = __ble__(i, j)

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

__bge__(3), __bgt__(3), __blt__(3)

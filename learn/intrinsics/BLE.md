---
layout: book
title: ble
permalink: /learn/intrinsics/BLE
---
### NAME

**ble**(3f) - \[BIT COMPARE\] Bitwise less than or equal to
(GFDL)

### DESCRIPTION

Determines whether an integer is bitwise less than or equal to another.

### SYNTAX

result = **ble**(i, j)

### ARGUMENTS

  - **I**
    Shall be of INTEGER type.

  - **J**
    Shall be of INTEGER type, and of the same kind as I.

### RETURN VALUE

The return value is of type LOGICAL and of the default kind.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

**bge**(3), **bgt**(3), **blt**(3)

---
layout: book
title: blt
permalink: /learn/intrinsics/BLT
---
### NAME

**blt**(3f) - \[BIT COMPARE\] Bitwise less than
(GFDL)

### SYNTAX

result = **blt**(i, j)

### DESCRIPTION

Determines whether an integer is bitwise less than another.

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

**bge**(3), **bgt**(3), **ble**(3)

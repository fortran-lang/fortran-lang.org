---
layout: book
title: bge
permalink: /learn/intrinsics/BGE
---
### NAME

**bge**(3f) - \[BIT COMPARE\] Bitwise greater than or equal to
(GFDL)

### DESCRIPTION

Determines whether an integer is a bitwise greater than or equal to
another.

### SYNTAX

result = **bge**(i, j)

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

**bgt**(3), **ble**(3), **blt**(3)

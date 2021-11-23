---
layout: book
title: bgt
permalink: /learn/intrinsics/BGT
---
### NAME

**bgt**(3f) - \[BIT COMPARE\] Bitwise greater than
(GFDL)

### SYNTAX

result = **bgt**(i, j)

### DESCRIPTION

Determines whether an integer is bitwise greater than another.

### ARGUMENTS

  - **I**
    Shall be of INTEGER type or a BOZ literal constant.

  - **J**
    Shall be of INTEGER type, and of the same kind as I; or a BOZ
    literal constant.

### RETURN VALUE

The return value is of type LOGICAL and of the default kind. The result
is true if the sequence of bits represented by I is greater than the
sequence of bits represented by J, otherwise the result is false.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

**bge**(3), **ble**(3), **blt**(3)

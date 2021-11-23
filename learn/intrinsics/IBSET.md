---
layout: book
title: ibset
permalink: /learn/intrinsics/IBSET
---
### NAME

**ibset**(3f) - \[BIT MANIPULATION\] Set bit
(GFDL)

### SYNTAX

result = **ibset**(i, pos)

### DESCRIPTION

IBSET returns the value of I with the bit at position POS set to one.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **POS**
    The type shall be INTEGER. A value of zero refers to the least
    significant bit. pos is an **INTENT**(IN) scalar or array of type
    INTEGER. The value of pos must be within the range zero to
    (BIT\_SIZE(i)**-1**).

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**btest**(3), **ibclr**(3), **ibits**(3), **iand**(3), **ior**(3),
**ieor**(3), **mvbits**(3)

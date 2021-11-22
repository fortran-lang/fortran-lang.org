---
layout: book
title: ibclr
permalink: /learn/intrinsics/f_ibclr
---
### NAME

**ibclr**(3f) - \[BIT MANIPULATION\] Clear bit

### SYNTAX

result = **ibclr**(i, pos)

### DESCRIPTION

IBCLR returns the value of I with the bit at position POS set to zero.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **POS**
    The type shall be INTEGER. A value of zero refers to the least
    significant bit. POS is an **INTENT**(IN) scalar or array of type
    INTEGER. The value of POS must be within the range zero to
    (BIT\_SIZE(i)**-1**).

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**ibits**(3), **ibset**(3), **iand**(3), **ior**(3), **ieor**(3),
**mvbits**(3)

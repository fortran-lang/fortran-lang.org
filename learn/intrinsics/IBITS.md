---
layout: book
title: ibits
permalink: /learn/intrinsics/IBITS
---
### NAME

**ibits**(3f) - \[BIT MANIPULATION\] Bit extraction
(GFDL)

### SYNTAX

result = **ibits**(i, pos, len)

### DESCRIPTION

IBITS extracts a field of length LEN from I, starting from bit position
POS and extending left for LEN bits. The result is right-justified and
the remaining bits are zeroed. The value of pos+len must be less than or
equal to the value **bit\_size**(i).

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **POS**
    The type shall be INTEGER. A value of zero refers to the least
    significant bit.

  - **LEN**
    The type shall be INTEGER.

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**bit\_size**(3), **ibclr**(3), **ibset**(3), **iand**(3), **ior**(3),
**ieor**(3)

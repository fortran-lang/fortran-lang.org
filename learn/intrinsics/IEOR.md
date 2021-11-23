---
layout: book
title: ieor
permalink: /learn/intrinsics/IEOR
---
### NAME

**ieor**(3f) - \[BIT MANIPULATION\] Bitwise logical exclusive or
(GFDL)

### SYNTAX

result = **ieor**(i, j)

### DESCRIPTION

IEOR returns the bitwise Boolean exclusive-OR of I and J.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **J**
    The type shall be INTEGER, of the same kind as I.

### RETURN VALUE

The return type is INTEGER, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**ior**(3), **iand**(3), **ibits**(3), **ibset**(3), **ibclr**(3),
**not**(3)

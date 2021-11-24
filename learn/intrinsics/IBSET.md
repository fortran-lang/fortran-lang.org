---
layout: book
title: ibset
permalink: /learn/intrinsics/IBSET
---
### NAME

__ibset__(3f) - \[BIT MANIPULATION\] Set bit
(GFDL)

### SYNTAX

result = __ibset__(i, pos)

### DESCRIPTION

IBSET returns the value of I with the bit at position POS set to one.

### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __POS__
    The type shall be INTEGER. A value of zero refers to the least
    significant bit. pos is an __INTENT__(IN) scalar or array of type
    INTEGER. The value of pos must be within the range zero to
    (BIT\_SIZE(i)__-1__).

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__btest__(3), __ibclr__(3), __ibits__(3), __iand__(3), __ior__(3),
__ieor__(3), __mvbits__(3)

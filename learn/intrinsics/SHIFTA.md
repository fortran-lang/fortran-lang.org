---
layout: book
title: shifta
permalink: /learn/intrinsics/SHIFTA
---
### NAME

**shifta**(3f) - \[BIT MANIPULATION\] shift bits right with fill
(GFDL)

### SYNTAX

result = **SHIFTA**(I, SHIFT)

### DESCRIPTION

Returns a value corresponding to I with all of the bits shifted right by
SHIFT places. If the absolute value of SHIFT is greater than
**BIT\_SIZE**(I), the value is undefined. Bits shifted out from the
right end are lost. The fill is arithmetic: the bits shifted in from the
left end are equal to the leftmost bit, which in two's complement
representation is the sign bit.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **SHIFT**
    The type shall be INTEGER.

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

**shiftl**(3), **shiftr**(3)

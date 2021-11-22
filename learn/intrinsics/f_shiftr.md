---
layout: book
title: shiftr
permalink: /learn/intrinsics/f_shiftr
---
### NAME

**shiftr**(3f) - \[BIT MANIPULATION\] shift bits right

### SYNTAX

result = **SHIFTR**(I, SHIFT)

### DESCRIPTION

Returns a value corresponding to I with all of the bits shifted right by
SHIFT places. If the absolute value of SHIFT is greater than
**BIT\_SIZE**(I), the value is undefined. Bits shifted out from the
right end are lost, and bits shifted in from the left end are set to 0.

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

**shifta**(3), **shiftl**(3)

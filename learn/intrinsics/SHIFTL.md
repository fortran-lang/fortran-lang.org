---
layout: book
title: shiftl
permalink: /learn/intrinsics/SHIFTL
---
### NAME

__shiftl__(3f) - \[BIT MANIPULATION\] shift bits left
(GFDL)

### SYNTAX

result = __SHIFTL__(I, SHIFT)

### DESCRIPTION

Returns a value corresponding to I with all of the bits shifted left by
SHIFT places. If the absolute value of SHIFT is greater than
__BIT\_SIZE__(I), the value is undefined. Bits shifted out from the left
end are lost, and bits shifted in from the right end are set to 0.

### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __SHIFT__
    The type shall be INTEGER.

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

__shifta__(3), __shiftr__(3)

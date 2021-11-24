---
layout: book
title: shifta
permalink: /learn/intrinsics/SHIFTA
---
### NAME

__shifta__(3f) - \[BIT MANIPULATION\] shift bits right with fill
(GFDL)

### SYNTAX

result = __SHIFTA__(I, SHIFT)

### DESCRIPTION

Returns a value corresponding to I with all of the bits shifted right by
SHIFT places. If the absolute value of SHIFT is greater than
__BIT\_SIZE__(I), the value is undefined. Bits shifted out from the
right end are lost. The fill is arithmetic: the bits shifted in from the
left end are equal to the leftmost bit, which in two's complement
representation is the sign bit.

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

__shiftl__(3), __shiftr__(3)

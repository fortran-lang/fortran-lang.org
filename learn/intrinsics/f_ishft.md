---
layout: book
title: ishft
permalink: /learn/intrinsics/f_ishft
---
### NAME

**ishft**(3f) - \[BIT MANIPULATION\] Shift bits

### SYNTAX

result = **ishft**(i, shift)

### DESCRIPTION

ISHFT returns a value corresponding to I with all of the bits shifted
SHIFT places. A value of SHIFT greater than zero corresponds to a left
shift, a value of zero corresponds to no shift, and a value less than
zero corresponds to a right shift. If the absolute value of SHIFT is
greater than **bit\_size**(i), the value is undefined. Bits shifted out
from the left end or right end are lost; zeros are shifted in from the
opposite end.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **SHIFT**
    The type shall be INTEGER.

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**ishftc**(3)

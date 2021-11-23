---
layout: book
title: ishftc
permalink: /learn/intrinsics/ISHFTC
---
### NAME

**ishftc**(3f) - \[BIT MANIPULATION\] Shift bits circularly
(GFDL)

### SYNTAX

result = **ishftc**(i, shift \[, size\])

### DESCRIPTION

ISHFTC returns a value corresponding to I with the rightmost SIZE bits
shifted circularly SHIFT places; that is, bits shifted out one end are
shifted into the opposite end. A value of SHIFT greater than zero
corresponds to a left shift, a value of zero corresponds to no shift,
and a value less than zero corresponds to a right shift. The absolute
value of SHIFT must be less than SIZE. If the SIZE argument is omitted,
it is taken to be equivalent to **bit\_size**(i).

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **SHIFT**
    The type shall be INTEGER.

  - **SIZE**
    (Optional) The type shall be INTEGER; the value must be greater than
    zero and less than or equal to **bit\_size**(i).

### RETURN VALUE

The return value is of type INTEGER and of the same kind as I.

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**ishft**(3)

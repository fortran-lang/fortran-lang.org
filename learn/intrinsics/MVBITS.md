---
layout: book
title: mvbits
permalink: /learn/intrinsics/MVBITS
---
### NAME

**mvbits**(3f) - \[BIT MANIPULATION\] Move bits from one integer to another
(GFDL)

### SYNTAX

call **mvbits**(from, frompos, len, to, topos)

### DESCRIPTION

Moves LEN bits from positions FROMPOS through frompos+len-1 of FROM to
positions TOPOS through topos+len-1 of TO. The portion of argument TO
not affected by the movement of bits is unchanged. The values of
frompos+len-1 and topos+len-1 must be less than **bit\_size**(from).

### ARGUMENTS

  - **FROM**
    The type shall be INTEGER.

  - **FROMPOS**
    The type shall be INTEGER.

  - **LEN**
    The type shall be INTEGER.

  - **TO**
    The type shall be INTEGER, of the same kind as FROM.

  - **TOPOS**
    The type shall be INTEGER.

### STANDARD

Fortran 95 and later

### CLASS

Elemental subroutine

### SEE ALSO

**ibclr**(3), **ibset**(3), **ibits**(3), **iand**(3), **ior**(3)

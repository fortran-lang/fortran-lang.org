---
layout: book
title: dshiftr
permalink: /learn/intrinsics/DSHIFTR
---
### NAME

__dshiftr__(3f) - \[BIT MANIPULATION\] combines bits of arguments I and J
(GFDL)

### SYNTAX

result = __DSHIFTL__(I, J, SHIFT)

### DESCRIPTION

__DSHIFTR__(I, J, SHIFT) combines bits of I and J. The leftmost SHIFT
bits of the result are the rightmost SHIFT bits of I, and the remaining
bits are the leftmost bits of J.

### ARGUMENTS

  - __I__
    Shall be of type INTEGER.

  - __J__
    Shall be of type INTEGER, and of the same kind as I.

  - __SHIFT__
    Shall be of type INTEGER.

### RETURN VALUE

The return value has same type and kind as I.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

__dshiftl__(3)

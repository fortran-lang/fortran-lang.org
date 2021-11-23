---
layout: book
title: dshiftr
permalink: /learn/intrinsics/DSHIFTR
---
### NAME

**dshiftr**(3f) - \[BIT MANIPULATION\] combines bits of arguments I and J
(GFDL)

### SYNTAX

result = **DSHIFTL**(I, J, SHIFT)

### DESCRIPTION

**DSHIFTR**(I, J, SHIFT) combines bits of I and J. The leftmost SHIFT
bits of the result are the rightmost SHIFT bits of I, and the remaining
bits are the leftmost bits of J.

### ARGUMENTS

  - **I**
    Shall be of type INTEGER.

  - **J**
    Shall be of type INTEGER, and of the same kind as I.

  - **SHIFT**
    Shall be of type INTEGER.

### RETURN VALUE

The return value has same type and kind as I.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental function

### SEE ALSO

**dshiftl**(3)

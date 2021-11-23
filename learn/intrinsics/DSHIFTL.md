---
layout: book
title: dshiftl
permalink: /learn/intrinsics/DSHIFTL
---
### NAME

**dshiftl**(3f) - \[BIT MANIPULATION\] combines bits of arguments I and J
(GFDL)

### SYNTAX

result = **DSHIFTL**(I, J, SHIFT)

### DESCRIPTION

**DSHIFTL**(I, J, SHIFT) combines bits of I and J. The rightmost SHIFT
bits of the result are the leftmost SHIFT bits of J, and the remaining
bits are the rightmost bits of I.

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

**dshiftr**(3)

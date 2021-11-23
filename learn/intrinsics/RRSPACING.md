---
layout: book
title: rrspacing
permalink: /learn/intrinsics/RRSPACING
---
### NAME

**rrspacing**(3f) - \[MODEL\_COMPONENTS\] Reciprocal of the relative spacing
(GFDL)

### SYNTAX

result = **rrspacing**(x)

### DESCRIPTION

**rrspacing**(x) returns the reciprocal of the relative spacing of model
numbers near X.

### ARGUMENTS

  - **X**
    Shall be of type REAL.

### RETURN VALUE

The return value is of the same type and kind as X. The value returned
is equal to **abs**(fraction(x)) \* **float**(radix(x))\*\*digits(x).

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**spacing**(3)

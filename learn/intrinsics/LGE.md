---
layout: book
title: lge
permalink: /learn/intrinsics/LGE
---
### NAME

**lge**(3f) - \[CHARACTER\] Lexical greater than or equal
(GFDL)

### SYNTAX

result = **lge**(string\_a, string\_b)

### DESCRIPTION

Determines whether one string is lexically greater than or equal to
another string, where the two strings are interpreted as containing
ASCII character codes. If the String A and String B are not the same
length, the shorter is compared as if spaces were appended to it to form
a value that has the same length as the longer.

In general, the lexical comparison intrinsics LGE, LGT, LLE, and LLT
differ from the corresponding intrinsic operators .ge., .gt., .le., and
.lt., in that the latter use the processor's character ordering (which
is not ASCII on some targets), whereas the former always use the ASCII
ordering.

### ARGUMENTS

  - **string\_a**
    Shall be of default CHARACTER type.

  - **string\_b**
    Shall be of default CHARACTER type.

### RETURN VALUE

Returns .true. if string\_a \>= string\_b, and .false. otherwise, based
on the ASCII ordering.

### STANDARD

FORTRAN 77 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**\[\[lgt**(3), **\[\[lle**(3), **\[\[llt**(3)

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)

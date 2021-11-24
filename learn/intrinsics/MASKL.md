---
layout: book
title: maskl
permalink: /learn/intrinsics/MASKL
---
### NAME

__maskl__(3f) - \[\] Left justified mask
(GFDL)

### SYNOPSIS

result = __maskl__(i\[, *kind*\])

### DESCRIPTION

__maskl__(i\[, *kind*\]) has its leftmost I bits set to 1, and the
remaining bits set to 0.

### SYNTAX

### ARGUMENTS

  - __I__
    Shall be of type INTEGER.

  - __KIND__
    Shall be a scalar constant expression of type INTEGER.

### RETURN VALUE

The return value is of type INTEGER. If KIND is present, it specifies
the *kind* value of the return type; otherwise, it is of the default
integer *kind*.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__maskr__(3)

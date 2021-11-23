---
layout: book
title: maskl
permalink: /learn/intrinsics/MASKL
---
### NAME

**maskl**(3f) - \[\] Left justified mask
(GFDL)

### SYNOPSIS

result = **maskl**(i\[, *kind*\])

### DESCRIPTION

**maskl**(i\[, *kind*\]) has its leftmost I bits set to 1, and the
remaining bits set to 0.

### SYNTAX

### ARGUMENTS

  - **I**
    Shall be of type INTEGER.

  - **KIND**
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

**maskr**(3)

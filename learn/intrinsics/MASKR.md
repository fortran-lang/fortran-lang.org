---
layout: book
title: maskr
permalink: /learn/intrinsics/MASKR
---
### NAME

**maskr**(3f) - \[\] Right justified mask
(GFDL)

### SYNTAX

result = **maskr**(i\[, kind\])

### DESCRIPTION

**maskr**(i\[, kind\]) has its rightmost I bits set to 1, and the
remaining bits set to 0.

### ARGUMENTS

  - **I**
    Shall be of type INTEGER.

  - **KIND**
    Shall be a scalar constant expression of type INTEGER.

### RETURN VALUE

The return value is of type INTEGER. If KIND is present, it specifies
the kind value of the return type; otherwise, it is of the default
integer kind.

### STANDARD

Fortran 2008 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

**maskl**(3)

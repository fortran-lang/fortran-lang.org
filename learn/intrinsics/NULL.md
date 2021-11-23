---
layout: book
title: null
permalink: /learn/intrinsics/NULL
---
### NAME

**null**(3f) - \[TRANSFORMATIONAL FUNCTION\] Function that returns a disassociated pointer
(GFDL)

### SYNTAX

ptr =\> **null**(\[mold\])

### DESCRIPTION

Returns a disassociated pointer.

If MOLD is present, a disassociated pointer of the same type is
returned, otherwise the type is determined by context.

In \[\[Fortran 95\]\], MOLD is optional. Please note that \[\[Fortran
2003\]\] includes cases where it is required.

### ARGUMENTS

  - **MOLD**
    (Optional) shall be a pointer of any association status and of any
    type.

### RETURN VALUE

A disassociated pointer.

### EXAMPLE

Sample program:

```
    real, pointer, dimension(:) :: vec => null ()
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

**associated**(3)

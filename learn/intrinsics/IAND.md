---
layout: book
title: iand
permalink: /learn/intrinsics/IAND
---
### NAME

__iand__(3f) - \[BIT MANIPULATION\] Bitwise logical and
(GFDL)

### SYNTAX

result = __iand__(i, j)

### DESCRIPTION

Bitwise logical AND.

### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __J__
    The type shall be INTEGER, of the same kind as I.

### RETURN VALUE

The return type is INTEGER, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### EXAMPLE

Sample program:

```
    program demo_iand
    implicit none
      integer :: a, b
      data a / z'f' /, b / z'3' /
      write (*,*) iand(a, b)
    end program demo_iand
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__ior__(3), __ieor__(3), __ibits__(3), __ibset__(3), __ibclr__(3),
__not__(3)

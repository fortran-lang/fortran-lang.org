---
layout: book
title: iand
permalink: /learn/intrinsics/f_iand
---
### NAME

**iand**(3f) - \[BIT MANIPULATION\] Bitwise logical
and

### SYNTAX

result = **iand**(i, j)

### DESCRIPTION

Bitwise logical AND.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **J**
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

Elemental procedure|Elemental function

### SEE ALSO

**ior**(3), **ieor**(3), **ibits**(3), **ibset**(3), **ibclr**(3),
**not**(3)

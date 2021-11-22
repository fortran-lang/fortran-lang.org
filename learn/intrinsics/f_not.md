---
layout: book
title: not
permalink: /learn/intrinsics/f_not
---
### NAME

**not**(3f) - \[BIT MANIPULATION\] Logical negation

### SYNTAX

result = **not**(i)

### DESCRIPTION

NOT returns the bitwise Boolean inverse of I.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

### RETURN VALUE

The return type is INTEGER, of the same kind as the argument.

### EXAMPLE

Sample program

```fortran
    program demo_not
    implicit none
    integer :: i
       i=13741
       write(*,'(b32.32,1x,i0)')i,i
       write(*,'(b32.32,1x,i0)')not(i),not(i)
    end program demo_not
```

Results:

```
   00000000000000000011010110101101 13741
   11111111111111111100101001010010 -13742
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental function

### SEE ALSO

**iand**(3), **ior**(3), **ieor**(3), **ibits**(3), **ibset**(3),
**ibclr**(3)

#### @urbanjost

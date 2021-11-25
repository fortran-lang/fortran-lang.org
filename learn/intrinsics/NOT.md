---
layout: book
title: not
permalink: /learn/intrinsics/NOT
---
### NAME
__not__(3f) - \[BIT MANIPULATION\] Logical negation

### SYNTAX

result = __not__(i)

### DESCRIPTION

NOT returns the bitwise Boolean inverse of I.

### ARGUMENTS

  - __I__
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

__iand__(3), __ior__(3), __ieor__(3), __ibits__(3), __ibset__(3),
__ibclr__(3)

###### fortran-lang intrinsic descriptions

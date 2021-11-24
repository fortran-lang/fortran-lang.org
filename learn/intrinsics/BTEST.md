---
layout: book
title: btest
permalink: /learn/intrinsics/BTEST
---
### NAME

__btest__(3f) - \[BIT MANIPULATION\] Bit test function
(GFDL)

### SYNTAX

result = __btest__(i, pos)

### DESCRIPTION

__btest__(i,pos) returns logical .true. if the bit at POS in I is set.

### ARGUMENTS

  - __I__
    The type shall be INTEGER.

  - __POS__
    The type shall be INTEGER. A value of zero refers to the least
    significant bit.

### RETURN VALUE

The return value is of type LOGICAL

### EXAMPLE

Sample program:

```
    program demo_btest
    implicit none
    integer :: i = 32768 + 1024 + 64
    integer :: pos
    logical :: bool
        do pos=0,16
            bool = btest(i, pos)
            print *, pos, bool
        end do
    end program demo_btest
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

__ibclr__(3), __ibits__(3), __ibset__(3), __iand__(3), __ior__(3),
__ieor__(3), __mvbits__(3)

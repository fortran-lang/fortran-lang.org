---
layout: book
title: btest
permalink: /learn/intrinsics/BTEST
---
### NAME

**btest**(3f) - \[BIT MANIPULATION\] Bit test function
(GFDL)

### SYNTAX

result = **btest**(i, pos)

### DESCRIPTION

**btest**(i,pos) returns logical .true. if the bit at POS in I is set.

### ARGUMENTS

  - **I**
    The type shall be INTEGER.

  - **POS**
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

**ibclr**(3), **ibits**(3), **ibset**(3), **iand**(3), **ior**(3),
**ieor**(3), **mvbits**(3)

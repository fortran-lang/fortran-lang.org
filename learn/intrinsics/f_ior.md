---
layout: book
title: ior
permalink: /learn/intrinsics/f_ior
---
### NAME

**ior**(3f) - \[BIT MANIPULATION\] Bitwise logical
inclusive or

### SYNTAX

result = **ior**(i, j)

```
    integer,intent(in) :: i
    integer,intent(in) :: j
```

### DESCRIPTION

IOR returns the bit-wise Boolean inclusive-OR of I and J.

### ARGUMENTS

  - **I**
    an INTEGER scalar or array.

  - **J**
    INTEGER scalar or array, of the same kind as I.

### RETURN VALUE

The return type is INTEGER, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

### EXAMPLE

Sample program:

```
   program demo_ior
   implicit none
   integer :: i, j, k
      i=53       ! i=00110101 binary (lowest order byte)
      j=45       ! j=00101101 binary (lowest order byte)
      k=ior(i,j) ! k=00111101 binary (lowest order byte) , k=61 decimal
      write(*,'(i8,1x,b8.8)')i,i,j,j,k,k
   end program demo_ior
```

Results:

```
         53 00110101
         45 00101101
         61 00111101
```

### STANDARD

Fortran 95 and later

### CLASS

Elemental procedure|Elemental function

### SEE ALSO

**ieor**(3), **iand**(3), **ibits**(3), **ibset**(3), **ibclr**(3),
**not**(3)

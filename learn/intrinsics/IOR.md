---
layout: book
title: ior
permalink: /learn/intrinsics/IOR
---
#### NAME

__ior__(3f) - \[BIT MANIPULATION\] Bitwise logical inclusive or
(GFDL)

#### SYNTAX

result = __ior__(i, j)

```
    integer,intent(in) :: i
    integer,intent(in) :: j
```

#### DESCRIPTION

IOR returns the bit-wise Boolean inclusive-OR of I and J.

#### ARGUMENTS

  - __I__
    an INTEGER scalar or array.

  - __J__
    INTEGER scalar or array, of the same kind as I.

#### RETURN VALUE

The return type is INTEGER, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

#### EXAMPLE

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

#### STANDARD

Fortran 95 and later

#### CLASS

Elemental procedure\|Elemental function

#### SEE ALSO

__ieor__(3), __iand__(3), __ibits__(3), __ibset__(3), __ibclr__(3),
__not__(3)

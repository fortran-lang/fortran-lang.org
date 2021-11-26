---
layout: book
title: ior
permalink: /learn/intrinsics/IOR
---
## __Name__

__ior__(3) - \[BIT MANIPULATION\] Bitwise logical inclusive or
(GFDL)

## __Syntax__

result = __ior__(i, j)

```
    integer,intent(in) :: i
    integer,intent(in) :: j
```

## __Description__

IOR returns the bit-wise Boolean inclusive-OR of I and J.

## __Arguments__

  - __I__
    an _integer_ scalar or array.

  - __J__
    _integer_ scalar or array, of the same kind as I.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

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

## __Standard__

Fortran 95 and later

## __See Also__

__ieor__(3), __iand__(3), __ibits__(3), __ibset__(3), __ibclr__(3),
__not__(3)

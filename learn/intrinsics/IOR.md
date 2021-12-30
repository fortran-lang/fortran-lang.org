---
layout: book
title: ior
permalink: /learn/intrinsics/IOR
---
## __Name__

__ior__(3) - \[BIT:LOGICAL\] Bitwise logical inclusive or


## __Syntax__
```fortran
   result = ior(i, j)
    integer,intent(in) :: i
    integer,intent(in) :: j
```
## __Description__

__ior__ returns the bit-wise Boolean inclusive-__or__ of __i__ and __j__.

## __Arguments__

  - __i__
    : an _integer_ scalar or array.

  - __j__
    : _integer_ scalar or array, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```fortran
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

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__btest__(3)](BTEST),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

###### fortran-lang intrinsic descriptions

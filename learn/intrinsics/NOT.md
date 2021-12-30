---
layout: book
title: not
permalink: /learn/intrinsics/NOT
---
## __Name__
__not__(3) - \[BIT:LOGICAL\] Logical negation

## __Syntax__
```fortran
result = not(i)
```
## __Description__

NOT returns the bitwise Boolean inverse of I.

## __Arguments__

  - __i__
    : The type shall be _integer_.

## __Returns__

The return type is _integer_, of the same kind as the argument.

## __Examples__

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

## __Standard__

Fortran 95 and later

## __See Also__

[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),

[__ibclr__(3)](IBCLR)

###### fortran-lang intrinsic descriptions (license: MIT))

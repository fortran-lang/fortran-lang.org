---
layout: book
title: iand
permalink: /learn/intrinsics/IAND
---
## __Name__

__iand__(3) - \[BIT MANIPULATION\] Bitwise logical and
(GFDL)

## __Syntax__
```fortran
result = iand(i, j)
```
## __Description__

Bitwise logical __and__.

## __Arguments__

  - __i__
    : The type shall be _integer_.

  - __j__
    : The type shall be _integer_, of the same kind as __i__.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```fortran
program demo_iand
implicit none
integer :: a, b
      data a / z'f' /, b / z'3' /
      write (*,*) iand(a, b)
end program demo_iand
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
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

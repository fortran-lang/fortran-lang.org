---
layout: book
title: iand
permalink: /learn/intrinsics/IAND
---
## __Name__

__iand__(3) - \[BIT MANIPULATION\] Bitwise logical and
(GFDL)

## __Syntax__

result = __iand__(i, j)

## __Description__

Bitwise logical AND.

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __J__
    The type shall be _integer_, of the same kind as I.

## __Returns__

The return type is _integer_, of the same kind as the arguments. (If the
argument kinds differ, it is of the same kind as the larger argument.)

## __Examples__

Sample program:

```
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

__ior__(3), __ieor__(3), __ibits__(3), __ibset__(3), __ibclr__(3),
__not__(3)

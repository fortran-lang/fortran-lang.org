---
layout: book
title: btest
permalink: /learn/intrinsics/BTEST
---
## __Name__

__btest__(3) - \[BIT MANIPULATION\] Bit test function
(GFDL)

## __Syntax__

result = __btest__(i, pos)

## __Description__

__btest__(i,pos) returns logical .true. if the bit at POS in I is set.

## __Arguments__

  - __I__
    The type shall be _integer_.

  - __POS__
    The type shall be _integer_. A value of zero refers to the least
    significant bit.

## __Returns__

The return value is of type _logical_

## __Examples__

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

## __Standard__

Fortran 95 and later

## __See Also__

__ibclr__(3), __ibits__(3), __ibset__(3), __iand__(3), __ior__(3),
__ieor__(3), __mvbits__(3)

---
layout: book
title: btest
permalink: /learn/intrinsics/BTEST
---
## __Name__

__btest__(3) - \[BIT MANIPULATION\] Bit test function
(GFDL)

## __Syntax__

__result = btest(i, pos)__

## __Description__

__btest(i,pos)__ returns logical __.true.__ if the bit at __pos__ in __i__ is set.

## __Arguments__

  - __i__
    The type shall be _integer_.

  - __pos__
    The type shall be _integer_. A value of zero refers to the least
    significant bit.

## __Returns__

The return value is of type _logical_

## __Examples__

Sample program:

```fortran
program demo_btest
implicit none
integer :: i = 32768 + 1024 + 64
integer :: pos
logical :: bool
    do pos=0,16
        bool = btest(i, pos)
        print *, pos, bool
    enddo
end program demo_btest
```

## __Standard__

Fortran 95 and later

## __See Also__

[__ieor__(3)](IEOR), 
[__ibclr__(3)](IBCLR),
[__not__(3)](NOT),
[__ibclr__(3)](IBCLR),
[__ibits__(3)](IBITS),
[__ibset__(3)](IBSET),
[__iand__(3)](IAND),
[__ior__(3)](IOR),
[__ieor__(3)](IEOR),
[__mvbits__(3)](MVBITS)

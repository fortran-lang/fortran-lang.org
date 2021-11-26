---
layout: book
title: repeat
permalink: /learn/intrinsics/REPEAT
---
## __Name__

__repeat__(3) - \[CHARACTER\] Repeated string concatenation
(GFDL)

## __Syntax__

result = __repeat__(string, ncopies)

## __Description__

Concatenates NCOPIES copies of a string.

## __Arguments__

  - __STRING__
    Shall be scalar and of type CHARACTER.

  - __NCOPIES__
    Shall be scalar and of type _integer_.

## __Returns__

A new scalar of type CHARACTER built up from NCOPIES copies of STRING.

## __Examples__

Sample program:

```
    program demo_repeat
    implicit none
      write(*,*) repeat("x", 5)   ! "xxxxx"
    end program demo_repeat
```

## __Standard__

Fortran 95 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)

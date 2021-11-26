---
layout: book
title: trim
permalink: /learn/intrinsics/TRIM
---
## __Name__

__trim__(3) - \[CHARACTER\] Remove trailing blank characters of a string
(GFDL)

## __Syntax__

result = __trim__(string)

## __Description__

Removes trailing blank characters of a string.

## __Arguments__

  - __STRING__
    Shall be a scalar of type CHARACTER.

## __Returns__

A scalar of type CHARACTER which length is that of STRING less the
number of trailing blanks.

## __Examples__

Sample program:

```
    program demo_trim
    implicit none
      character(len=10), parameter :: s = "gfortran  "
      write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks
    end program demo_trim
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

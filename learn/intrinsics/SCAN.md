---
layout: book
title: scan
permalink: /learn/intrinsics/SCAN
---
## __Name__

__scan__(3) - \[CHARACTER\] Scan a string for the presence of a set of characters
(GFDL)

## __Syntax__

result = __scan__(string, set\[, back \[, kind\]\])

## __Description__

Scans a STRING for any of the characters in a SET of characters.

If BACK is either absent or equals FALSE, this function returns the
position of the leftmost character of STRING that is in SET. If BACK
equals TRUE, the rightmost position is returned. If no character of SET
is found in STRING, the result is zero.

## __Arguments__

  - __STRING__
    Shall be of type CHARACTER.

  - __SET__
    Shall be of type CHARACTER.

  - __BACK__
    (Optional) shall be of type _logical_.

  - __KIND__
    (Optional) An _integer_ initialization expression indicating the kind
    parameter of the result.

## __Returns__

The return value is of type _integer_ and of kind KIND. If KIND is absent,
the return value is of default integer kind.

## __Examples__

Sample program:

```
    program demo_scan
    implicit none
      write(*,*) scan("fortran", "ao")          ! 2, found 'o'
      write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
      write(*,*) scan("fortran", "c++")         ! 0, found none
    end program demo_scan
```

## __Standard__

Fortran 95 and later, with KIND argument - Fortran 2003 and later

## __See Also__

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)

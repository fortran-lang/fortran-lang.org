---
layout: book
title: scan
permalink: /learn/intrinsics/SCAN
---
### NAME

__scan__(3f) - \[CHARACTER\] Scan a string for the presence of a set of characters
(GFDL)

### SYNTAX

result = __scan__(string, set\[, back \[, kind\]\])

### DESCRIPTION

Scans a STRING for any of the characters in a SET of characters.

If BACK is either absent or equals FALSE, this function returns the
position of the leftmost character of STRING that is in SET. If BACK
equals TRUE, the rightmost position is returned. If no character of SET
is found in STRING, the result is zero.

### ARGUMENTS

  - __STRING__
    Shall be of type CHARACTER.

  - __SET__
    Shall be of type CHARACTER.

  - __BACK__
    (Optional) shall be of type LOGICAL.

  - __KIND__
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind.

### EXAMPLE

Sample program:

```
    program demo_scan
    implicit none
      write(*,*) scan("fortran", "ao")          ! 2, found 'o'
      write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
      write(*,*) scan("fortran", "c++")         ! 0, found none
    end program demo_scan
```

### STANDARD

Fortran 95 and later, with KIND argument - Fortran 2003 and later

### CLASS

Elemental procedure\|Elemental function

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)

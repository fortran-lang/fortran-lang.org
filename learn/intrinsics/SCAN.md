---
layout: book
title: scan
permalink: /learn/intrinsics/SCAN
---
### NAME

**scan**(3f) - \[CHARACTER\] Scan a string for the presence of a set of characters
(GFDL)

### SYNTAX

result = **scan**(string, set\[, back \[, kind\]\])

### DESCRIPTION

Scans a STRING for any of the characters in a SET of characters.

If BACK is either absent or equals FALSE, this function returns the
position of the leftmost character of STRING that is in SET. If BACK
equals TRUE, the rightmost position is returned. If no character of SET
is found in STRING, the result is zero.

### ARGUMENTS

  - **STRING**
    Shall be of type CHARACTER.

  - **SET**
    Shall be of type CHARACTER.

  - **BACK**
    (Optional) shall be of type LOGICAL.

  - **KIND**
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

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)

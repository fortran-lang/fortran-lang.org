---
layout: book
title: trim
permalink: /learn/intrinsics/TRIM
---
### NAME

__trim__(3f) - \[CHARACTER\] Remove trailing blank characters of a string
(GFDL)

### SYNTAX

result = __trim__(string)

### DESCRIPTION

Removes trailing blank characters of a string.

### ARGUMENTS

  - __STRING__
    Shall be a scalar of type CHARACTER.

### RETURN VALUE

A scalar of type CHARACTER which length is that of STRING less the
number of trailing blanks.

### EXAMPLE

Sample program:

```
    program demo_trim
    implicit none
      character(len=10), parameter :: s = "gfortran  "
      write(*,*) len(s), len(trim(s))  ! "10 8", with/without trailing blanks
    end program demo_trim
```

### STANDARD

Fortran 95 and later

### CLASS

Transformational function

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)

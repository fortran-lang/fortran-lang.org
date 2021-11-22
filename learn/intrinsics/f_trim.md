---
layout: book
title: trim
permalink: /learn/intrinsics/f_trim
---
### NAME

**trim**(3f) - \[CHARACTER\] Remove trailing blank
characters of a string

### SYNTAX

result = **trim**(string)

### DESCRIPTION

Removes trailing blank characters of a string.

### ARGUMENTS

  - **STRING**
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

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)

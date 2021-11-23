---
layout: book
title: index
permalink: /learn/intrinsics/INDEX
---
### NAME

**index**(3f) - \[CHARACTER\] Position of a substring within a string
(GFDL)

### SYNTAX

**index**(STRING, SUBSTRING \[, BACK \[, KIND\]\]) **result**(START)

```
     character(len=*),intent(in) :: STRING
     character(len=*),intent(in) :: SUBSTRING
     logical,intent(in),optional :: BACK
     integer,intent(in),optional :: KIND
     integer(kind=KIND)          :: START
```

### DESCRIPTION

Returns the position of the start of the leftmost or rightmost
occurrence of string SUBSTRING in STRING, counting from one. If
SUBSTRING is not present in STRING, zero is returned.

### ARGUMENTS

  - **STRING**
    string to be searched

  - **SUBSTRING**
    string to attempt to locate in STRING

  - **BACK**
    If the BACK argument is present and true, the return value is the
    start of the rightmost occurrence rather than the leftmost.

  - **KIND**
    An INTEGER initialization expression indicating the kind parameter
    of the result.

### RETURN VALUE

  - **START**
    The return value is of type INTEGER and of kind KIND. If KIND is
    absent, the return value is of default integer kind.

### EXAMPLE

Example program

```
   program demo_index
   implicit none
   character(len=*),parameter :: str=&
   'Search this string for this expression'
   !1234567890123456789012345678901234567890
   write(*,*)&
      index(str,'this').eq.8,              &
      ! return value is counted from the left end even if BACK=.TRUE.
      index(str,'this',back=.true.).eq.24, &
      ! INDEX is case-sensitive
      index(str,'This').eq.0
   end program demo_index
```

Expected Results:

```
   > T T T
```

### STANDARD

FORTRAN 77 and later, with KIND argument Fortran 2003
and later

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)

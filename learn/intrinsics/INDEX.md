---
layout: book
title: index
permalink: /learn/intrinsics/INDEX
---
### NAME

__index__(3f) - \[CHARACTER\] Position of a substring within a string
(GFDL)

### SYNTAX

__index__(STRING, SUBSTRING \[, BACK \[, KIND\]\]) __result__(START)

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

  - __STRING__
    string to be searched

  - __SUBSTRING__
    string to attempt to locate in STRING

  - __BACK__
    If the BACK argument is present and true, the return value is the
    start of the rightmost occurrence rather than the leftmost.

  - __KIND__
    An INTEGER initialization expression indicating the kind parameter
    of the result.

### RETURN VALUE

  - __START__
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

  - __Elemental:__
    __adjustl__(3), __adjustr__(3), __index__(3), __len\_trim__(3),
    __scan__(3), __verify__(3)

  - __Nonelemental:__
    __repeat__(3), __trim__(3)

---
layout: book
title: len
permalink: /learn/intrinsics/LEN
---
### NAME

**len**(3f) - \[CHARACTER\] Length of a character entity
(GFDL)

### SYNTAX

l = **len**(string \[, kind\])

### DESCRIPTION

Returns the length of a character string. If STRING is an array, the
length of an element of STRING is returned. Note that STRING need not be
defined when this intrinsic is invoked, since only the length, not the
content, of STRING is needed.

### ARGUMENTS

  - **STRING**
    Shall be a scalar or array of type CHARACTER, with **intent**(in)

  - **KIND**
    (Optional) An INTEGER initialization expression indicating the kind
    parameter of the result.

### RETURN VALUE

The return value is of type INTEGER and of kind KIND. If KIND is absent,
the return value is of default integer kind.

### STANDARD

FORTRAN 77 and later, with KIND argument - Fortran 2003 and later

### CLASS

Inquiry function

### EXAMPLE

Sample program

```
     program demo_len
     implicit none
     character(len=:),allocatable :: string
        string=' how long is this string?     '
        write(*,*)'LENGTH=',len(string)
        write(*,*)'TRIMMED LENGTH=',len_trim(string)
     end program demo_len
```

### SEE ALSO

Functions that perform operations on character strings, return lengths
of arguments, and search for certain arguments:

  - **Elemental:**
    **adjustl**(3), **adjustr**(3), **index**(3), **len\_trim**(3),
    **scan**(3), **verify**(3)

  - **Nonelemental:**
    **repeat**(3), **trim**(3)
